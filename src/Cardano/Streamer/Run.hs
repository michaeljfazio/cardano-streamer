{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Streamer.Run (runApp) where

import Cardano.Chain.Block as Byron (ChainValidationState (..))
import qualified Cardano.Chain.Common as Byron (lovelaceToInteger)
import qualified Cardano.Chain.Common as ByronCommon (
  TxFeePolicy (..),
  TxSizeLinear (..),
  lovelaceToInteger,
  )
import qualified Cardano.Chain.Delegation as Byron (unMap)
import qualified Cardano.Chain.Delegation.Validation.Interface as ByronDI (State, delegationMap)
import qualified Cardano.Chain.Slotting as Byron (
  EpochNumber (getEpochNumber),
  SlotNumber (unSlotNumber),
  )
import qualified Cardano.Chain.UTxO as Byron (UTxO, balance, unUTxO)
-- `Cardano.Chain.Update.ProtocolParameters` is a HIDDEN module; `Cardano.Chain.Update`
-- is the package's public façade for the same type.
import qualified Cardano.Chain.Update as ByronPP (
  ProtocolParameters (..),
  )
import qualified Cardano.Chain.Update.Validation.Interface as ByronUPI (
  State (adoptedProtocolParameters, currentEpoch),
  )
import qualified Data.Bimap as Bimap
import Cardano.Ledger.Address
import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  BoundedRational (..),
  EpochNo (..),
  StrictMaybe (..),
  activeSlotCoeff,
  activeSlotVal,
  epochInfoPure,
  maxLovelaceSupply,
  securityParameter,
  )
import Cardano.Ledger.Slot (EpochSize (..), SlotNo (..), epochInfoSize)
import Cardano.Ledger.Coin (Coin (..), DeltaCoin (..))
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core (
  PParams,
  ppA0L,
  ppDG,
  ppEMaxL,
  ppKeyDepositL,
  ppMaxBBSizeL,
  ppMaxBHSizeL,
  ppMaxTxSizeL,
  ppMinFeeAL,
  ppMinFeeBL,
  ppMinPoolCostL,
  ppNOptL,
  ppPoolDepositL,
  ppProtocolVersionL,
  ppRhoL,
  ppTauL,
  )
import Cardano.Ledger.Alonzo.Scripts (Prices (..))
import Cardano.Ledger.Alonzo.PParams (
  AlonzoEraPParams,
  ppCollateralPercentageL,
  ppCostModelsL,
  ppMaxBlockExUnitsL,
  ppMaxCollateralInputsL,
  ppMaxTxExUnitsL,
  ppMaxValSizeL,
  ppPricesL,
  )
import Cardano.Ledger.Babbage.PParams (
  BabbageEraPParams,
  ppCoinsPerUTxOByteL,
  unCoinPerByte,
  )
import Cardano.Ledger.Conway.PParams (
  ConwayEraPParams,
  DRepVotingThresholds (..),
  PoolVotingThresholds (..),
  ppCommitteeMaxTermLengthL,
  ppCommitteeMinSizeL,
  ppDRepActivityL,
  ppDRepDepositL,
  ppDRepVotingThresholdsL,
  ppGovActionDepositL,
  ppGovActionLifetimeL,
  ppMinFeeRefScriptCostPerByteL,
  ppPoolVotingThresholdsL,
  )
import Data.Aeson.Types (Pair)
import Cardano.Ledger.Conway.Governance (
  ConwayEraGov (committeeGovStateL, constitutionGovStateL),
  finishDRepPulser,
  newEpochStateDRepPulsingStateL,
  rsEnactStateL,
  )
import Cardano.Ledger.Conway.State (ConwayEraCertState, certVStateL, vsCommitteeStateL)
import Cardano.Ledger.Shelley.RewardUpdate (PulsingRewUpdate (..), RewardSnapShot (..), RewardUpdate (..))
import Cardano.Ledger.Shelley.Rewards (sumRewards)
import Cardano.Ledger.Hashes (unKeyHash)
import Cardano.Crypto.Hash.Class (hashToTextAsHex)
import Cardano.Ledger.Shelley.API (SnapShot (..), SnapShots (..))
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.State (
  IndividualPoolStake (..),
  Obligations (..),
  PoolDistr (..),
  chainAccountStateL,
  casTreasuryL,
  casReservesL,
  individualPoolStake,
  individualTotalPoolStake,
  obligationCertState,
  obligationGovState,
  sumAllStake,
  sumObligation,
  unPoolDistr,
  )
import Cardano.Streamer.Benchmark
import Cardano.Streamer.Common
import Cardano.Streamer.Inspection
import Cardano.Streamer.LedgerState
import Cardano.Streamer.Producer
import Cardano.Streamer.ProtocolInfo
import Cardano.Protocol.Crypto (StandardCrypto)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Config (TopLevelConfig)
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import Cardano.Streamer.RTS
import Cardano.Streamer.Rewards
import Cardano.Ledger.Conway.Governance.DRepPulser (psDRepDistr)
import Cardano.Streamer.Storage (ledgerDbTipExtLedgerState)
import Conduit
import Control.ResourceRegistry (withRegistry)
import Criterion.Measurement (initializeTime)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity (runIdentity)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Map.Strict as Map
import Lens.Micro ((^.))
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Shelley.Ledger.Ledger (shelleyLedgerState)
import Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (DiskSnapshot (..))
import RIO.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, listDirectory)
import RIO.File (withBinaryFileDurable)
import RIO.FilePath
import RIO.List as List
import qualified RIO.Set as Set
import qualified RIO.Text as T

replayChain :: RIO App ()
replayChain =
  runConduit $ sourceBlocksWithInspector_ (SlotInspector noInspection) .| sinkNull

replayBenchmarkReport :: RIO App ()
replayBenchmarkReport = do
  report <-
    runConduit $
      sourceBlocksWithInspector (GetPure ()) (SlotInspector benchmarkInspection) .| calcStatsReport
  writeReport "Benchmark" report

replayEpochStats :: RIO App ()
replayEpochStats = do
  epochStats <-
    runConduit $
      sourceBlocksWithInspector (GetPure ()) (SlotInspector epochBlockStatsInspection)
        .| foldMapC toEpochStats
  writeReport "EpochStats" epochStats
  writeNamedCsv "EpochStats" (epochStatsToNamedCsv epochStats)
  logInfo $ "Final summary: \n    " <> display (fold $ unEpochStats epochStats)

-- | Encode a Rational exactly as {"numerator": n, "denominator": d}.
rationalToJson :: Rational -> Aeson.Value
rationalToJson r =
  Aeson.object
    [ "numerator" Aeson..= numerator r
    , "denominator" Aeson..= denominator r
    ]

-- | The governance thresholds, named and as EXACT rationals.
--
-- Two deliberate choices, both about making a comparison meaningful rather than
-- merely present.
--
-- NAMED, not positional: these records are order-sensitive on the wire, and
-- getting that order wrong is a real defect (dugite #951 shifted six of the ten
-- DRep thresholds and appended `constitution` where `treasuryWithdrawal`
-- belongs, silently changing which governance actions pass). Comparing named
-- keys catches a mislabelled field; comparing an array cannot distinguish a
-- wrong order from a wrong value.
--
-- EXACT rationals, not the ledger's own `ToJSON`: that renders a `UnitInterval`
-- as a decimal, so 51/100 becomes @0.51@ — and this module deliberately encodes
-- every other rational parameter as {numerator, denominator} for exactly that
-- reason ("encoded as exact rationals, not Double"). A decimal on one side and a
-- ratio on the other is a definitional mismatch that shows up as a divergence
-- while proving nothing, which is the `eta` and `epochFees` failure again.
-- | Byron's `txFeePolicy` as `minFee = summand + ceiling(size * multiplier)`.
--
-- `TxFeePolicy` has one constructor at every version of the Byron ledger
-- (`TxFeePolicyTxSizeLinear`), so this is total in practice; the catch-all emits
-- null rather than inventing a shape, because a future constructor would be a
-- policy this cannot describe.
txFeePolicyJson :: ByronCommon.TxFeePolicy -> Aeson.Value
txFeePolicyJson (ByronCommon.TxFeePolicyTxSizeLinear (ByronCommon.TxSizeLinear a b)) =
  Aeson.object
    [ "summand" Aeson..= ByronCommon.lovelaceToInteger a
    , "multiplier" Aeson..= rationalToJson (toRational b)
    ]

poolThresholdsJson :: PoolVotingThresholds -> Aeson.Value
poolThresholdsJson t =
  Aeson.object
    [ "motionNoConfidence" Aeson..= u (pvtMotionNoConfidence t)
    , "committeeNormal" Aeson..= u (pvtCommitteeNormal t)
    , "committeeNoConfidence" Aeson..= u (pvtCommitteeNoConfidence t)
    , "hardForkInitiation" Aeson..= u (pvtHardForkInitiation t)
    , "ppSecurityGroup" Aeson..= u (pvtPPSecurityGroup t)
    ]
  where
    u = rationalToJson . unboundRational

-- | The ten DRep thresholds, in the canonical order, each named.
drepThresholdsJson :: DRepVotingThresholds -> Aeson.Value
drepThresholdsJson t =
  Aeson.object
    [ "motionNoConfidence" Aeson..= u (dvtMotionNoConfidence t)
    , "committeeNormal" Aeson..= u (dvtCommitteeNormal t)
    , "committeeNoConfidence" Aeson..= u (dvtCommitteeNoConfidence t)
    , "updateToConstitution" Aeson..= u (dvtUpdateToConstitution t)
    , "hardForkInitiation" Aeson..= u (dvtHardForkInitiation t)
    , "ppNetworkGroup" Aeson..= u (dvtPPNetworkGroup t)
    , "ppEconomicGroup" Aeson..= u (dvtPPEconomicGroup t)
    , "ppTechnicalGroup" Aeson..= u (dvtPPTechnicalGroup t)
    , "ppGovGroup" Aeson..= u (dvtPPGovGroup t)
    , "treasuryWithdrawal" Aeson..= u (dvtTreasuryWithdrawal t)
    ]
  where
    u = rationalToJson . unboundRational

-- | Build the JSON snapshot for a given ledger state.
--
-- Byron is dumped too, in its OWN shape — see 'extractByronSnapshotData'. It
-- used to return Nothing, which made every Byron epoch oracle-silent and so
-- structurally uncomparable: on mainnet that is epochs 1-207, i.e. the 207
-- epochs that sit UNDER everything the Shelley-era comparison verifies.
--
-- Returns Just (fullJson, rupdNext) where rupdNext should be threaded to the
-- next epoch's call as mRupdApplied (it is the reward update that will be
-- applied at that epoch boundary). Byron has no reward update at all, so its
-- rupdNext is Null — which is also the right value to thread into epoch 208,
-- because the Byron->Shelley boundary applies none.
buildSnapshotJson ::
  TopLevelConfig (CardanoBlock StandardCrypto) ->
  Maybe Aeson.Value ->
  -- | The slot-derived epoch, for Byron only. Byron cannot report its own epoch
  -- (see 'extLedgerStateEpochNoForSlot'), so the caller supplies it.
  Maybe EpochNo ->
  ExtLedgerState (CardanoBlock StandardCrypto) mk ->
  Maybe (Aeson.Value, Aeson.Value)
buildSnapshotJson topLevelConfig mRupdApplied mByronEpoch extLedgerState =
  let eraName = show $ extLedgerStateCardanoEra extLedgerState
      mGlobals = globalsFromLedgerConfig (extLedgerStateCardanoEra extLedgerState) extLedgerState topLevelConfig
      mConwayGov = applyConwayNewEpochState extractConwayGovData extLedgerState
      mEpochNonce = extLedgerStateEpochNonce extLedgerState
      -- Era-GATED protocol parameters, emitted only in the eras that have them.
      --
      -- A sibling key rather than extra fields inside `protocolParams`: that
      -- object is already compared across 312 mainnet epochs, and widening a
      -- compared object changes existing paths. This is purely additive.
      --
      -- Nothing (hence an absent key) in eras that lack the group — Shelley,
      -- Allegra and Mary have no cost models, so emitting an empty map there
      -- would manufacture a value for a parameter that does not exist.
      eraPParams =
        Aeson.object $
          concat
            [ maybe [] id (applyAlonzoPParams alonzoPParamsPairs extLedgerState)
            , maybe [] id (applyBabbagePParams babbagePParamsPairs extLedgerState)
            , maybe [] id (applyConwayPParams conwayPParamsPairs extLedgerState)
            ]
   in applyNewEpochState
        (Just . (,Aeson.Null) . extractByronSnapshotData mByronEpoch)
        (\_ -> Just . extractSnapshotData eraName mGlobals mConwayGov mRupdApplied mEpochNonce eraPParams)
        extLedgerState
  where
    -- Introduced by ALONZO: Plutus cost models, execution-unit budgets and
    -- prices, collateral, and the max serialised Value size.
    alonzoPParamsPairs :: AlonzoEraPParams era => PParams era -> [Pair]
    alonzoPParamsPairs pp =
      [ "costModels" Aeson..= (pp ^. ppCostModelsL)
      , -- Exact rationals, for the same reason the governance thresholds are.
        -- The ledger's own ToJSON renders these `NonNegativeInterval`s as
        -- decimals, and mainnet's step price comes out in SCIENTIFIC notation
        -- (`7.21e-05`) — a spelling any other implementation would have to
        -- reproduce character for character to compare equal. `721/10000000` is
        -- the value; `7.21e-05` is one rendering of it.
        "executionUnitPrices"
          Aeson..= Aeson.object
            [ "priceMemory" Aeson..= rationalToJson (unboundRational (prMem (pp ^. ppPricesL)))
            , "priceSteps" Aeson..= rationalToJson (unboundRational (prSteps (pp ^. ppPricesL)))
            ]
      , "maxTxExUnits" Aeson..= (pp ^. ppMaxTxExUnitsL)
      , "maxBlockExUnits" Aeson..= (pp ^. ppMaxBlockExUnitsL)
      , "maxValueSize" Aeson..= (pp ^. ppMaxValSizeL)
      , "collateralPercentage" Aeson..= (pp ^. ppCollateralPercentageL)
      , "maxCollateralInputs" Aeson..= (pp ^. ppMaxCollateralInputsL)
      ]

    -- Introduced by BABBAGE. Alonzo's `coinsPerUTxOWord` is a DIFFERENT
    -- parameter with a different unit, and conflating the two is dugite #919.
    babbagePParamsPairs :: BabbageEraPParams era => PParams era -> [Pair]
    babbagePParamsPairs pp =
      [ "coinsPerUTxOByte" Aeson..= unCoinPerByte (pp ^. ppCoinsPerUTxOByteL)
      ]

    -- Introduced by CONWAY (CIP-1694). The two threshold records are the
    -- reason these are worth comparing: they are order-sensitive on the wire and
    -- a wrong field order silently changes which governance actions pass.
    conwayPParamsPairs :: ConwayEraPParams era => PParams era -> [Pair]
    conwayPParamsPairs pp =
      [ "poolVotingThresholds" Aeson..= poolThresholdsJson (pp ^. ppPoolVotingThresholdsL)
      , "dRepVotingThresholds" Aeson..= drepThresholdsJson (pp ^. ppDRepVotingThresholdsL)
      , "committeeMinSize" Aeson..= (pp ^. ppCommitteeMinSizeL)
      , "committeeMaxTermLength" Aeson..= (pp ^. ppCommitteeMaxTermLengthL)
      , "govActionLifetime" Aeson..= (pp ^. ppGovActionLifetimeL)
      , "govActionDeposit" Aeson..= unCoin (pp ^. ppGovActionDepositL)
      , "dRepDeposit" Aeson..= unCoin (pp ^. ppDRepDepositL)
      , "dRepActivity" Aeson..= (pp ^. ppDRepActivityL)
      , "minFeeRefScriptCostPerByte"
          Aeson..= rationalToJson (unboundRational (pp ^. ppMinFeeRefScriptCostPerByteL))
      ]

    -- | Byron's ledger state, in the shape Byron actually has.
    --
    -- Byron is NOT a cut-down Shelley: there is no treasury, no reserves, no
    -- reward pot, no stake distribution and no pools. Back-projecting the
    -- Shelley shape onto it is exactly what disqualified Koios as an oracle, so
    -- this emits only what 'ChainValidationState' carries and lets the
    -- comparator see the rest as absent rather than as zero.
    --
    -- The load-bearing field is @utxo.balance@ — the circulating supply. It is
    -- what the Shelley translation turns into @reserves@
    -- (@maxLovelaceSupply - circulating@), so every reward calculation in every
    -- later era rests on it. Byron burns fees (no treasury), so it falls
    -- monotonically below genesis @initial_funds@.
    --
    -- Safe to read the UTxO here despite the DiffMK INVARIANT at the call site:
    -- that invariant is about @esUTxOState@ / the ledger TABLES, and Byron has
    -- none — @cvsUtxo@ is an ordinary strict field of 'ChainValidationState',
    -- complete in every state regardless of the mapkind. Verified by the number
    -- rather than by the types: the balance reproduces
    -- @45e15 - reserves(epoch 208)@ to the lovelace, which an incomplete map
    -- could not.
    extractByronSnapshotData :: Maybe EpochNo -> ChainValidationState -> Aeson.Value
    extractByronSnapshotData mEpoch cvs =
      let utxo = cvsUtxo cvs
          upiState = cvsUpdateState cvs
          pparams = ByronUPI.adoptedProtocolParameters upiState
          -- `balance` sums every output and can only fail on Lovelace overflow,
          -- which a valid chain cannot reach. Emit null rather than 0 if it
          -- ever does: a fabricated 0 would read as a real supply collapse.
          mBalance = case Byron.balance utxo of
            Right l -> Just (Byron.lovelaceToInteger l)
            Left _ -> Nothing
          delegs = Bimap.toList . Byron.unMap $ ByronDI.delegationMap (cvsDelegationState cvs)
       in Aeson.object
            [ -- The SLOT-derived epoch, supplied by the caller. Byron's own
              -- `UPI.State.currentEpoch` is kept alongside it rather than used:
              -- on mainnet it reads 0 for the entire era, and publishing that as
              -- `epoch` would label all 207 dumps epoch 0.
              "epoch" Aeson..= fmap unEpochNo mEpoch
            , "byronUpdateEpoch" Aeson..= Byron.getEpochNumber (ByronUPI.currentEpoch upiState)
            , "snapshotEraName" Aeson..= ("Byron" :: String)
            , "lastSlot" Aeson..= Byron.unSlotNumber (cvsLastSlot cvs)
            , "utxo"
                Aeson..= Aeson.object
                  [ "count" Aeson..= Map.size (Byron.unUTxO utxo)
                  , "balance" Aeson..= mBalance
                  ]
            , "byronDelegation"
                Aeson..= Aeson.object
                  [ "count" Aeson..= length delegs
                  ]
            , "byronProtocolParams"
                Aeson..= Aeson.object
                  [ "scriptVersion" Aeson..= ByronPP.ppScriptVersion pparams
                  , "maxBlockSize" Aeson..= ByronPP.ppMaxBlockSize pparams
                  , "maxTxSize" Aeson..= ByronPP.ppMaxTxSize pparams
                  , -- STRUCTURED, not `show`. The Haskell rendering
                    -- "TxFeePolicyTxSizeLinear (TxSizeLinear (Lovelace 155381)
                    -- (21973 % 500))" is a Haskell value printed, and no other
                    -- implementation can reproduce that string — so the field
                    -- could never be compared, only eyeballed. Emitting `a` and
                    -- `b` as a lovelace and an exact rational makes it a real
                    -- comparison, and dugite's genesis-derived values then match
                    -- it exactly.
                    "txFeePolicy" Aeson..= txFeePolicyJson (ByronPP.ppTxFeePolicy pparams)
                  ]
            ]
    extractConwayGovData ::
      (ConwayEraGov era, ConwayEraCertState era) => NewEpochState era -> Aeson.Value
    extractConwayGovData nes =
      let (snap, ratifyState) = finishDRepPulser (nes ^. newEpochStateDRepPulsingStateL)
          drepDistr = Map.map fromCompact (psDRepDistr snap)
          committee = nes ^. newEpochStateGovStateL . committeeGovStateL
          constitution = nes ^. newEpochStateGovStateL . constitutionGovStateL
          committeeState =
            nes ^. nesEpochStateL . esLStateL . lsCertStateL . certVStateL . vsCommitteeStateL
          nextEnactState = ratifyState ^. rsEnactStateL
       in Aeson.object
            [ "drepDistr" Aeson..= drepDistr
            , "committee" Aeson..= committee
            , "constitution" Aeson..= constitution
            , "committeeState" Aeson..= committeeState
            , "nextEnactState" Aeson..= nextEnactState
            ]

    -- Returns (fullJson, rupdData) where rupdData is threaded to the next epoch.
    extractSnapshotData eraName mGlobals mConwayGov mPrevRupd mEpochNonce eraPParams nes =
      let epochNum = case nesEL nes of EpochNo n -> n
          epochState = nesEs nes
          poolDistr = nesPd nes
          treasuryAmt = unCoin $ epochState ^. chainAccountStateL . casTreasuryL
          reservesAmt = unCoin $ epochState ^. chainAccountStateL . casReservesL

          -- Extract all 3 snapshots (mark, set, go) and fees
          SnapShots {ssStakeMark = markSnap, ssStakeSet = setSnap, ssStakeGo = goSnap, ssFee = feeCoin} = epochState ^. esSnapshotsL
          fees = unCoin feeCoin

          -- Extract deposit obligations
          obligations =
            obligationCertState (epochState ^. esLStateL . lsCertStateL)
              <> obligationGovState (nes ^. newEpochStateGovStateL)

          -- The ledger itself does not sum every reward unconditionally: pre-Allegra
          -- (protocol version major <= 2), a credential with multiple rewards in one
          -- epoch is paid only the Set-minimum one (`filterRewards`/`Set.deleteFindMin`
          -- in Cardano.Ledger.Shelley.Rewards), and `completeRupd` computes deltaR2 from
          -- that FILTERED sum. Must thread the same protocol version `completeRupd`
          -- itself would use, or totalDistributed/deltaR2 over-count at pv<=2.
          sumRs pv m = unCoin (sumRewards pv m) :: Integer

          fromPulsing globals rewsnap@RewardSnapShot {..} pulser =
            let Coin rPot' = rewFees <> rewDeltaR1
                (RewardUpdate {rs = forcedRs}, _) =
                  runIdentity $ runReaderT (completeRupd (Pulsing rewsnap pulser)) globals
                totalDistributed = sumRs rewProtocolVersion forcedRs
                deltaR2 = unCoin rewR - totalDistributed
             in Aeson.object
                  [ "deltaR1" Aeson..= unCoin rewDeltaR1
                  , "deltaR2" Aeson..= deltaR2
                  , "deltaT1" Aeson..= unCoin rewDeltaT1
                  , "rPot" Aeson..= rPot'
                  , "rewardPot" Aeson..= unCoin rewR
                  , "totalDistributed" Aeson..= totalDistributed
                  ]

          rupdData =
            case (nesRu nes, mGlobals) of
              (SJust (Complete RewardUpdate {..}), _) ->
                let DeltaCoin deltaT1 = deltaT
                    DeltaCoin deltaRCombined = deltaR
                 in Aeson.object
                      [ "deltaT1" Aeson..= deltaT1
                      , "deltaR" Aeson..= deltaRCombined
                      , "totalDistributed" Aeson..= sumRs (pr ^. ppProtocolVersionL) rs
                      ]
              (_, Nothing) -> Aeson.Null
              (SJust (Pulsing rewsnap pulser), Just globals) ->
                fromPulsing globals rewsnap pulser
              (SNothing, Just globals) ->
                let pulsing =
                      startStep
                        (epochInfoSize (epochInfoPure globals) (nesEL nes))
                        (nesBprev nes)
                        (nesEs nes)
                        (Coin $ fromIntegral $ maxLovelaceSupply globals)
                        (activeSlotCoeff globals)
                        (securityParameter globals)
                 in case pulsing of
                      Pulsing rewsnap pulser -> fromPulsing globals rewsnap pulser
                      Complete RewardUpdate {..} ->
                        Aeson.object ["totalDistributed" Aeson..= sumRs (pr ^. ppProtocolVersionL) rs]

          -- Eta: performance multiplier. Computed the same way the ledger does
          -- in startStep: if d >= 0.8 then 1, otherwise blocksMade/expectedBlocks.
          -- (RewardSnapShot does not store eta, so we derive it from nesBprev.)
          mEta = mGlobals <&> \globals ->
            let d = unboundRational (pr ^. ppDG)
                EpochSize slots = epochInfoSize (epochInfoPure globals) (nesEL nes)
                n = floor $ (1 - d) * unboundRational (activeSlotVal (activeSlotCoeff globals)) * fromIntegral slots :: Integer
                BlocksMade bm = nesBprev nes
                blocksMade = fromIntegral $ Map.foldl' (+) 0 bm :: Integer
             in if d >= 0.8 || n == 0
                  then 1 :: Rational
                  else blocksMade % n

          -- Protocol parameters: encoded as exact rationals, not Double
          pr = epochState ^. prevPParamsEpochStateL
          protoParams =
            Aeson.object
              [ "rho" Aeson..= rationalToJson (unboundRational (pr ^. ppRhoL))
              , "tau" Aeson..= rationalToJson (unboundRational (pr ^. ppTauL))
              , "d" Aeson..= rationalToJson (unboundRational (pr ^. ppDG))
              , "a0" Aeson..= rationalToJson (unboundRational (pr ^. ppA0L))
              , "nOpt" Aeson..= (pr ^. ppNOptL)
              , "minPoolCost" Aeson..= unCoin (pr ^. ppMinPoolCostL)
              , "protocolVersion" Aeson..= (pr ^. ppProtocolVersionL)
              ]

          -- Era-COMMON parameters (every era from Shelley on). Separate from
          -- `protoParams` for the same reason `eraPParams` is: that object is
          -- already compared across 312 mainnet epochs, so this is additive.
          --
          -- These carry real defect history — the fee parameters and deposits
          -- decide tx validity, and `eMax` decides pool retirement.
          commonPParams =
            Aeson.object
              [ "minFeeA" Aeson..= unCoin (pr ^. ppMinFeeAL)
              , "minFeeB" Aeson..= unCoin (pr ^. ppMinFeeBL)
              , "maxBlockBodySize" Aeson..= (pr ^. ppMaxBBSizeL)
              , "maxTxSize" Aeson..= (pr ^. ppMaxTxSizeL)
              , "maxBlockHeaderSize" Aeson..= (pr ^. ppMaxBHSizeL)
              , "keyDeposit" Aeson..= unCoin (pr ^. ppKeyDepositL)
              , "poolDeposit" Aeson..= unCoin (pr ^. ppPoolDepositL)
              , "eMax" Aeson..= (pr ^. ppEMaxL)
              ]

          -- Circulation and active stake
          totalStake = case mGlobals of
            Nothing -> Nothing
            Just globals ->
              Just $ fromIntegral (maxLovelaceSupply globals) - reservesAmt :: Maybe Integer
          activeStake = unCoin $ sumAllStake (ssStake goSnap)

          -- Pending MIR transfers (Shelley–Babbage; always empty in Conway+)
          instantaneousRewards =
            epochState ^. esLStateL . lsCertStateL . certDStateL . dsIRewardsL

          -- expectedBlocks is derived from genesis/pparams; eta comes from the pulser
          mExpectedBlocks = mGlobals <&> \globals ->
            let d = unboundRational (pr ^. ppDG)
                EpochSize slots = epochInfoSize (epochInfoPure globals) (nesEL nes)
                asc = activeSlotCoeff globals
             in floor $ (1 - d) * unboundRational (activeSlotVal asc) * fromIntegral slots :: Integer

          -- Pool distribution: stake as exact rational + exact lovelace count.
          -- The fractions in PoolDistr sum to exactly 1 by ledger construction,
          -- so stakePercent is simply stakeRational * 100.
          poolMap = unPoolDistr poolDistr
          poolCount = Map.size poolMap
          poolEntries = map mkPoolEntry (Map.toList poolMap)
            where
              mkPoolEntry (pid, poolData) =
                let stakeRational = individualPoolStake poolData
                    stakeLovelace = unCoin $ fromCompact (individualTotalPoolStake poolData)
                    stakePercent = fromRational (stakeRational * 100) :: Double
                 in Aeson.object
                      [ "poolId" Aeson..= hashToTextAsHex (unKeyHash pid)
                      , "stake" Aeson..= rationalToJson stakeRational
                      , "stakeLovelace" Aeson..= (stakeLovelace :: Integer)
                      , "stakePercent" Aeson..= stakePercent
                      ]

          snapshotInfo name mBlocks snap =
            Aeson.object $
              [ "name" Aeson..= (name :: String)
              , "stake" Aeson..= ssStake snap
              , "delegations" Aeson..= ssDelegations snap
              , "poolParams" Aeson..= ssPoolParams snap
              ]
                ++ ["blocks" Aeson..= b | Just b <- [mBlocks]]

          json =
            Aeson.object
              [ "epoch" Aeson..= (fromIntegral epochNum :: Integer)
              , "snapshotEraName" Aeson..= eraName
              , "epochNonce" Aeson..= mEpochNonce
              , "protocolParams" Aeson..= protoParams
              , "commonProtocolParams" Aeson..= commonPParams
              , "eraProtocolParams" Aeson..= eraPParams
              , "totalStake" Aeson..= totalStake
              , "activeStake" Aeson..= activeStake
              , "eta" Aeson..= fmap rationalToJson mEta
              , "expectedBlocks" Aeson..= mExpectedBlocks
              , "rupdNext" Aeson..= rupdData
              , "rupdApplied" Aeson..= mPrevRupd
              , "treasury" Aeson..= treasuryAmt
              , "reserves" Aeson..= reservesAmt
              , "totalPools" Aeson..= poolCount
              , "poolDistribution" Aeson..= poolEntries
              , "epochFees" Aeson..= fees
              , "deposits"
                  Aeson..= Aeson.object
                    [ "stakeKey" Aeson..= unCoin (oblStake obligations)
                    , "pool" Aeson..= unCoin (oblPool obligations)
                    , "dRep" Aeson..= unCoin (oblDRep obligations)
                    , "proposal" Aeson..= unCoin (oblProposal obligations)
                    , "total" Aeson..= unCoin (sumObligation obligations)
                    ]
              , "instantaneousRewards" Aeson..= instantaneousRewards
              , "conwayGov" Aeson..= mConwayGov
              , "snapshots"
                  Aeson..= Aeson.object
                    [ "mark" Aeson..= snapshotInfo "mark" (Just (nesBcur nes)) markSnap
                    , "set" Aeson..= snapshotInfo "set" (Nothing :: Maybe BlocksMade) setSnap
                    , "go" Aeson..= snapshotInfo "go" (Just (nesBprev nes)) goSnap
                    ]
              ]
       in (json, rupdData)

dumpLedgerSnapshot :: RIO App ()
dumpLedgerSnapshot = do
  extLedgerState <- ledgerDbTipExtLedgerState
  app <- ask
  -- Nothing for the Byron epoch: this one-shot dump has no slot context to
  -- derive it from, so a Byron dump here reports `epoch: null` rather than a
  -- guess. `dump-epoch-snapshots` is the path that supplies it.
  case buildSnapshotJson (pInfoConfig (dsAppProtocolInfo app)) Nothing Nothing extLedgerState of
    Just (snapshotData, _) -> liftIO $ BSL.putStrLn $ Aeson.encode snapshotData
    Nothing -> logError "Cannot build snapshot for this ledger state"

dumpEpochSnapshots :: RIO App ()
dumpEpochSnapshots = do
  app <- ask
  outDir <-
    maybe (throwString "--out-dir is required for dump-epoch-snapshots") pure
      =<< asks dsAppOutDir
  prevRupdRef <- newIORef Nothing
  -- Byron's epoch has to be tracked HERE rather than asked of the ledger.
  -- `isFirstSlotOfNewEpoch` compares epoch numbers, and Byron reports
  -- `EpochNo 0` for all 207 of its epochs (see `extLedgerStateEpochNoForSlot`),
  -- so that predicate is false at every Byron block and the whole era emitted
  -- nothing. This holds the last Byron epoch actually dumped, so the boundary
  -- is detected from the SLOT.
  lastByronEpochRef <- newIORef Nothing
  let topLevelConfig = pInfoConfig (dsAppProtocolInfo app)
      snapshotInspection =
        noInspection
          { siFinal = \swb _ _ _ _ _ -> do
              -- Byron: fire when the slot-derived epoch advances. Non-Byron is
              -- left to the validated `isFirstSlotOfNewEpoch` untouched — at
              -- the Byron->Shelley seam it still fires, because Byron's 0 is
              -- below Shelley's 208.
              let mByronEpoch
                    | swbCardanoEra swb /= Byron = Nothing
                    | otherwise =
                        extLedgerStateEpochNoForSlot
                          topLevelConfig
                          (swbPrevExtLedgerState swb)
                          (swbSlotNo swb)
              isNewByronEpoch <- case mByronEpoch of
                Nothing -> pure False
                Just (EpochNo cur) -> do
                  lastDumped <- readIORef lastByronEpochRef
                  pure $ maybe True (cur >) lastDumped
              when (isFirstSlotOfNewEpoch swb || isNewByronEpoch) $ do
                prevRupd <- readIORef prevRupdRef
                -- We use the post-block state (DiffMK) rather than the bare
                -- epoch-boundary state. This is safe because buildSnapshotJson reads
                -- exclusively from NewEpochState fields (epoch number, treasury,
                -- reserves, snapshots, pool distribution, protocol params, reward
                -- update) — none of which are part of the UTxO table, so the
                -- unapplied DiffMK diffs are irrelevant.
                -- INVARIANT: do not add snapshot fields that read from the UTxO
                -- (esUTxOState / utxosUtxo) without switching to a ValuesMK state.
                let mResult =
                      buildSnapshotJson topLevelConfig prevRupd mByronEpoch (swbNewExtLedgerState swb)
                forM_ mResult $ \(json, rupdNext) -> do
                  writeIORef prevRupdRef (Just rupdNext)
                  -- The epoch that NAMES the file must be the same one the JSON
                  -- reports. In Byron `swbEpochNo` is 0 at every block, so using
                  -- it would label all 207 dumps "epoch 0" — distinct filenames
                  -- only because the slot differs, and a comparator keying on
                  -- epoch would pair every one of them against epoch 0.
                  let epochNo = fromMaybe (swbEpochNo swb) mByronEpoch
                  forM_ mByronEpoch $ \(EpochNo e) ->
                    writeIORef lastByronEpochRef (Just e)
                  let epochStr = show (unEpochNo epochNo)
                      slotStr = show (unSlotNo (swbSlotNo swb))
                      fp = outDir </> epochStr <> "-" <> slotStr <> ".json"
                  liftIO $ BSL.writeFile fp (Aeson.encode json)
                  logInfo $
                    "Dumped epoch "
                      <> display epochNo
                      <> " snapshot to: "
                      <> display (T.pack fp)
          }
  runConduit $ sourceBlocksWithInspector_ (SlotInspector snapshotInspection) .| sinkNull

replayRewards :: NE.NonEmpty RewardAccount -> RIO App ()
replayRewards accounts = do
  mOutDir <- dsAppOutDir <$> ask
  case mOutDir of
    Nothing ->
      logError "Output directory is required for exporting rewards"
    Just outDir -> do
      let filePaths =
            [ ( raCredential account,
                outDir </> T.unpack (formatRewardAccount account) <.> "csv"
              )
              | account <- NE.toList accounts
            ]
          withRewardsFiles action = go [] filePaths
            where
              go hdls [] = action $ Map.fromList hdls
              go hdls ((ra, fp) : fps) =
                withBinaryFileDurable fp WriteMode $ \hdl -> do
                  logInfo $ "Opened file for exporting rewards " <> displayShow fp
                  go ((ra, hdl) : hdls) fps
          transformAndFilterRewards handles rd = do
            let filteredRewardsWithHandles = Map.intersectionWith (,) handles (rdRewards rd)
            guard (not (Map.null filteredRewardsWithHandles))
            Just $ map (\(h, c) -> (h, rd {rdRewards = c})) $ Map.elems filteredRewardsWithHandles
      withRewardsFiles $ \rewardHandles -> do
        writeRewardsHeaders rewardHandles
        runConduit $
          sourceBlocksWithInspector (GetPure ()) (SlotInspector rewardsInspection)
            .| concatMapC (>>= transformAndFilterRewards rewardHandles)
            .| mapM_C (mapM_ (uncurry writeRewardDistribution))

runApp :: Opts -> IO ()
runApp Opts {..} = do
  -- Consensus code will initialize the chain directory if it doesn't exist, which makes
  -- no sense for a tool that is suppose to be read only.
  unlessM (doesDirectoryExist oChainDir) $ do
    throwString $ "Chain directory does not exist: " <> oChainDir
  whenM (null <$> listDirectory oChainDir) $ do
    throwString $ "Chain directory is empty: " <> oChainDir
  logOpts <- logOptionsHandle stderr oVerbose
  withLogFunc (setLogMinLevel oLogLevel $ setLogUseLoc oDebug logOpts) $ \logFunc -> do
    withRegistry $ \registry -> do
      let (writeBlockSlots, writeBlockHashes) =
            bimap Set.fromList Set.fromList $
              partitionEithers $
                map unBlockHashOrSlotNo oWriteBlocks
          appConf =
            AppConfig
              { appConfChainDir = oChainDir,
                appConfFilePath = oConfigFilePath,
                appConfReadDiskSnapshot =
                  DiskSnapshot <$> oReadSnapShotSlotNumber <*> pure oSnapShotSuffix,
                appConfWriteDiskSnapshots =
                  DiskSnapshot <$> oWriteSnapShotSlotNumbers <*> pure oSnapShotSuffix,
                appConfStopSlotNumber = oStopSlotNumber,
                appConfValidationMode = oValidationMode,
                appConfWriteBlocksSlotNoSet = writeBlockSlots,
                appConfWriteBlocksBlockHashSet = writeBlockHashes,
                appConfLogFunc = logFunc,
                appConfRegistry = registry
              }
      initializeTime
      void $ runRIO appConf $ runDbStreamerApp $ do
        forM_ oOutDir (createMissingDirectory "output")
        rtsStatsFilePathMaybe <-
          forM oRTSStatsFilePath $ \rtsStatsFilePath -> do
            -- We need to fail when stats are not enabled before we start creating files
            checkRTSStatsEnabled
            fullPath <-
              if isAbsolute rtsStatsFilePath
                then
                  pure rtsStatsFilePath
                else case oOutDir of
                  Nothing ->
                    throwString $
                      "Supplied a relative path for RTS stats: '"
                        <> rtsStatsFilePath
                        <> "' without supplying the OUT_DIR"
                  Just outDir -> pure $ outDir </> rtsStatsFilePath
            createMissingDirectory "RTS Stats File" (dropFileName fullPath)
            whenM (doesPathExist fullPath) $
              throwString $
                "Can't use an existing file for writing RTS stats: " <> fullPath
            let ext = toLower <$> takeExtension fullPath
            unless (ext == ".csv") $
              logWarn $
                "Expected a file path for a CSV file, but "
                  <> displayShow fullPath
                  <> " has an unexpected extension: "
                  <> displayShow ext
            pure fullPath
        unless (null writeBlockSlots) $
          logInfo $
            "Will try to dump blocks if they exist at slots: "
              <> mconcat (intersperse "," (map display (Set.toList writeBlockSlots)))
        unless (null writeBlockHashes) $
          logInfo $
            "Will try to dump blocks with hashes if they exist: "
              <> mconcat
                (intersperse "," (map display (Set.toList writeBlockHashes)))
        app <- ask
        withMaybeFile rtsStatsFilePathMaybe $ \rtsStatsHandle ->
          runRIO (app {dsAppOutDir = oOutDir, dsAppRTSStatsHandle = rtsStatsHandle}) $ do
            logInfo $ "Starting to " <> display oCommand
            writeStreamerHeader
            case oCommand of
              Replay -> replayChain
              Benchmark -> replayBenchmarkReport
              Stats -> replayEpochStats
              ComputeRewards accountIds -> replayRewards accountIds
              DumpSnapshot -> dumpLedgerSnapshot
              DumpEpochSnapshots -> dumpEpochSnapshots
  where
    withMaybeFile mFilePath action =
      case mFilePath of
        Nothing -> action Nothing
        Just fp -> withBinaryFileDurable fp WriteMode (action . Just)
    createMissingDirectory name dir =
      unlessM (doesDirectoryExist dir) $ do
        logInfo $ "Creating " <> name <> " directory: " <> displayShow dir
        createDirectoryIfMissing True dir

-- -- TxOuts:
-- total <- runRIO (app{dsAppOutDir = mOutDir}) $ countTxOuts initLedger
-- logInfo $ "Total TxOuts: " <> displayShow total
-- runRIO (app{dsAppOutDir = mOutDir}) $ revalidateWriteNewEpochState initLedger
