{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Streamer.ProtocolInfo where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Byron as ApiByron
import Cardano.Ledger.BaseTypes (ProtVer (..), SlotNo (..), natVersion)
import Cardano.Ledger.Core (ProtVerHigh)
import Cardano.Streamer.Common
import Cardano.Streamer.Storage
import Control.Monad.Trans.Except
import Criterion.Measurement (initializeTime)
import qualified Ouroboros.Consensus.Byron.Node as Consensus
import qualified Ouroboros.Consensus.Cardano as Consensus
import Ouroboros.Consensus.Cardano.Block
import qualified Ouroboros.Consensus.Cardano.Node as Consensus
import Ouroboros.Consensus.Config (configStorage, emptyCheckpointsMap)
import qualified Ouroboros.Consensus.Node as Node
import qualified Ouroboros.Consensus.Node.InitStorage as Node
import Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.Impl.Args (
  cdbImmDbArgs,
  cdbLgrDbArgs,
  completeChainDbArgs,
  updateTracer,
 )
import Ouroboros.Consensus.Storage.LedgerDB.Args (lgrStartSnapshot)
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots (
  DiskSnapshot (..),
 )
import Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr)
import RIO.Time

newtype NodeConfigError = NodeConfigError {unNodeConfigError :: Text}
  deriving (Show, Eq)

instance Exception NodeConfigError

readNodeConfig :: MonadIO m => FilePath -> m Api.NodeConfig
readNodeConfig =
  liftIO . throwExceptT . withExceptT NodeConfigError . Api.readNodeConfig . Api.File

readCardanoGenesisConfig :: MonadIO m => Api.NodeConfig -> m Api.GenesisConfig
readCardanoGenesisConfig =
  liftIO . throwExceptT . Api.readCardanoGenesisConfig

readProtocolInfoCardano :: MonadIO m => FilePath -> m (ProtocolInfo (CardanoBlock StandardCrypto))
readProtocolInfoCardano configFilePath = do
  nodeConfig <- readNodeConfig configFilePath
  mkProtocolInfoCardanoAtLedgerMaxPV <$> readCardanoGenesisConfig nodeConfig

-- | 'Api.mkProtocolInfoCardano' with the obsolete-node bound raised to the
-- greatest protocol version the LEDGER in this dependency set declares it
-- implements, instead of the constant cardano-api hardcodes.
--
-- cardano-api writes the bound as a literal:
--
-- > , Consensus.cardanoProtocolVersion = ProtVer (natVersion @10) 0
--
-- byte-identical in 10.23.0.0 and 10.26.0.0, and consensus turns that one field
-- into the bound every header is checked against
-- (@Ouroboros.Consensus.Cardano.Node@):
--
-- > -- The major protocol version of the last era is the maximum major protocol
-- > -- version we support.
-- > maxMajorProtVer = MaxMajorProtVer $ pvMajor cardanoProtocolVersion
--
-- so a PV11 header dies with @ObsoleteNode (Version 11) (Version 10)@ however
-- new the packages are. That is why bumping cardano-api 10.23 -> 10.26 did not
-- move it, and why a changelog's @ProtVerHigh@ is not evidence about what a
-- node ACCEPTS.
--
-- The bound is taken from @ProtVerHigh ConwayEra@ rather than written as 11, so
-- it tracks the pinned ledger instead of becoming a second constant to keep in
-- step with it. Conway and not the last era on purpose: Dijkstra has not
-- shipped, and cardano-api's lower literal is exactly how consensus documents
-- marking a trailing era experimental. An oracle that silently accepted an era
-- whose rules it cannot check would report agreement it never established.
--
-- The block-forging half of 'Consensus.protocolInfoCardano' is discarded here:
-- cardano-streamer replays an existing chain and never mints, so the other
-- documented consequence of this field — the protocol version stamped into
-- minted headers — cannot arise.
mkProtocolInfoCardanoAtLedgerMaxPV
  :: Api.GenesisConfig
  -> ProtocolInfo (CardanoBlock StandardCrypto)
mkProtocolInfoCardanoAtLedgerMaxPV (Api.GenesisCardano dnc byronGenesis shelleyGenesisHash transCfg) =
  fst $
    -- @IO pins the monad of the discarded block-forging half, which is
    -- otherwise ambiguous once 'fst' throws it away.
    Consensus.protocolInfoCardano @StandardCrypto @IO
    Consensus.CardanoProtocolParams
      { Consensus.byronProtocolParams =
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = byronGenesis
            , Consensus.byronPbftSignatureThreshold =
                Consensus.PBftSignatureThreshold <$> Api.ncPBftSignatureThreshold dnc
            , Consensus.byronProtocolVersion = Api.ncByronProtocolVersion dnc
            , Consensus.byronSoftwareVersion = ApiByron.softwareVersion
            , Consensus.byronLeaderCredentials = Nothing
            }
      , Consensus.shelleyBasedProtocolParams =
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedInitialNonce = Api.shelleyPraosNonce shelleyGenesisHash
            , Consensus.shelleyBasedLeaderCredentials = []
            }
      , Consensus.cardanoHardForkTriggers = Api.ncHardForkTriggers dnc
      , Consensus.cardanoLedgerTransitionConfig = transCfg
      , Consensus.cardanoCheckpoints = emptyCheckpointsMap
      , Consensus.cardanoProtocolVersion =
          ProtVer (natVersion @(ProtVerHigh ConwayEra)) 0
      }

-- TODO: Move upstream
instance Exception ReadIncrementalErr

-- | Prepare arguments for chain db.
mkDbArgs ::
  ( MonadIO m
  , MonadReader env m
  , HasResourceRegistry env
  , HasLogFunc env
  , Show (Header blk)
  , Node.RunNode blk
  ) =>
  FilePath ->
  Maybe DiskSnapshot ->
  ProtocolInfo blk ->
  m (ChainDB.ChainDbArgs Identity IO blk)
mkDbArgs dbDir diskSnapshot ProtocolInfo{pInfoInitLedger, pInfoConfig} = do
  registry <- view registryL
  dbTracer <- mkTracer (Just "Trace") LevelDebug
  let
    ldbArgs = mkLedgerDbArgs InMemV2
    chainDbArgs =
      updateTracer dbTracer $
        completeChainDbArgs
          registry
          pInfoConfig
          pInfoInitLedger
          chunkInfo
          (const True)
          (Node.stdMkChainDbHasFS dbDir)
          (Node.stdMkChainDbHasFS dbDir)
          ldbArgs
          ChainDB.defaultArgs
    -- Overwrite starting disk snapshot for LedgerDB
    lgrDbArgs =
      (cdbLgrDbArgs chainDbArgs)
        { lgrStartSnapshot = diskSnapshot
        }
  logDebug $ "Preparing to open the database: " <> displayShow dbDir
  pure $ chainDbArgs{cdbLgrDbArgs = lgrDbArgs}
  where
    chunkInfo = Node.nodeImmutableDbChunkInfo (configStorage pInfoConfig)

runDbStreamerApp ::
  RIO (DbStreamerApp (CardanoBlock StandardCrypto)) a -> RIO AppConfig a
runDbStreamerApp action = do
  appConf <- ask
  protocolInfo <- readProtocolInfoCardano (appConfFilePath appConf)
  dbArgs <- mkDbArgs (appConfChainDir appConf) (appConfReadDiskSnapshot appConf) protocolInfo
  let
    iDbArgs = cdbImmDbArgs dbArgs
    startSlotNo = SlotNo . dsNumber <$> appConfReadDiskSnapshot appConf
  liftIO initializeTime
  logInfo "withImmutableDb prepare"
  withImmutableDb iDbArgs startSlotNo $ \iDb startPoint -> do
    logInfo "withImmutableDb start"
    startTime <- getCurrentTime
    writeBlocksRef <-
      newIORef (appConfWriteBlocksSlotNoSet appConf, appConfWriteBlocksBlockHashSet appConf)
    ledgerDb <- openLedgerDb (ChainDB.cdbLgrDbArgs dbArgs)
    refSnapshots <- newIORef (appConfWriteDiskSnapshots appConf)
    let app =
          DbStreamerApp
            { dsAppLogFunc = appConfLogFunc appConf
            , dsAppRegistry = appConfRegistry appConf
            , dsAppProtocolInfo = protocolInfo
            , dsAppChainDir = appConfChainDir appConf
            , dsAppChainDbArgs = dbArgs
            , dsAppIDb = iDb
            , dsAppLedgerDb = ledgerDb
            , dsAppOutDir = Nothing
            , dsAppStartPoint = startPoint
            , dsAppStopSlotNo = SlotNo <$> appConfStopSlotNumber appConf
            , dsAppWriteDiskSnapshots = refSnapshots
            , dsAppWriteBlocks = writeBlocksRef
            , dsAppValidationMode = appConfValidationMode appConf
            , dsAppStartTime = startTime
            , dsAppRTSStatsHandle = Nothing
            }
    result <- runRIO app action
    result <$ logInfo "withImmutableDb end"
