# Porting cardano-streamer to a cardano-node 11.x dependency set

## Why

The oracle must validate **PV11** blocks. It cannot today:

    ObsoleteNode (Version 11) (Version 10)

Measured twice, on the first PV11 block of preprod (epoch 293):

| build | cardano-api | result |
|---|---|---|
| `10.6.2-dump-snapshot` | 10.23.0.0 | dies at epoch 293, after 3h34m |
| `10.7.1-dump-snapshot` | 10.26.0.0 | dies at epoch 293, after 290 epochs |

So the cap does **not** move between cardano-api 10.23 and 10.26. It is not a
version-bump problem.

**Do not re-derive PV support from a changelog.** `cardano-ledger-conway
1.20.0.0` bumps `ProtVerHigh ConwayEra` to 11 and the 10.6.2 build really does
resolve that version — and still caps at 10, because the CONSENSUS
header-envelope check bounds independently of the ledger package. A recorded
claim in dugite's CLAUDE.md said the port was unnecessary for exactly this
reason and was wrong. Only running a real PV11 block through the binary settles
it.

## Consequences while this stands

* PV11 ledger state is unverifiable against this oracle on **any** network.
* dugite's mainnet tip comparison walls at **~epoch 640** (mainnet crossed PV11
  between 640 and 644), not 649.
* preprod is comparable only to epoch **292**; PV11 begins at 295.

## Starting point

This branch is `10.7.1-dump-snapshot`, which BUILDS. Upstream has nothing newer
to reuse: `lehins/cardano-streamer` has no 11.x branch, and its `master` pins the
same CHaP index-state (2026-02-09) as our 10.6.2.

## Work, in order

1. **Finish the 10.7.1 carry-over first.** `snapshotInfo` still emits `()` for
   `delegations` and `poolParams`. `SnapShot` went from three fields to two, with
   delegations merged into the stake entry — the same change dugite recorded from
   the other side in its #1057. Emitting `()` where the 10.6.2 oracle emits real
   maps would silently EMPTY two compared fields, which is worse than having no
   port at all. This is prerequisite work for any dependency set.

2. **Find the 11.x dependency set.** Raise the CHaP index-state until
   `cardano-api` resolves to the version cardano-node 11.0.1 ships, and check
   what `ouroboros-consensus`/`-cardano` come with it. The cap lives in the
   consensus header-envelope check, so that is the package that has to move.

3. **Expect API drift, and budget for it.** The 10.6.2 → 10.7.1 step alone moved
   `ssStake` between modules, deleted `ssDelegations`/`ssPoolParams`, turned the
   snapshot stake into `ActiveStake` (`sumAllStake` → `sumAllActiveStake`
   returning `NonZero Coin`), and added encoding-version constraints that had to
   become `EraApp` superclasses plus an explicit signature on
   `extractSnapshotData` (an unsignatured where-binding does not inherit the
   caller's constraint). A larger jump will drift more.

4. **Port dugite's dump customisations forward.** They live on
   `dugite/full-era-ledger-dumps`: Byron dumping (with the slot-derived epoch —
   Byron cannot report its own), era/PV-gated protocol parameters, named and
   exact-rational governance thresholds, exact `executionUnitPrices`, and the
   structured Byron `txFeePolicy`.

5. **Validate on a real PV11 block, not a version number.** preprod epoch 293 is
   the cheapest such block; a full preprod replay reaches it in ~3.5 hours.

## Build

    export PATH="$HOME/.ghcup/bin:$PATH"     # ghc-9.6.5, cabal-3.10.3.0
    cabal build cstreamer

Both branches share one `dist-newstyle`, so a build here overwrites the binary
the other branch produced. The validated 10.6.2 oracle is pinned out of harm's
way at `oracle-bin/cstreamer-10.6.2` with its sha256 in `PROVENANCE.txt` — check
it before trusting any run.
