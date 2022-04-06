{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-consensus"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Consensus layer for the Ouroboros blockchain protocol";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filelock" or (errorHandler.buildDepError "filelock"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streaming" or (errorHandler.buildDepError "streaming"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."unix-bytestring" or (errorHandler.buildDepError "unix-bytestring"))
            ]);
        buildable = true;
        modules = [
          "Ouroboros/Consensus/Block"
          "Ouroboros/Consensus/Block/Abstract"
          "Ouroboros/Consensus/Block/EBB"
          "Ouroboros/Consensus/Block/Forging"
          "Ouroboros/Consensus/Block/NestedContent"
          "Ouroboros/Consensus/Block/RealPoint"
          "Ouroboros/Consensus/Block/SupportsMetrics"
          "Ouroboros/Consensus/Block/SupportsProtocol"
          "Ouroboros/Consensus/BlockchainTime"
          "Ouroboros/Consensus/BlockchainTime/API"
          "Ouroboros/Consensus/BlockchainTime/WallClock/Default"
          "Ouroboros/Consensus/BlockchainTime/WallClock/HardFork"
          "Ouroboros/Consensus/BlockchainTime/WallClock/Simple"
          "Ouroboros/Consensus/BlockchainTime/WallClock/Types"
          "Ouroboros/Consensus/BlockchainTime/WallClock/Util"
          "Ouroboros/Consensus/Config"
          "Ouroboros/Consensus/Config/SecurityParam"
          "Ouroboros/Consensus/Config/SupportsNode"
          "Ouroboros/Consensus/Forecast"
          "Ouroboros/Consensus/Fragment/Diff"
          "Ouroboros/Consensus/Fragment/InFuture"
          "Ouroboros/Consensus/Fragment/Validated"
          "Ouroboros/Consensus/Fragment/ValidatedDiff"
          "Ouroboros/Consensus/HardFork/Abstract"
          "Ouroboros/Consensus/HardFork/Combinator"
          "Ouroboros/Consensus/HardFork/Combinator/Abstract"
          "Ouroboros/Consensus/HardFork/Combinator/Abstract/CanHardFork"
          "Ouroboros/Consensus/HardFork/Combinator/Abstract/NoHardForks"
          "Ouroboros/Consensus/HardFork/Combinator/Abstract/SingleEraBlock"
          "Ouroboros/Consensus/HardFork/Combinator/AcrossEras"
          "Ouroboros/Consensus/HardFork/Combinator/Basics"
          "Ouroboros/Consensus/HardFork/Combinator/Block"
          "Ouroboros/Consensus/HardFork/Combinator/Compat"
          "Ouroboros/Consensus/HardFork/Combinator/Condense"
          "Ouroboros/Consensus/HardFork/Combinator/Degenerate"
          "Ouroboros/Consensus/HardFork/Combinator/Embed/Binary"
          "Ouroboros/Consensus/HardFork/Combinator/Embed/Nary"
          "Ouroboros/Consensus/HardFork/Combinator/Embed/Unary"
          "Ouroboros/Consensus/HardFork/Combinator/Forging"
          "Ouroboros/Consensus/HardFork/Combinator/Info"
          "Ouroboros/Consensus/HardFork/Combinator/InjectTxs"
          "Ouroboros/Consensus/HardFork/Combinator/Ledger"
          "Ouroboros/Consensus/HardFork/Combinator/Ledger/CommonProtocolParams"
          "Ouroboros/Consensus/HardFork/Combinator/Ledger/PeerSelection"
          "Ouroboros/Consensus/HardFork/Combinator/Ledger/Query"
          "Ouroboros/Consensus/HardFork/Combinator/Mempool"
          "Ouroboros/Consensus/HardFork/Combinator/Node"
          "Ouroboros/Consensus/HardFork/Combinator/Node/InitStorage"
          "Ouroboros/Consensus/HardFork/Combinator/Node/Metrics"
          "Ouroboros/Consensus/HardFork/Combinator/PartialConfig"
          "Ouroboros/Consensus/HardFork/Combinator/Protocol"
          "Ouroboros/Consensus/HardFork/Combinator/Protocol/ChainSel"
          "Ouroboros/Consensus/HardFork/Combinator/Protocol/LedgerView"
          "Ouroboros/Consensus/HardFork/Combinator/Serialisation"
          "Ouroboros/Consensus/HardFork/Combinator/Serialisation/Common"
          "Ouroboros/Consensus/HardFork/Combinator/Serialisation/SerialiseDisk"
          "Ouroboros/Consensus/HardFork/Combinator/Serialisation/SerialiseNodeToClient"
          "Ouroboros/Consensus/HardFork/Combinator/Serialisation/SerialiseNodeToNode"
          "Ouroboros/Consensus/HardFork/Combinator/State"
          "Ouroboros/Consensus/HardFork/Combinator/State/Infra"
          "Ouroboros/Consensus/HardFork/Combinator/State/Instances"
          "Ouroboros/Consensus/HardFork/Combinator/State/Lift"
          "Ouroboros/Consensus/HardFork/Combinator/State/Types"
          "Ouroboros/Consensus/HardFork/Combinator/Translation"
          "Ouroboros/Consensus/HardFork/Combinator/Util/DerivingVia"
          "Ouroboros/Consensus/HardFork/Combinator/Util/Functors"
          "Ouroboros/Consensus/HardFork/Combinator/Util/InPairs"
          "Ouroboros/Consensus/HardFork/Combinator/Util/Match"
          "Ouroboros/Consensus/HardFork/Combinator/Util/Tails"
          "Ouroboros/Consensus/HardFork/Combinator/Util/Telescope"
          "Ouroboros/Consensus/HardFork/History"
          "Ouroboros/Consensus/HardFork/History/Caching"
          "Ouroboros/Consensus/HardFork/History/EpochInfo"
          "Ouroboros/Consensus/HardFork/History/EraParams"
          "Ouroboros/Consensus/HardFork/History/Qry"
          "Ouroboros/Consensus/HardFork/History/Summary"
          "Ouroboros/Consensus/HardFork/History/Util"
          "Ouroboros/Consensus/HardFork/Simple"
          "Ouroboros/Consensus/HeaderStateHistory"
          "Ouroboros/Consensus/HeaderValidation"
          "Ouroboros/Consensus/Ledger/Abstract"
          "Ouroboros/Consensus/Ledger/Basics"
          "Ouroboros/Consensus/Ledger/CommonProtocolParams"
          "Ouroboros/Consensus/Ledger/Dual"
          "Ouroboros/Consensus/Ledger/Extended"
          "Ouroboros/Consensus/Ledger/Inspect"
          "Ouroboros/Consensus/Ledger/Query"
          "Ouroboros/Consensus/Ledger/Query/Version"
          "Ouroboros/Consensus/Ledger/SupportsMempool"
          "Ouroboros/Consensus/Ledger/SupportsPeerSelection"
          "Ouroboros/Consensus/Ledger/SupportsProtocol"
          "Ouroboros/Consensus/Mempool"
          "Ouroboros/Consensus/Mempool/API"
          "Ouroboros/Consensus/Mempool/Impl"
          "Ouroboros/Consensus/Mempool/Impl/Pure"
          "Ouroboros/Consensus/Mempool/Impl/Types"
          "Ouroboros/Consensus/Mempool/TxLimits"
          "Ouroboros/Consensus/Mempool/TxSeq"
          "Ouroboros/Consensus/MiniProtocol/BlockFetch/Server"
          "Ouroboros/Consensus/MiniProtocol/ChainSync/Client"
          "Ouroboros/Consensus/MiniProtocol/ChainSync/Server"
          "Ouroboros/Consensus/MiniProtocol/LocalStateQuery/Server"
          "Ouroboros/Consensus/MiniProtocol/LocalTxSubmission/Server"
          "Ouroboros/Consensus/MiniProtocol/LocalTxMonitor/Server"
          "Ouroboros/Consensus/Network/NodeToClient"
          "Ouroboros/Consensus/Network/NodeToNode"
          "Ouroboros/Consensus/Node"
          "Ouroboros/Consensus/Node/DbLock"
          "Ouroboros/Consensus/Node/DbMarker"
          "Ouroboros/Consensus/Node/ErrorPolicy"
          "Ouroboros/Consensus/Node/RethrowPolicy"
          "Ouroboros/Consensus/Node/Exit"
          "Ouroboros/Consensus/NodeId"
          "Ouroboros/Consensus/NodeKernel"
          "Ouroboros/Consensus/Node/InitStorage"
          "Ouroboros/Consensus/Node/NetworkProtocolVersion"
          "Ouroboros/Consensus/Node/ProtocolInfo"
          "Ouroboros/Consensus/Node/Recovery"
          "Ouroboros/Consensus/Node/Run"
          "Ouroboros/Consensus/Node/Serialisation"
          "Ouroboros/Consensus/Node/Tracers"
          "Ouroboros/Consensus/Protocol/Abstract"
          "Ouroboros/Consensus/Protocol/BFT"
          "Ouroboros/Consensus/Protocol/LeaderSchedule"
          "Ouroboros/Consensus/Protocol/MockChainSel"
          "Ouroboros/Consensus/Protocol/ModChainSel"
          "Ouroboros/Consensus/Protocol/PBFT"
          "Ouroboros/Consensus/Protocol/PBFT/Crypto"
          "Ouroboros/Consensus/Protocol/PBFT/State"
          "Ouroboros/Consensus/Protocol/Signed"
          "Ouroboros/Consensus/Ticked"
          "Ouroboros/Consensus/TypeFamilyWrappers"
          "Ouroboros/Consensus/Util"
          "Ouroboros/Consensus/Util/AnchoredFragment"
          "Ouroboros/Consensus/Util/Args"
          "Ouroboros/Consensus/Util/Assert"
          "Ouroboros/Consensus/Util/CallStack"
          "Ouroboros/Consensus/Util/CBOR"
          "Ouroboros/Consensus/Util/Condense"
          "Ouroboros/Consensus/Util/Counting"
          "Ouroboros/Consensus/Util/DepPair"
          "Ouroboros/Consensus/Util/EarlyExit"
          "Ouroboros/Consensus/Util/FileLock"
          "Ouroboros/Consensus/Util/HList"
          "Ouroboros/Consensus/Util/IOLike"
          "Ouroboros/Consensus/Util/MonadSTM/NormalForm"
          "Ouroboros/Consensus/Util/MonadSTM/RAWLock"
          "Ouroboros/Consensus/Util/MonadSTM/StrictMVar"
          "Ouroboros/Consensus/Util/OptNP"
          "Ouroboros/Consensus/Util/Orphans"
          "Ouroboros/Consensus/Util/RedundantConstraints"
          "Ouroboros/Consensus/Util/ResourceRegistry"
          "Ouroboros/Consensus/Util/Singletons"
          "Ouroboros/Consensus/Util/SOP"
          "Ouroboros/Consensus/Util/STM"
          "Ouroboros/Consensus/Util/Time"
          "Ouroboros/Consensus/Util/TraceSize"
          "Ouroboros/Consensus/Util/Versioned"
          "Ouroboros/Consensus/Storage/ChainDB"
          "Ouroboros/Consensus/Storage/ChainDB/API"
          "Ouroboros/Consensus/Storage/ChainDB/Impl"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/Args"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/Background"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/BlockCache"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/ChainSel"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/Follower"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/Iterator"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/LgrDB"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/Paths"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/Query"
          "Ouroboros/Consensus/Storage/ChainDB/Impl/Types"
          "Ouroboros/Consensus/Storage/ChainDB/Init"
          "Ouroboros/Consensus/Storage/Common"
          "Ouroboros/Consensus/Storage/FS/API"
          "Ouroboros/Consensus/Storage/FS/API/Types"
          "Ouroboros/Consensus/Storage/FS/CRC"
          "Ouroboros/Consensus/Storage/FS/Handle"
          "Ouroboros/Consensus/Storage/FS/IO"
          "Ouroboros/Consensus/Storage/IO"
          "Ouroboros/Consensus/Storage/ImmutableDB"
          "Ouroboros/Consensus/Storage/ImmutableDB/API"
          "Ouroboros/Consensus/Storage/ImmutableDB/Chunks"
          "Ouroboros/Consensus/Storage/ImmutableDB/Chunks/Internal"
          "Ouroboros/Consensus/Storage/ImmutableDB/Chunks/Layout"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Index"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Index/Cache"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Index/Primary"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Index/Secondary"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Iterator"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Parser"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/State"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Types"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Util"
          "Ouroboros/Consensus/Storage/ImmutableDB/Impl/Validation"
          "Ouroboros/Consensus/Storage/LedgerDB/DiskPolicy"
          "Ouroboros/Consensus/Storage/LedgerDB/InMemory"
          "Ouroboros/Consensus/Storage/LedgerDB/OnDisk"
          "Ouroboros/Consensus/Storage/LedgerDB/Types"
          "Ouroboros/Consensus/Storage/Serialisation"
          "Ouroboros/Consensus/Storage/VolatileDB"
          "Ouroboros/Consensus/Storage/VolatileDB/API"
          "Ouroboros/Consensus/Storage/VolatileDB/Impl"
          "Ouroboros/Consensus/Storage/VolatileDB/Impl/FileInfo"
          "Ouroboros/Consensus/Storage/VolatileDB/Impl/Index"
          "Ouroboros/Consensus/Storage/VolatileDB/Impl/Parser"
          "Ouroboros/Consensus/Storage/VolatileDB/Impl/State"
          "Ouroboros/Consensus/Storage/VolatileDB/Impl/Types"
          "Ouroboros/Consensus/Storage/VolatileDB/Impl/Util"
          "Data/SOP/Strict"
          ] ++ (pkgs.lib).optional (system.isWindows) "Ouroboros/Consensus/Storage/Seek";
        hsSourceDirs = [ "src" ] ++ (if system.isWindows
          then [ "src-win32" ]
          else [ "src-unix" ]);
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "10";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "10";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/ouroboros-consensus; echo source root reset to $sourceRoot";
    }