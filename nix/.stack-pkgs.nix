{
  extras = hackage:
    {
      packages = {
        "containers" = (((hackage.containers)."0.5.11.0").revisions).default;
        "ekg-prometheus-adapter" = (((hackage.ekg-prometheus-adapter)."0.1.0.4").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.1").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
        "pretty-show" = (((hackage.pretty-show)."1.8.2").revisions).default;
        } // {
        cardano-node = ./.stack.nix/cardano-node.nix;
        cardano-ledger = ./.stack.nix/cardano-ledger.nix;
        cardano-crypto-wrapper = ./.stack.nix/cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./.stack.nix/cardano-crypto-test.nix;
        cardano-ledger-test = ./.stack.nix/cardano-ledger-test.nix;
        cardano-prelude = ./.stack.nix/cardano-prelude.nix;
        cardano-prelude-test = ./.stack.nix/cardano-prelude-test.nix;
        cardano-binary = ./.stack.nix/cardano-binary.nix;
        cardano-binary-test = ./.stack.nix/cardano-binary-test.nix;
        cardano-shell = ./.stack.nix/cardano-shell.nix;
        contra-tracer = ./.stack.nix/contra-tracer.nix;
        iohk-monitoring = ./.stack.nix/iohk-monitoring.nix;
        cardano-sl-x509 = ./.stack.nix/cardano-sl-x509.nix;
        bimap = ./.stack.nix/bimap.nix;
        io-sim-classes = ./.stack.nix/io-sim-classes.nix;
        ouroboros-network = ./.stack.nix/ouroboros-network.nix;
        ouroboros-consensus = ./.stack.nix/ouroboros-consensus.nix;
        typed-protocols = ./.stack.nix/typed-protocols.nix;
        cborg = ./.stack.nix/cborg.nix;
        cardano-crypto = ./.stack.nix/cardano-crypto.nix;
        canonical-json = ./.stack.nix/canonical-json.nix;
        };
      compiler.version = "8.6.4";
      compiler.nix-name = "ghc864";
      };
  resolver = "lts-13.16";
  compiler = "ghc-8.6.4";
  }
