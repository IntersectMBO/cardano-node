{
  extras = hackage:
    {
      packages = {
        "containers" = (((hackage.containers)."0.5.11.0").revisions).default;
        "ekg-prometheus-adapter" = (((hackage.ekg-prometheus-adapter)."0.1.0.4").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.1").revisions).default;
        "pvss" = (((hackage.pvss)."0.2.0").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "pretty-show" = (((hackage.pretty-show)."1.8.2").revisions).default;
        "ansi-terminal-game" = (((hackage.ansi-terminal-game)."0.3.1.0").revisions)."3a1697c0ce0216f4c7fbc02b22c8b7663d142ed6703785d2eb8fae5f1fec6005";
        "linebreak" = (((hackage.linebreak)."1.0.0.3").revisions)."3152802008e06f8444d364f6e1047b412e3cb692c1382b1657a425d72a0cfd5f";
        "timers-tick" = (((hackage.timers-tick)."0.4.1.0").revisions)."27d345648b8e01fad48adf1dc0e870b19fabb03ccdf3ad0bc5acb57f5da6cb5c";
        } // {
        cardano-node = ./cardano-node.nix;
        cardano-ledger = ./cardano-ledger.nix;
        cardano-ledger-test = ./cardano-ledger-test.nix;
        cardano-crypto-wrapper = ./cardano-crypto-wrapper.nix;
        cardano-crypto-test = ./cardano-crypto-test.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        cardano-crypto-class = ./cardano-crypto-class.nix;
        cardano-shell = ./cardano-shell.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        cardano-sl-x509 = ./cardano-sl-x509.nix;
        bimap = ./bimap.nix;
        io-sim-classes = ./io-sim-classes.nix;
        network-mux = ./network-mux.nix;
        ouroboros-network = ./ouroboros-network.nix;
        ouroboros-consensus = ./ouroboros-consensus.nix;
        typed-protocols = ./typed-protocols.nix;
        typed-protocols-cbor = ./typed-protocols-cbor.nix;
        cborg = ./cborg.nix;
        cardano-crypto = ./cardano-crypto.nix;
        canonical-json = ./canonical-json.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.24";
  compiler = "ghc-8.6.5";
  }