{
  extras = hackage:
    {
      packages = {
        "binary" = (((hackage.binary)."0.8.7.0").revisions).default;
        "containers" = (((hackage.containers)."0.5.11.0").revisions).default;
        "ekg-prometheus-adapter" = (((hackage.ekg-prometheus-adapter)."0.1.0.4").revisions).default;
        "generic-monoid" = (((hackage.generic-monoid)."0.1.0.0").revisions).default;
        "prometheus" = (((hackage.prometheus)."2.1.1").revisions).default;
        "pvss" = (((hackage.pvss)."0.2.0").revisions).default;
        "time-units" = (((hackage.time-units)."1.0.0").revisions).default;
        "libsystemd-journal" = (((hackage.libsystemd-journal)."1.4.4").revisions).default;
        "tasty-hedgehog" = (((hackage.tasty-hedgehog)."1.0.0.1").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        "cborg" = (((hackage.cborg)."0.2.2.0").revisions).default;
        "canonical-json" = (((hackage.canonical-json)."0.6.0.0").revisions).default;
        "brick" = (((hackage.brick)."0.47").revisions)."4936c50acfdf09620dad5217fb384fc0d59626f75abed8b48250b419ec2ab623";
        "config-ini" = (((hackage.config-ini)."0.2.4.0").revisions)."38a6d484d471c6fac81445de2eac8c4e8c82760962fca5491ae1c3bfca9c4047";
        "data-clist" = (((hackage.data-clist)."0.1.2.2").revisions)."4d70add0a200a178853cd37c6469101bac3c36aebb3aa9c503ff225211b1a8c9";
        "text-zipper" = (((hackage.text-zipper)."0.10.1").revisions)."8b73a97a3717a17df9b0a722b178950c476ff2268ca5c583e99d010c94af849e";
        "word-wrap" = (((hackage.word-wrap)."0.4.1").revisions)."f72233b383ef569c557bfd9812cbb8e306c415ce509082c0bd15ee51c0239ccc";
        } // {
        cardano-config = ./cardano-config.nix;
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
        cardano-crypto = ./cardano-crypto.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  compiler = "ghc-8.6.5";
  }