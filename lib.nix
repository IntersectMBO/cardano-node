let
  # iohk-nix can be overridden for debugging purposes by setting
  # NIX_PATH=iohk_nix=/path/to/iohk-nix
  iohkNix = import (
    let try = builtins.tryEval <iohk_nix>;
    in if try.success
    then builtins.trace "using host <iohk_nix>" try.value
    else
      let
        spec = builtins.fromJSON (builtins.readFile ./nix/iohk-nix-src.json);
      in builtins.fetchTarball {
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      }) {};

  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
in lib // {
  inherit iohkNix pkgs;
  inherit (iohkNix) nix-tools;
  environments = {
    mainnet = {
      relays = "relays.cardano-mainnet.iohk.io";
      confKey = "mainnet_full";
      genesisFile = ./configuration/mainnet-genesis.json;
      genesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb";
      private = false;
    };
    staging = {
      relays = "relays.awstest.iohkdev.io";
      confKey = "mainnet_dryrun_full";
      genesisFile = ./configuration/mainnet-genesis-dryrun-with-stakeholders.json;
      genesisHash = "c6a004d3d178f600cd8caa10abbebe1549bef878f0665aea2903472d5abf7323";
      private = false;
    };
    testnet = {
      relays = "relays.cardano-testnet.iohkdev.io";
      confKey = "testnet_full";
      genesisFile = ./configuration/testnet-genesis.json;
      genesisHash = "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471";
      private = false;
    };
    shelley_staging = {
      relays = "relays.shelley-staging.aws.iohkdev.io";
      confKey = "shelley_staging_full";
      genesisFile = ./configuration/shelley-staging-genesis.json;
      genesisHash = "82995abf3e0e0f8ab9a6448875536a1cba305f3ddde18cd5ff54c32d7a5978c6";
      private = false;
    };
    shelley_staging_short = {
      relays = "relays.staging-shelley-short.aws.iohkdev.io";
      confKey = "shelley_staging_short_full";
      genesisFile = ./configuration/shelley-staging-short-genesis.json;
      genesisHash = "703e238001bb306ea0b588b566f9655cc3129f0470d83f0033b5de5663b71830";
      private = false;
    };
  };
}
