{ config, pkgs, lib, ... }:

#
# Module describing the genesis component of a profile.
#

with lib;

let
  json = pkgs.formats.json { };

  # handy aliases
  inherit (config) composition derived genesis;
in

{
  options = {
    genesis = {
      ## NOTE: The following defaults follow the values in prof0-defaults

      ## Trivia

      network_magic = mkOption {
        description = "The network magic id";
        type = types.ints.unsigned;
        default = 42;
      };

      ## Incrementality

      single_shot = mkOption {
        description = "Incrementality: single_shot";
        type = types.bool;
        default = true;
      };

      ## UTxO & delegation

      per_pool_balance = mkOption {
        # part of the profile
        description = "UTxO & delegation: per_pool_balance";
        type = types.int;
        default = 1000000000000000;
      };

      funds_balance = mkOption {
        description = "UTxO & delegation: funds_balance";
        type = types.int;
        default = 10000000000000;
      };

      utxo = mkOption {
        description = "UTxO & delegation: utxo";
        type = types.int;
        default = 0;
      };

      ## Blockchain time & block density

      active_slots_coeff = mkOption {
        description = "Blockchain time & block density: active_slots_coeff";
        type = types.float;
        default = 0.05;
      };

      epoch_length = mkOption {
        description = "Blockchain time & block density: epoch_length. Ought to be at least (10 * k / f)";
        type = types.ints.unsigned;
        default = 600;
      };

      parameter_k = mkOption {
        description = "Blockchain time & block density: k";
        type = types.ints.unsigned;
        default = 3;
      };

      slot_duration = mkOption {
        description = "Blockchain time & block density: slot_duration";
        type = types.ints.unsigned;
        default = 1;
      };

      ## Protocol parameters

      pparamsEpoch = mkOption {
        description = "Blockchain time & block density: pparamsEpoch";
        type = types.ints.unsigned;
        default = 300;
        # NOTE: See pparams/epoch-timeline.jq
      };

      pparamsOverlays = mkOption {
        description = "Blockchain time & block density: pparamsOverlays";
        type = types.anything; # FIXME: is this right?
        default = [ ];
      };

      ##
      ## NOTE: The following options are defined in prof3-derived.
      ##

      delegator_coin = mkOption {
        description = "delegator_coin";
        type = types.ints.unsigned;
      };

      delegators = mkOption {
        description = "delegators";
        type = types.ints.unsigned;
      };

      pool_coin = mkOption {
        # Only needed for genesis creation, to be removed from profile
        description = "pool_coin";
        type = types.ints.unsigned;
      };

      # NOTE:
      #
      # In defining the options for the genesis templates, we use mkDefault in the definition
      # of the submodule, rather than specifying a default value for the option.
      #
      # i.e.
      #
      # { type = submodule { freeformType = _; config = mkDefault { a = 1; b = 2; }; }; default = { }; }
      #
      # rather than
      #
      # { type = submodule { freeformType = _; }; default = { a = 1; b = 2; }; }
      #
      # In this way each single value is a default value. While in the second case the default
      # value would be all or nothing.
      #
      # i.e.
      #
      # (evalModules {
      #   modules = [{
      #     options.abc = mkOption {
      #       type = types.submodule {
      #         freeformType = (pkgs.formats.json { }).type;
      #         config = mkDefault { a = 1; b = 2; };
      #       };
      #     };
      #     config.abc.a = 10;
      #   }];
      # }).config
      #
      # evaluates to
      #
      # { abc = { a = 10; b = 2; }; }
      #
      # while
      #
      # (evalModules {
      #   modules = [{
      #     options.abc = mkOption {
      #       type = types.submodule {
      #         freeformType = (pkgs.formats.json { }).type;
      #       };
      #       default = { a = 1; b = 2; };
      #     };
      #     config.abc.a = 10;
      #   }];
      # }).config
      #
      # evaluates to
      #
      # { abc = { a = 10; }; }

      alonzo = mkOption {
        description = "The Alonzo genesis template";
        type = types.submodule {
          freeformType = json.type;
          # See note above about mkDefault
          config = mkDefault (importJSON ../profile/presets/mainnet/genesis/genesis.alonzo.json);
        };
        default = { };
      };

      shelley = mkOption {
        description = "The Shelley genesis template";
        type = types.submodule {
          freeformType = json.type;
          # See note above about mkDefault
          config = mkDefault (importJSON ../profile/presets/mainnet/genesis/genesis-shelley.json);
        };
        default = { };
      };

      conway = mkOption {
        description = "The Conway genesis template";
        type = types.submodule {
          freeformType = json.type;
          # See note above about mkDefault
          config = mkDefault (importJSON ../profile/presets/mainnet/genesis/genesis.conway.json);
        };
        default = { };
      };

      byron = mkOption {
        description = "Byron protocol parameters";
        type = types.submodule {
          freeformType = json.type;
          config = mkDefault {
            heavyDelThd = "300000";
            maxBlockSize = "641000";
            maxHeaderSize = "200000";
            maxProposalSize = "700";
            maxTxSize = "4096";
            mpcThd = "200000";
            scriptVersion = 0;
            slotDuration = "20000";
            softforkRule = {
              initThd = "900000";
              minThd = "600000";
              thdDecrement = "100000";
            };
            txFeePolicy = {
              multiplier = "439460";
              summand = "155381";
            };
            unlockStakeEpoch = "184467";
            updateImplicit = "10000";
            updateProposalThd = "100000";
            updateVoteThd = "100000";
          };
        };
        default = { };
      };

      create-staked-args = mkOption {
        description = "Arguments to cardano-cli genesis create-staked";
        type = types.separatedString " ";
      };

      byron-genesis-args = mkOption {
        description = "Arguments to cardano-cli byron genesis genesis";
        type = types.separatedString " ";
      };

      pool-relays = mkOption {
        description = "pool-relays.json";
        type = types.submodule { freeformType = json.type; };
      };

      cache-key = mkOption {
        type = types.str;
        readOnly = true;
      };

      cache-key-input = mkOption {
        type = types.submodule { freeformType = json.type; };
        readOnly = true;
      };

      extra_future_offset = mkOption {
        description = ''
          Blockchain time & block density: extra_future_offset.

          Related to automation, genesis creation takes some time and it cannot be created in the future or too far in the past. It depends on the dataset size.
        '';
        type = types.ints.unsigned;
        default = 0;
      };
    };
  };

  config = {
    genesis = {
      create-staked-args = concatStringsSep " " ([
        "--supply ${toString genesis.funds_balance}"
        "--gen-utxo-keys 1"
        "--gen-genesis-keys ${toString composition.n_bft_hosts}"
        "--supply-delegated ${toString derived.supply_delegated}"
        "--gen-pools ${toString composition.n_pools}"
        "--gen-stake-delegs ${toString derived.delegators_effective}"
        "--num-stuffed-utxo ${toString derived.utxo_stuffed}"
        "--testnet-magic ${toString genesis.network_magic}"
      ]
      ++ optional (composition.dense_pool_density != 1) [
        "--bulk-pool-cred-files ${toString composition.n_dense_hosts}"
        "--bulk-pools-per-file ${toString composition.dense_pool_density}"
      ]);

      byron-genesis-args = concatStringsSep " " ([
        "--k ${toString genesis.parameter_k}"
        "--protocol-magic ${toString genesis.network_magic}"
        "--n-poor-addresses 1"
        "--n-delegate-addresses 1"
        "--total-balance 300000"
        "--delegate-share 0.9"
        "--avvm-entry-count 0"
        "--avvm-entry-balance 0"
      ]);

      cache-key-input = {
        inherit (composition) n_pools n_bft_hosts n_dense_hosts dense_pool_density;
        inherit (derived) utxo_stuffed;
        inherit (genesis) delegators funds_balance network_magic per_pool_balance pool_coin;
      };

      cache-key =
        concatStringsSep "-"
          ([ "k${toString composition.n_pools}" ]
            ++ optional (composition.dense_pool_density != 1) "d${toString composition.dense_pool_density}"
            ++ [
            "${toString (genesis.delegators / 1000)}kD"
            "${toString (derived.utxo_stuffed / 1000)}kU"
            (substring 0 7 (builtins.hashString "sha1" (builtins.toJSON genesis.cache-key-input)))
          ]);

      pool-relays = mapAttrs'
        (name: value: {
          name = toString value.i;
          value = [{ "single host name" = { dnsName = name; port = value.port; }; }];
        })
        config.node-specs;
    };
  };
}
