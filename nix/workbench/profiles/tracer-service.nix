{ pkgs
, runJq

## An attrset of specific methods and parameters.
, services-config

, profile
}:

with pkgs.lib;

let
  # We're reusing configuration from a cluster node.
  nodes = profile.node-services;

  ##
  ## nodeSpecsTracerConfig :: Map NodeId NodeSpec -> TracerConfig
  ##
  nodeSpecsTracerConfig =
    nodeSpecs:
    let
    in
        services-config.finaliseTracerService
        {
          ## In both the local and remote scenarios, it's most frequently convenient to act as an acceptor.
          acceptingSocket = "tracer.socket";

          networkMagic = profile.value.genesis.network_magic;

          ## logRoot = ## ..really depends on context -- available in services-config.finaliseTracerService

          dsmPassthrough = {
            # rtsOpts = ["-xc"];
          };
        };

  ## Given an env config, evaluate it and produce the service.
  ##
  ## tracerConfigServiceConfig :: TracerConfig -> NixosServiceConfig
  ##
  tracerConfigServiceConfig =
    tracerConfig:
    let
    systemdCompat.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      users = mkOption {};
      assertions = mkOption {};
    };
    eval = let
      extra = {
        services.cardano-tracer = {
          enable = true;
        } // tracerConfig;
      };
    in evalModules {
      prefix = [];
      modules = import ../../nixos/module-list.nix ++ [
        (import ../../nixos/cardano-tracer-service.nix pkgs)
        systemdCompat extra
      ];
      args = { inherit pkgs; };
    };
    in eval.config.services.cardano-tracer;

  ##
  ## generator-service :: (TracerConfig, NixosServiceConfig, Config, StartScript)
  ##
  tracer-service =
    (nodeSpecs:
    let
      tracerConfig          = nodeSpecsTracerConfig nodeSpecs;
      nixosServiceConfig    = tracerConfigServiceConfig tracerConfig;
      nixosServiceConfigFns = ["configJSONfn"];
      execConfig            = nixosServiceConfig.configJSONfn nixosServiceConfig;
    in {
      tracer-config = {
        value = tracerConfig;
        JSON  = runJq "tracer-config.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON tracerConfig}'
                  '' "$x";
      };

      nixos-service-config = {
        ## XXX: service == appallingly bad name -- it's bona-fide NixOS service "config", not a service!
        value = nixosServiceConfig;
        JSON  = runJq "nixos-service-config.json"
                  ''--null-input --sort-keys
                    --argjson x '${__toJSON (removeAttrs nixosServiceConfig nixosServiceConfigFns)}'
                  '' "$x";
      };

      config = rec {
        value = execConfig;
        JSON  = runJq "config.json"
                  ''--null-input
                    --argjson x '${__toJSON execConfig}'
                  '' "$x";
      };

      startupScript = rec {
        JSON = pkgs.writeScript "startup-tracer.sh" value;
        value = ''
          #!${pkgs.stdenv.shell}

          ${nixosServiceConfig.script}
          '';
      };
    })
    profile.node-specs.value;
in
{
  inherit tracer-service;
}
