{ pkgs
, runJq

, backend
, profile
, nodeSpecs
}:

with pkgs.lib;

let
  finaliseTracerService =
    svc: recursiveUpdate svc
      ({
        configFile     = "config.json";
        logRoot        = ".";
      } // optionalAttrs backend.useCabalRun {
        executable     = "cardano-tracer";
      } // optionalAttrs profile.node.rtview {
        RTView         = {
          epHost = "127.0.0.1";
          epPort = 3300;
        };
      });

  ##
  ## nodeSpecsTracerConfig :: Map NodeId NodeSpec -> TracerConfig
  ##
  nodeSpecsTracerConfig =
    nodeSpecs:
    let
    in
        finaliseTracerService
        {
          ## In both the local and remote scenarios, it's most frequently convenient to act as an acceptor.
          acceptingSocket = "tracer.socket";

          networkMagic = profile.genesis.network_magic;

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
      modules = import ../../nixos/module-list.nix
                ++ [ (import ../../nixos/cardano-tracer-service.nix pkgs)
                     systemdCompat extra
                     { config._module.args = { inherit pkgs; }; }
                   ]
                ++ [ backend.service-modules.tracer or {} ];
      # args = { inherit pkgs; };
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
    nodeSpecs.value;
in
{
  inherit tracer-service;
}
