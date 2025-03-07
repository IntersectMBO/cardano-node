{ pkgs

, backend
, profile
, nodeSpecs
}:

with pkgs.lib;

let

  ## Given an env config, evaluate it and produce the service.
  ##
  ## tracerConfigServiceConfig :: TracerConfig -> NixosServiceConfig
  ##
  tracerConfigServiceConfig =
    let
      tracerConfig =
        {
          ## In both the local and remote scenarios, it's most frequently
          ## convenient to act as an acceptor.
          acceptingSocket = "tracer.socket";
          networkMagic = profile.genesis.network_magic;
          dsmPassthrough = {
            # rtsOpts = ["-xc"];
          } // optionalAttrs (profile.tracer.withresources or false) {
            rtsOpts = [ "-scardano-tracer.gcstats" ];
          };
          configFile     = "config.json";
          logRoot        = ".";
          metricsHelp    = "../../../cardano-tracer/configuration/metrics_help.json";
        } // optionalAttrs backend.useCabalRun {
          executable     = "cardano-tracer";
        } // optionalAttrs profile.tracer.rtview {
          RTView         = {
            epHost = "127.0.0.1";
            epPort = 3300;
          };
        } // optionalAttrs (profile.tracer.withresources or false) {
          resourceFreq = 1000;
        }
      ;
      systemdCompat.options = {
        systemd.services = mkOption {};
        systemd.sockets = mkOption {};
        users = mkOption {};
        assertions = mkOption {};
        environment = mkOption {};
      };
      eval =
        let
          extra = {
            services.cardano-tracer = {
              enable = true;
            } // tracerConfig;
          };
        in evalModules {
          prefix = [];
         modules = [
                     (import ../../nixos/cardano-node-service.nix)
                     (import ../../nixos/cardano-submit-api-service.nix)
                     # (import ../../nixos/cardano-tracer-service.nix)
                     (import ../../nixos/cardano-tracer-service-legacy.nix pkgs)
                     systemdCompat
                     extra
                     { config._module.args = { inherit pkgs; }; }
                   ]
                   ++ [ backend.service-modules.tracer or {} ]
          ;
          # args = { inherit pkgs; };
        }
      ;
    in
      eval.config.services.cardano-tracer;

  ##
  ## generator-service :: (TracerConfig, NixosServiceConfig, Config, StartScript)
  ##
  tracer-service =
    (nodeSpecs:
    let
      nixosServiceConfig    = tracerConfigServiceConfig;
      execConfig            = nixosServiceConfig.configJSONfn nixosServiceConfig;
    in {
      start =
        ''
        #!${pkgs.stdenv.shell}

        ${nixosServiceConfig.script}
        ''
      ;

      config = execConfig;
    })
    nodeSpecs;
in
{
  inherit tracer-service;
}
