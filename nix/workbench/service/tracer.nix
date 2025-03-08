{ pkgs

, backend
, profile
, nodeSpecs
}:

with pkgs.lib;

let
  # For testing legacy to new cardano-tracer-service transition
  # useLegacyTracerService = true;
  useLegacyTracerService = false;

  ## Given an env config, evaluate it and produce the service.
  ##
  ## tracerConfigServiceConfig :: TracerConfig -> NixosServiceConfig
  ##
  tracerConfigServiceConfig =
    let
      tracerConfig =
        {
          enable = true;
          ## In both the local and remote scenarios, it's most frequently
          ## convenient to act as an acceptor.
          acceptingSocket = "tracer.socket";
          networkMagic = profile.genesis.network_magic;
          configFile     = "config.json";
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
        } // optionalAttrs useLegacyTracerService {
          dsmPassthrough = {
            # rtsOpts = ["-xc"];
          } // optionalAttrs (profile.tracer.withresources or false) {
            rtsOpts = [ "-scardano-tracer.gcstats" ];
          };
          logRoot    = ".";
        } // optionalAttrs (!useLegacyTracerService) {
          logging = [
            {
              logRoot    = ".";
              logMode    = "FileMode";
              logFormat  = "ForMachine";
            }
          ];
          rtsArgs =
            # ["-xc"] ++
            optionals (profile.tracer.withresources or false) ["-scardano-tracer.gcstats"];
          stateDir = null;
        }
      ;
      systemdCompat.options = {
        systemd.services = mkOption {};
        systemd.sockets = mkOption {};
        users = mkOption {};
        assertions = mkOption {};
        environment = mkOption {};
      };
      eval = evalModules {
        prefix = [];

        modules = [
          (import ../../nixos/cardano-node-service.nix)
          (import ../../nixos/cardano-submit-api-service.nix)
          { config._module.args = { inherit pkgs; }; }
          { services.cardano-tracer = tracerConfig; }
          systemdCompat
        ]
          ++ [ backend.service-modules.tracer or {} ]
          ++ optionals useLegacyTracerService
            [ (import ../../nixos/cardano-tracer-service-legacy.nix pkgs) ]
          ++ optionals (!useLegacyTracerService)
            [ (import ../../nixos/cardano-tracer-service.nix) ]
          ;

        # args = { inherit pkgs; };
      };
    in
      eval.config.services.cardano-tracer;

  ##
  ## generator-service :: (TracerConfig, NixosServiceConfig, Config, StartScript)
  ##
  tracer-service =
    (nodeSpecs:
    let
      nixosServiceConfig    = tracerConfigServiceConfig;
      execConfig            = if useWorkbenchTracerService
                              then nixosServiceConfig.configJSONfn nixosServiceConfig
                              else nixosServiceConfig.tracerConfig;
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
