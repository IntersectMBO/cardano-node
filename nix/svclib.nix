{ pkgs }:
with pkgs.lib;
let
  inherit (pkgs) lib;

  id = x: x;

  ## Make a launch script for a service,
  ## given:
  ##  - service name
  ##  - package containing the service binary
  ##  - name of the service binary
  ##  - service configuration
  ##  - transformation from service configuration to binary arglist
  ##  - optional pre-start shell snippet
  ##
  mkServiceScript =
    { svcName
    , package
    , exeName
    , config
    , configExeArgsFn
    , configScriptPreambleFn
    , traceServiceStartup ? false
    }:
    let cmd = map normaliseCmdArg
                  ([config.executable] ++ configExeArgsFn config);
        normaliseCmdArg = x:
            (({ float  = toString;
                int    = toString;
                path   = toString;
                string = toString;
              }."${__typeOf x}" or
              (_: throw "\nWhile computing the stringified arglist for service '${svcName}':  Unsupported type of command arg: ${__typeOf (traceValSeqN 2 x)}\n")) x);
    in  ''
        ${if traceServiceStartup then "set -x" else ""}
        ${configScriptPreambleFn config}
        CMD=(
            '' + concatStringsSep "\n     " cmd +
        ''

        )
        echo -e "Starting service ${svcName}:\n"

        for x in "''${CMD[@]}"
        do echo "    ''${x}"; done

        echo -e "\n..or, once again, in a single line:\n"
        echo -e "    ''${CMD[@]@Q}\n"

        "''${CMD[@]}"
        status=$?

        echo "Service binary '${exeName}' returned status: $status" >&2
        exit $status'';

  ## Define a typical service module,
  ## given:
  ##  - service name (must be a valid NixOS & systemd service name & Unix username)
  ##  - free-form, one-line service description
  ##  - package selector function, of type (NixPkgs -> Package)
  ##  - executable name (must be a valid cabal project executable)
  ##  - extra NixOS module option declarations (default: {})
  ##  - executable's arglist (default: _: [])
  ##  - script preamble shell snippet (default: _: "")
  ##  - systemd extra config (see defaults below)
  ##  - systemd extra service config (see defaults below)
  defServiceModule = argClosure:
    traceValSeqN 2
    (defServiceModule__ (argClosure (pkgs.lib // serviceDeclarationLib)));
  defServiceModule__ =
    { ## WARNING: when changing this arglist,
      ## make sure to update 'unhandledArgs' below.
      svcName
    , svcDesc
    , svcPackageSelector
    , extraOptionDecls ? {}
    , exeName
    , configExeArgsFn ? (config: ["--help"])
    , configRtsOpts ? (config: [])
    , configScriptPreambleFn ? (config: "")
    , configSystemdExtraConfig ? null
    , configSystemdExtraServiceConfig ? null
    , configAssertions ? ({ config, lib }: [])
    }@args:
    { config
    , lib
    , pkgs
    , ... }:
    let svcConfig = config.services."${svcName}";
        systemdDefaults = _: {
          after = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
        };
        systemdServiceDefaults = _: {
          Restart = "yes";
          # WorkingDirectory = ncfg.stateDir;
          # StateDirectory =  lib.removePrefix stateDirBase ncfg.stateDir;
        };
        systemdExtraConfig =
          (if configSystemdExtraConfig == null
           then systemdDefaults else configSystemdExtraConfig)
            svcConfig;
        systemdExtraServiceConfig =
          (if configSystemdExtraServiceConfig == null
           then systemdServiceDefaults else configSystemdExtraServiceConfig)
            svcConfig;
        unhandledArgs = attrNames (removeAttrs args
          [ "svcName" "svcDesc" "svcPackageSelector" "extraOptionDecls"
            "exeName" "configExeArgsFn"
            "configScriptPreambleFn" "configSystemdExtraConfig"
            "configSystemdExtraServiceConfig" "configAssertions"
            "configRtsOpts"
            "traceServiceStartup"
          ]);
        handleUnhandledArgs = rest:
          if unhandledArgs == [] then rest
          else throw "Unhandled args to defServiceModule ${toString unhandledArgs}";
    in handleUnhandledArgs {
      options = {
        services."${svcName}" = {
          enable = mkOption { type = types.bool; default = false; description = ''Enable ${svcName}, ${svcDesc}.''; };
          package = mkOption {
            type = types.package;
            default = pkgs.${svcName} or
              (throw "the 'pkgs' does not have ${svcName} -- please adjust service configuration.");
            description = ''The package for ${svcName} that should be used.'';
          };
          executable = mkOption {
            type = types.str;
            default ="${if __hasAttr "components" svcConfig.package
                        then svcConfig.package.components.exes.${exeName} or
                          (throw "the package for ${svcName} not have the 'components.exes.${exeName}' attribute -- please set service 'executable'.")
                        else svcConfig.package}/bin/${exeName}";
            description = ''The executable for ${svcName} that should be used.'';
          };
          script = mkOption { type = types.str;  default =
            mkServiceScript {
              inherit svcName exeName configScriptPreambleFn;
              config = svcConfig;
              package = svcPackageSelector pkgs;
              traceServiceStartup =
                svcConfig.dsmPassthrough.traceServiceStartup or false;
              configExeArgsFn = cfg:
                configExeArgsFn cfg
                ++ (let rts = configRtsOpts cfg
                              ++ svcConfig.dsmPassthrough.rtsOpts or [];
                    in if rts == []
                       then [] else ["+RTS"] ++ rts ++ ["-RTS"]);
            };
          };
          dsmPassthrough = mkOption {
            type = types.attrs; default = {};
            description = "Pass through generic args to defServiceModule.";
          };
        } // extraOptionDecls;
      };
      config = {
        systemd.services."${svcName}" = {
          enable        = svcConfig.enable;
          description   = "${svcName}, ${svcDesc}.";
          script        = svcConfig.script;
          serviceConfig = {
            User = "${svcName}";
            Group = "${svcName}";
          } // systemdExtraServiceConfig;
        } // systemdExtraConfig;
        assertions = configAssertions { inherit lib; config = svcConfig; };
      };
    };

  ## A small library of helpers, to establish comfortable
  ## and compact definition of services.
  serviceDeclarationLib = with pkgs.lib; with types;
    types // rec {
      ## A shorter alternative to mkOption.
      opt    = type: default: description:
               mkOption { inherit type default description; };
      ## Maybe `opt`: same as `opt`, but nullable and defaults to `null`.
      mayOpt = type: description:
               opt (nullOr type) null description;
    };

  ## Make a script equivalent to a startup script of a NixOS service,
  ## given:
  ##  - a NixOS service name,
  ##  - its module dependency list
  ##  - its optional configuration
  ## mkScriptOfService
  ##   :: String ServiceName
  ##   -> Attrs ServiceConfig
  ##   -> [NixPath NixOSModule]  -- Modules used by service
  ##   -> String ShellCmd
  extractServiceScript =
    { svcName
    , svcModules
    , config ? {}
    }:
   let
    script = svcConfig.script;
    svcConfig = combinedModule.config.services."${svcName}";
      ## TODO:  sadly this obscures argument errors.
      # let r = tryEval combinedModule.config.services."${svcName}";
      # in if r.success == true
      #    then r.value
      #    else throw "Service '${svcName}' not defined by modules:  ${toString svcModules}";
    combinedModule = modules.evalModules {
      modules = svcModules ++ [
        systemdCompat
        injectServiceConfigs
        pkgsModule
      ];
    };
    injectServiceConfigs = {
      config.services."${svcName}" = { enable = true; } // config;
    };
    pkgsModule = {
      config._module.args.pkgs = mkDefault pkgs;
    };
    systemdCompat.options = {
      systemd.services = mkOption {};
      assertions = [];
      users = mkOption {};
    };
  in pkgs.writeScript "run-${svcName}" ''
    #!${pkgs.runtimeShell}

    ${script} "$@"
  '';

  processScriptDeclaration =
    name: decl:
    extractServiceScript (decl // { svcName = name; });

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # NOTE: due to some cabal limitation,
  #  you have to remove all `source-repository-package` entries from cabal.project
  #  after entering nix-shell for cabal to use nix provided dependencies for them.
  mkSupervisordCluster =
    { useCabalRun, profileName }:
    pkgs.callPackage ./supervisord-cluster
      { inherit profileName useCabalRun;
        workbench = pkgs.callPackage ./workbench { inherit useCabalRun; };
      };
in
{
  inherit
  defServiceModule
  mkScriptOfService
  mkServiceScript
  mkSupervisordCluster
  ;
}
