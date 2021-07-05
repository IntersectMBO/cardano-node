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
    let cmd = map normaliseCmdArg ([
          config.executable
        ] ++ configExeArgsFn config);
        normaliseCmdArg = x:
          ({ int = toString; float = toString; string = id;
             ## TODO: ugh, why oh why:
             ## while evaluating 'normaliseCmdArg' at svclib.nix:32:27:
             ## while evaluating anonymous function at tx-generator-service.nix:98:16:
             ## file '/dev/null' has an unsupported type

             path = x: "${x}";
           }."${__typeOf x}" or
            (_: throw "\nWhile processing service declaration for '${svcName}':  Unsupported type of command arg: ${__typeOf x}\n")) x;
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
    let dsmDeclSpec = argClosure (pkgs.lib // serviceDeclarationLib);
    in defServiceModule__ dsmDeclSpec;
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
            default = "${svcConfig.package.components.exes.${exeName}}/bin/${exeName}" or
              (throw "the package for ${svcName} not have the components.exes.${exeName} attribute -- please adjust service configuration.");
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
      config = mkIf svcConfig.enable ({
        systemd.services."${svcName}" = {
          enable        = true;
          description   = "${svcName}, ${svcDesc}.";
          script        = svcConfig.script;
          serviceConfig = {
            User = "${svcName}";
            Group = "${svcName}";
          } // systemdExtraServiceConfig;
        } // systemdExtraConfig;
        assertions = configAssertions { inherit lib; config = svcConfig; };
      });
    };

  ## A small library of helpers, to establish comfortable
  ## and compact definition of services.
  serviceDeclarationLib = with pkgs.lib; rec {
    opt = type: default: description:
              mkOption { inherit type default description; };
    intOpt  = opt types.int;
    pathOpt = opt (types.or types.path types.str);
    strOpt  = opt types.str;
    attrOpt = opt types.attrs;
    listOpt = opt (types.listOf types.attrs);
    enumOpt = set: default: description:
      mkOption { inherit default description;
                 type = types.enum set; };
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
in
{
  inherit
  defServiceModule
  mkScriptOfService
  mkServiceScript
  ;
}
