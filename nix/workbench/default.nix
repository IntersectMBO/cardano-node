{ pkgs, lib
, cardanoNodePackages
, cardanoNodeProject
}:

with lib;

let

  # recover CHaP location from cardano's project
  chap = cardanoNodeProject.args.inputMap."https://chap.intersectmbo.org/";
  # build plan as computed by nix
  nixPlanJson = cardanoNodeProject.plan-nix.json;

  # Workbench derivation and functions to create derivations from `wb` commands.
  ##############################################################################

  workbench' = tools:
    pkgs.stdenv.mkDerivation {
      pname = "workbench";

      version = "0.1";

      src = ./.;

      buildInputs = with pkgs; [ makeWrapper ];

      buildPhase = ''
        patchShebangs .
      '';

      postFixup = ''
        wrapProgram "$out/bin/wb"                \
          --argv0 wb                             \
          --prefix PATH ":" ${makeBinPath tools} \
          --set WB_CHAP_PATH ${chap}             \
          --set WB_NIX_PLAN ${nixPlanJson}
      '';

      installPhase = ''
        mkdir -p                                                    $out/bin
        cp    -a wb ede profile                                     $out/bin
        for dir in . analyse backend genesis topology
        do cp    -a $dir/*                                          $out/bin/$dir
        done
      '';

      dontStrip = true;
    };

  # Workbench with its dependencies to call from Nix.
  workbench = workbench' (
      (with pkgs;
        [ git graphviz
          jq
          moreutils
          procps
        ]
      )
      ++
      (with cardanoNodePackages;
        [
          cardano-cli
          cardano-profile
          cardano-topology
          locli
        ]
      )
      ++
      lib.optional (!pkgs.stdenv.hostPlatform.isDarwin) pkgs.db-analyser
    );

  runWorkbench =
    name: command: # Name of the derivation and `wb` command to run.
    pkgs.runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  # Helper functions.
  ##############################################################################

  runJq =
    name: args: query:
    pkgs.runCommand name {} ''
      args=(${args})
      ${pkgs.jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  # Auxiliary functions of `wb` commands.
  ##############################################################################

  profile-names = __fromJSON (__readFile profile-names-json);

  profile-names-json = runWorkbench "profile-names.json" "profiles list";

# Output
################################################################################

in pkgs.lib.fix (self: {

  inherit cardanoNodePackages;
  inherit workbench' workbench runWorkbench;
  inherit runJq;
  inherit profile-names-json profile-names;

  # Return a profile attr with a `materialise-profile` function.
  # profileName -> profiling -> profile
  profile =
    { profileName
    , profiling
    }:
    (import ./profile/profile.nix
      { inherit pkgs lib;
        workbenchNix = self;
        inherit profileName profiling;
      }
    )
  ;

  # Return a backend attr with a `materialise-profile` function.
  # backendName -> stateDir -> basePort -> useCabalRun -> backend
  backend =
    let backendRegistry = {
        nomadcloud      = params:
          import ./backend/nomad/cloud.nix  params;
        nomadexec       = params:
          import ./backend/nomad/exec.nix   params;
        supervisor      = params:
          import ./backend/supervisor.nix   params;
        };
  in { backendName
     , stateDir
     , basePort
     , useCabalRun
     }:
      # The `useCabalRun` flag is set in the backend to allow the backend to
      # override its value. The runner uses the value of `useCabalRun` from
      # the backend to prevent a runner using a different value.
      (backendRegistry."${backendName}")
        { inherit pkgs lib stateDir basePort useCabalRun; }
  ;

  # A conveniently-parametrisable workbench preset.
  # See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # The general idea is:
  # 1. profileName -> profiling -> profile
  # 2. backendName -> stateDir -> basePort -> useCabalRun -> backend
  # 3. profile -> backend -> batchName -> runner
  runner =
    { profileName
    , profiling
    , backendName
    , stateDir
    , basePort
    , useCabalRun
    , workbenchDevMode
    , batchName
    , workbenchStartArgs
    , cardano-node-rev
    }:
    let
        # Only a name needed to create a profile attrset.
        profile = self.profile { inherit profileName profiling; };
        # The `useCabalRun` flag is set in the backend to allow the backend to
        # override its value. The runner uses the value of `useCabalRun` from
        # the backend to prevent a runner using a different value.
        backend = self.backend
                    { inherit backendName stateDir basePort useCabalRun; }
        ;
    in import ./backend/runner.nix
      {
          inherit pkgs lib;
          inherit profile backend;
          inherit workbench workbenchDevMode cardanoNodePackages;
          inherit batchName workbenchStartArgs;
          inherit cardano-node-rev;
      };
})
