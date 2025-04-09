{ pkgs, lib
, cardanoNodeProject
, cardanoNodePackages
}:

with lib;

let

  # Workbench derivation to create derivations from `wb` commands.
  ##############################################################################

  # "Minimal" workbench, manually add the dependencies expected in $PATH.
  # Which tools to add differs if full workbench needed or `wb` commands subset.
  workbench = tools:
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
          --prefix PATH ":" ${makeBinPath tools}
      '';

      installPhase = ''
        mkdir -p                                          $out/bin
        cp    -a wb ede profile                           $out/bin
        for dir in . analyse backend genesis topology
        do
          cp  -a $dir/*                                   $out/bin/$dir
        done
      '';

      dontStrip = true;
    }
  ;

  # Extras.
  ##############################################################################

  profile-names = __fromJSON (__readFile profile-names-json);

  profile-names-json = pkgs.runCommand "profile-names.json" {}
    ''
    ${cardanoNodePackages.cardano-profile}/bin/cardano-profile "names" > $out
    ''
  ;

# Output
################################################################################

in pkgs.lib.fix (self: {

  inherit cardanoNodePackages;
  inherit workbench;
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
          inherit cardanoNodeProject cardanoNodePackages;
          inherit workbench;
          inherit profile backend;
          inherit batchName workbenchStartArgs;
          inherit cardano-node-rev;
      }
  ;
})
