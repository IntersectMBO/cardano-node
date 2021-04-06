{ lib
, stdenv

, cabal-install
, graphviz
, jq
, moreutils
, makeWrapper
, runCommand

## Default to pure Nix-iness:
, useCabalRun ? false
, workbenchDevMode ? false

, cardano-cli
, cardano-topology
}:

with lib;

let
  workbench =
    stdenv.mkDerivation {
      pname = "workbench";

      version = "0.1";

      src = ./.;

      buildInputs = [ jq makeWrapper ];

      buildPhase = ''
        patchShebangs .
      '';

      postFixup = ''
        wrapProgram "$out/bin/wb" --prefix PATH ":" ${stdenv.lib.makeBinPath
          [ graphviz
            jq
            moreutils

            cardano-cli
            cardano-topology
          ]}
      '';

      installPhase = ''
        mkdir -p         $out/bin
        cp -a wb profiles *.sh *.jq $out/bin
      '';

      dontStrip = true;
    };

  runWorkbench =
    name: command:
    runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    runCommand name {} ''
      args=(${args})
      ${jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  exeCabalRunner = exe:
    toString [ "${cabal-install}/bin/cabal" "-v0" "run" "exe:${exe}" "--"];

  shellHook = ''
    ${optionalString workbenchDevMode
    ''
    echo 'workbench:  dev mode enabled, calling wb directly from checkout (instead of using Nix store)' >&2

    workbench_cardano_node_repo_root=$(git rev-parse --show-toplevel)
    workbench_extra_flags=

    function wb() {
      $workbench_cardano_node_repo_root/nix/workbench/wb $workbench_extra_flags "$@"
    }

    export workbench_cardano_node_repo_root workbench_extra_flags
    export -f wb

    ''}

    ${optionalString useCabalRun
    ''
    echo 'workbench:  cabal-inside-nix-shell mode enabled, calling cardano-* via 'cabal run' (instead of using Nix store)' >&2

    function cardano-cli() {
      ${exeCabalRunner "cardano-cli"} "$@"
    }

    function cardano-node() {
      ${exeCabalRunner "cardano-node"} "$@"
    }

    function cardano-topology() {
      ${exeCabalRunner "cardano-topology"} "$@"
    }

    export -f cardano-cli cardano-node cardano-topology

    ''}
    '';

  generateProfiles =
    { pkgs

    ## The backend is an attrset of AWS/supervisord-specific methods and parameters.
    , backend

    ## Environmental settings:
    ##   - either affect semantics on all backends equally,
    ##   - or have no semantic effect
    , environment
    }:
    rec {
      profile-names-json =
        runWorkbench "profile-names.json" "profiles list";

      profile-names =
        __fromJSON (__readFile profile-names-json);

      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              backend
              environment
              profileName;
          };

      profiles = genAttrs profile-names mkProfile;
    };
in
{
  inherit workbench runWorkbench runJq;

  inherit generateProfiles shellHook;
}
