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
  nixWbMode =
    if useCabalRun
    then "cabal-exes+nix-wb"
    else "nix-exes+nix-wb";

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
        wrapProgram "$out/bin/wb" --argv0 wb --add-flags "--set-mode ${nixWbMode}" --prefix PATH ":" ${stdenv.lib.makeBinPath
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

  exeCabalOp = op: exe:
    toString [ "${cabal-install}/bin/cabal" "-v0" op "exe:${exe}" "--"];

  checkoutWbMode =
    if useCabalRun
    then "cabal-exes+checkout-wb"
    else "nix-exes+checkout-wb";

  shellHook = ''
    ${optionalString workbenchDevMode
    ''
    echo 'workbench:  dev mode enabled, calling wb directly from checkout (instead of using Nix store)' >&2

    workbench_cardano_node_repo_root=$(git rev-parse --show-toplevel)
    workbench_extra_flags=

    function wb() {
      $workbench_cardano_node_repo_root/nix/workbench/wb --set-mode ${checkoutWbMode} $workbench_extra_flags "$@"
    }

    export workbench_cardano_node_repo_root workbench_extra_flags
    export -f wb

    ''}

    ${optionalString useCabalRun
    ''
    echo 'workbench:  cabal-inside-nix-shell mode enabled, calling cardano-* via 'cabal run' (instead of using Nix store)' >&2

    function cardano-cli() {
      ${exeCabalOp "run" "cardano-cli"} "$@"
    }

    function cardano-node() {
      ${exeCabalOp "run" "cardano-node"} "$@"
    }

    function cardano-topology() {
      ${exeCabalOp "run" "cardano-topology"} "$@"
    }

    function prebuild-executables() {
      echo -n "workbench:  prebuilding executables:"
      for exe in cardano-cli cardano-node cardano-topology
      do echo -n " $exe"
         $exe --help >/dev/null || true
      done
      echo
    }

    export -f cardano-cli cardano-node cardano-topology prebuild-executables

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

      profilesJSON =
        runWorkbench "all-profiles.json" "profiles generate-all";
    };
in
{
  inherit workbench runWorkbench runJq;

  inherit generateProfiles shellHook;
}
