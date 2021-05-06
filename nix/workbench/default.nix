{ lib
, stdenv
, pkgs
, graphviz
, jq
, moreutils
, makeWrapper
, runCommand
, customConfig
, cardano-cli
, cardano-topology

, useCabalRun
}:

with lib; with customConfig.localCluster;

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
        wrapProgram "$out/bin/wb" --argv0 wb --add-flags "--set-mode ${nixWbMode}" \
        --prefix PATH ":" ${pkgs.lib.makeBinPath
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
    toString [ "cabal" op "${exe}" "--"];

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
      ${exeCabalOp "exec" "cardano-cli"} "$@"
    }

    function cardano-node() {
      ${exeCabalOp "exec" "cardano-node"} "$@"
    }

    function cardano-topology() {
      ${exeCabalOp "exec" "cardano-topology"} "$@"
    }

    export -f cardano-cli cardano-node cardano-topology

    ''}

    function workbench-prebuild-executables() {
      ${optionalString useCabalRun
        ''
      echo -n "workbench:  prebuilding executables (because of useCabalRun):"
      echo $PWD
      for exe in cardano-cli cardano-node cardano-topology
      do echo -n " $exe"
         cabal -v0 build exe:$exe >/dev/null || return 1
      done
      echo
        ''}
      true
    }
    export -f workbench-prebuild-executables

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
              runWorkbench runJq workbench
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
