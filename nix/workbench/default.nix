{ lib
, stdenv
, pkgs
, git
, graphviz
, jq
, moreutils
, makeWrapper
, runCommand
, customConfig
, cardanoNodePackages

, useCabalRun
}:

with lib; with customConfig.localCluster;

let
  nixWbMode =
    if useCabalRun
    then "cabal-exes+nix-wb"
    else "nix-exes+nix-wb";

  workbench' = tools:
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
        --prefix PATH ":" ${pkgs.lib.makeBinPath tools}
      '';

      installPhase = ''
        mkdir -p         $out/bin
        cp -a wb chain-filters profiles *.sh *.jq $out/bin
      '';

      dontStrip = true;
    };

  workbench = with cardanoNodePackages; workbench'
    [ git graphviz
      jq
      moreutils

      cardano-cli
      cardano-topology
      locli
    ];

  runWorkbench =
    name: command:
    runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runWorkbenchJqOnly =
    name: command:
    runCommand name {} ''
      ${workbench' [jq moreutils]}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    runCommand name {} ''
      args=(${args})
      ${jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  exeCabalOp = op: exe:
    toString [ "cabal" "-v0" op "--" "exe:${exe}"];

  checkoutWbMode =
    if useCabalRun
    then "cabal-exes+checkout-wb"
    else "nix-exes+checkout-wb";

  shellHook = ''
    echo 'workbench shellHook:  workbenchDevMode=${toString workbenchDevMode} useCabalRun=${toString useCabalRun}'
    export WORKBENCH_BACKEND=supervisor

    ${optionalString
      workbenchDevMode
    ''
    export WORKBENCH_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
    export WORKBENCH_EXTRA_FLAGS=

    function wb() {
      $WORKBENCH_CARDANO_NODE_REPO_ROOT/nix/workbench/wb --set-mode ${checkoutWbMode} $WORKBENCH_EXTRA_FLAGS "$@"
    }
    export -f wb
    ''}

    ${optionalString
      useCabalRun
      ''
      . nix/workbench/lib.sh
      . nix/workbench/lib-cabal.sh
      ''}

    export CARDANO_NODE_SOCKET_PATH=run/current/node-0/node.socket
    '';

  generateProfiles =
    { pkgs

    ## The backend is an attrset of AWS/supervisord-specific methods and parameters.
    , backend

    ## Environment arguments:
    ##   - either affect semantics on all backends equally,
    ##   - or have no semantic effect
    , envArgs
    }:
    rec {
      profile-names-json =
        runWorkbenchJqOnly "profile-names.json" "profiles list";

      profile-names =
        __fromJSON (__readFile profile-names-json);

      environment =
        ## IMPORTANT:  keep in sync with envArgs in 'supervisord-cluster/default.nix/envArgs'.
        with envArgs; rec {
          inherit cardanoLib stateDir;

          JSON = runWorkbenchJqOnly "environment.json"
          ''env compute-config \
            --cache-dir "${cacheDir}" \
            --base-port ${toString basePort} \
            ${optionalString staggerPorts "--stagger-ports"} \
          '';
          value = __fromJSON (__readFile JSON);
        };

      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              runWorkbenchJqOnly runJq workbench
              backend
              environment
              profileName;
          };

      profiles = genAttrs profile-names mkProfile;

      profilesJSON =
        runWorkbenchJqOnly "all-profiles.json" "profiles generate-all";
    };

  ## materialise-profile :: ProfileNix -> BackendProfile -> Profile
  materialise-profile      = import ./profile.nix  { inherit pkgs lib; };
  ## profile-topology :: ProfileNix -> Topology
  profile-topology         = import ./topology.nix { inherit pkgs; };
  ## profile-topology :: ProfileNix -> Topology -> Genesis
  profile-topology-genesis = import ./genesis.nix  { inherit pkgs; };

  with-profile =
    { pkgs, backend, envArgs, profileName }:
    let
      ps = generateProfiles
        { inherit pkgs backend envArgs; };

      profileNix = ps.profiles."${profileName}"
        or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames ps.profiles)}");

      profile = materialise-profile
        { inherit profileNix;
          backendProfile =
            backend.materialise-profile { inherit profileNix; };
        };

      topology = profile-topology { inherit profileNix; };

      genesis = profile-topology-genesis { inherit profileNix topology; };
    in {
      inherit
        profileNix profile
        topology
        genesis;
    };

  run-analysis = import ./analyse.nix;
in

{
  inherit workbench runWorkbench runJq with-profile run-analysis;

  inherit generateProfiles shellHook;
}
