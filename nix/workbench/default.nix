{ pkgs
, lib, jq, runCommand
, cardanoNodePackages
}:

with lib;

let
  workbench' = tools:
    pkgs.stdenv.mkDerivation {
      pname = "workbench";

      version = "0.1";

      src = ./.;

      buildInputs = with pkgs; [ jq makeWrapper ];

      buildPhase = ''
        patchShebangs .
      '';

      postFixup = ''
        wrapProgram "$out/bin/wb" --argv0 wb --prefix PATH ":" ${makeBinPath tools}
      '';

      installPhase = ''
        mkdir -p         $out/bin
        cp -a wb chain-filters profiles *.sh *.jq $out/bin
      '';

      dontStrip = true;
    };

  workbench = with cardanoNodePackages; with pkgs; workbench'
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
      ${workbench' (with pkgs; [jq moreutils])}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    runCommand name {} ''
      args=(${args})
      ${jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  profile-names-json =
    runWorkbenchJqOnly "profile-names.json" "profiles list";

  profile-names =
    __fromJSON (__readFile profile-names-json);

  all-profiles =
    ## The backend is an attrset of AWS/supervisord-specific methods and parameters.
    { backend

    ## Environment arguments:
    ##   - either affect semantics on all backends equally,
    ##   - or have no semantic effect
    , envArgs
    }:
    rec {
      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              runWorkbenchJqOnly runJq workbench
              backend
              profileName;
          };

      value = genAttrs profile-names mkProfile;

      JSON = pkgs.writeText "all-profiles.json" (__toJSON (mapAttrs (_: x: x.value) value));
    };

  ## materialise-profile :: ProfileNix -> BackendProfile -> Profile
  materialise-profile      = import ./profile.nix  { inherit pkgs lib; };
  ## profile-topology :: ProfileNix -> Topology
  profile-topology         = import ./topology.nix { inherit pkgs; };
  ## profile-topology :: ProfileNix -> Topology -> Genesis
  profile-topology-genesis = import ./genesis.nix  { inherit pkgs; };

  with-profile =
    { backend, envArgs, profileName }:
    let
      ps = all-profiles { inherit backend envArgs; };

      profileNix = ps.value."${profileName}"
        or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames ps.value)}");

      profile = materialise-profile
        { inherit profileNix workbench;
          backendProfile =
            backend.materialise-profile { inherit profileNix; };
        };

      topology = profile-topology { inherit profileNix profile; };

      genesis = profile-topology-genesis { inherit profileNix profile topology; };
    in {
      inherit
        profileNix profile
        topology
        genesis;
    };

  run-analysis = import ./analyse.nix;

in {
  inherit runJq;

  inherit workbench' workbench runWorkbench runWorkbenchJqOnly;

  inherit all-profiles profile-names profile-names-json with-profile;

  inherit run-analysis;
}
