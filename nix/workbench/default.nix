{ pkgs
, lib, jq, runCommand
, db-analyser
, cardanoNodePackages
}:

with lib;

let
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
        wrapProgram "$out/bin/wb" --argv0 wb --prefix PATH ":" ${makeBinPath tools}
      '';

      installPhase = ''
        mkdir -p                                     $out/bin
        cp    -a wb chain-filters profiles *.sh *.jq $out/bin
        mkdir -p                                     $out/bin/backend
        cp    -a backend/*.sh                        $out/bin/backend
      '';

      dontStrip = true;
    };

  workbench = with cardanoNodePackages; with pkgs; workbench' (
    [ git graphviz
      jq
      moreutils
      procps
      cardano-cli
      cardano-topology
    ] ++ lib.optional (!pkgs.stdenv.hostPlatform.isDarwin) db-analyser
      ++ [ locli ]
    );

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
    { backend }:
    rec {
      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              runWorkbenchJqOnly runJq workbench
              profileName;
            inherit (backend) services-config;
          };

      value = genAttrs profile-names mkProfile;

      JSON = pkgs.writeText "all-profiles.json" (__toJSON (mapAttrs (_: x: x.value) value));
    };

  with-profile =
    { backend, profileName }:
    let
      ps = all-profiles { inherit backend; };

      profileNix = ps.value."${profileName}"
        or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames ps.value)}");

      profile = import ./profile.nix   { inherit pkgs lib profileNix backend; };

      topology = import ./topology.nix { inherit pkgs profileNix profile; };

      genesis = import ./genesis.nix   { inherit pkgs profile; };
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
