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
        mkdir -p                                                    $out/bin
        cp    -a wb ede profile                                     $out/bin
        for dir in . analyse backend genesis topology
        do cp    -a $dir/*                                          $out/bin/$dir
        done
      '';

      dontStrip = true;
    };

  workbench = with cardanoNodePackages; with pkgs; workbench' (
    [ git graphviz
      jq
      moreutils
      procps
      cabalWrapped
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

  jsonFilePretty = name: x: runJq name ''--null-input --sort-keys
                                         --argjson x '${x}'
                                       '' "$x";

  run-analysis = import ./analyse/analyse.nix;

in {
  inherit workbench' workbench runWorkbench runWorkbenchJqOnly;
  inherit runJq jsonFilePretty;

  inherit run-analysis;

  inherit
    (pkgs.callPackage ./profile/profile.nix
      {
        inherit runJq jsonFilePretty runWorkbenchJqOnly runWorkbench;
      })
    profileJson
    topologyFiles
    nodeSpecsJson
    genesisFiles
    services
    profile
    profileData

    profile-names-json
    profile-names

    materialise-profile;
}
