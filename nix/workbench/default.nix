{ pkgs
, lib
, cardanoNodePackages
, cardanoNodeProject
}:

with lib;

let
  # recover CHaP location from cardano's project
  chap = cardanoNodeProject.args.inputMap."https://chap.intersectmbo.org/";
  # build plan as computed by nix
  nixPlanJson = cardanoNodeProject.plan-nix.json;

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

  workbench = with cardanoNodePackages; with pkgs; workbench' (
    [ git graphviz
      jq
      moreutils
      procps
      cardano-cli
      cardano-profile
      cardano-topology
    ] ++ lib.optional (!pkgs.stdenv.hostPlatform.isDarwin) db-analyser
      ++ [ locli ]
    );

  runWorkbench =
    name: command:
    pkgs.runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    pkgs.runCommand name {} ''
      args=(${args})
      ${pkgs.jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

in {
  inherit workbench' workbench runWorkbench;
  inherit runJq;

  inherit
    (import ./profile/profile.nix
      {inherit pkgs lib runJq runWorkbench; inherit (cardanoNodePackages) cardanoLib;}
    )
    profile-names-json
    profile-names
    materialise-profile;
}
