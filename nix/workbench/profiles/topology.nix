{ lib
, runCommand
, workbench
, profile
}:

let
  files =
    runCommand "topology-${profile.name}" {} ''
      ${workbench.workbench}/bin/wb topology make ${profile.JSON} $out
    '';
in
{
  inherit files;
}
