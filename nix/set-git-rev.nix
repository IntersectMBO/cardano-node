{ pkgs }:
drv:
pkgs.buildPackages.runCommand drv.name
{
  inherit (drv) exeName exePath meta passthru;
} ''
  mkdir -p $out
  cp --no-preserve=timestamps --recursive ${drv}/* $out/
  chmod -R +w $out/bin
  ${pkgs.pkgsBuildBuild.haskellBuildUtils}/bin/set-git-rev "${pkgs.gitrev}" $out/bin/*
''
