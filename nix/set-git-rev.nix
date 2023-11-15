{ pkgs }:
drv:
with pkgs;
buildPackages.runCommand drv.name
{
  inherit (drv) exeName exePath meta passthru;
  # this is to ensure we are re-signing macOS binaries that might have been
  # mutilated by set-git-rev (e.g. patching in the git revision.)
  nativeBuildInputs = lib.optionals hostPlatform.isDarwin [ darwin.signingUtils ];
} (''
  mkdir -p $out
  cp --no-preserve=timestamps --recursive ${drv}/* $out/
  chmod -R +w $out/bin
  ${pkgsBuildBuild.haskellBuildUtils}/bin/set-git-rev "${gitrev}" $out/bin/*
'' + lib.optionalString hostPlatform.isDarwin ''
  for exe in $out/bin/*; do
    signIfRequired "$exe"
  done
'')
