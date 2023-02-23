let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
  # This file is used by nix-shell.
  # It just takes the shell attribute from default.nix.
in
{
  # override scripts with custom configuration
  withHoogle ? defaultCustomConfig.withHoogle
, profileNix ? null
, profileName ? if profileNix != null then profileNix.profileName
                else defaultCustomConfig.localCluster.profileName;
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, customConfig ? {
    inherit withHoogle;
    localCluster = {
      inherit profileName workbenchDevMode;
    };
  }
, system ? builtins.currentSystem
}:
with (import ./nix/flake-compat.nix customConfig);
defaultNix // defaultNix.packages.${system} // {
  private.project = defaultNix.legacyPackages.${system};
}
