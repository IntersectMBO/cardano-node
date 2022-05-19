let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
  # This file is used by nix-shell.
  # It just takes the shell attribute from default.nix.
in
{
  # override scripts with custom configuration
  withHoogle ? defaultCustomConfig.withHoogle
, profileName ? defaultCustomConfig.localCluster.profileName
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, customConfig ? {
    inherit withHoogle;
    localCluster = {
      inherit profileName workbenchDevMode;
    };
  }
}:
with (import ./nix/flake-compat.nix customConfig);
defaultNix // defaultNix.packages.${builtins.currentSystem} // {
  private.project = defaultNix.legacyPackages.${builtins.currentSystem};
}
