let defaultCustomConfig = import ./nix/custom-config.nix defaultCustomConfig;
  # This file is used by nix-shell.
  # It just takes the shell attribute from default.nix.
in
{
  # override scripts with custom configuration
  withHoogle ? defaultCustomConfig.withHoogle
, profileData ? null
, profileName ? if profileData != null then profileData.profileName
                else defaultCustomConfig.localCluster.profileName
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, workbenchStartArgs ? defaultCustomConfig.localCluster.workbenchStartArgs
, customConfig ? {
    inherit withHoogle;
    localCluster = {
      inherit profileName workbenchDevMode workbenchStartArgs;
    };
  }
, system ? builtins.currentSystem
}:
with (import ./nix/flake-compat.nix customConfig);
defaultNix // defaultNix.packages.${system} // {
  private.project = defaultNix.legacyPackages.${system};
}
