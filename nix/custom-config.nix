self: {
  withHoogle = true;
  localCluster = {
    cacheDir    = "${self.localCluster.stateDir}/.cache";
    stateDir    = "run/current";
    profileName = "default-alzo";
    basePort    = 30000;
    autoStartCluster = false;
    autoStartClusterArgs = "";
    enableEKG        = true;
    workbenchDevMode = false;
    extraSupervisorConfig = {};
  };
  # optional extra haskell.nix module
  haskellNix = {};
}
