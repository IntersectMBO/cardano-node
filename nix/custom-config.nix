self: {
  profiling  = "none";
  withHoogle = true;
  localCluster = {
    cacheDir    = "${self.localCluster.stateDir}/.cache";
    stateDir    = "run/current";
    batchName   = "undefined";
    profileName = "default-coay";
    backendName = "supervisor";
    basePort    = 30000;
    workbenchDevMode = true;
    workbenchStartArgs = [];
    extraBackendConfig = {};
    useCabalRun = true;
  };
  # optional extra haskell.nix module
  haskellNix = {};
}
