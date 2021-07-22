self: {
  withHoogle = true;
  withR = false;
  localCluster = {
    cacheDir    = "${self.localCluster.stateDir}/.cache";
    stateDir    = "state-cluster";
    profileName = "default-alzo";
    basePort    = 30000;
    autoStartCluster = false;
    enableEKG        = true;
    workbenchDevMode = false;
    extraSupervisorConfig = {};
  };
}
