self: {
  profiled   = false;
  withHoogle = true;
  localCluster = {
    cacheDir    = "${self.localCluster.stateDir}/.cache";
    stateDir    = "run/current";
    batchName   = "plain";
    profileName = "default-bage";
    backendName = "supervisor";
    basePort    = 30000;
    workbenchDevMode = true;
    extraBackendConfig = {};
    useCabalRun = true;
  };
  membench = {
    snapshotSlot = 37173650;
    finalChunkNo = 1800;
    rtsMemSize = null;
    rtsflags = "";
    limit2 = "6553M";
    variantTable = {
      baseline = { legacyTracing = false; };
    };
    shelleyGenesisHash = "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81";
  };
  # optional extra haskell.nix module
  haskellNix = {};
}
