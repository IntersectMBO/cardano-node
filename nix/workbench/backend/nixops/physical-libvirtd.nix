let
  ## IFD:
  profile = __fromJSON (__readFile <profileJson>);
  nix = import ../../..
    { localCluster = {
        profileName = profile.name;
        backendName = "nixops";
        useCabalRun = false;
        workbenchDevMode = true;
      };
      withHoogle = false;
    };
in
with nix;

import ./deployment.nix {
  instances = with iohk-ops-lib.physical.libvirtd; {
    inherit targetEnv;
    core-node   = medium;
    relay-node  = large;
    test-node   = large;
    smash       = medium;
    faucet      = medium;
    metadata    = medium;
    explorer    = medium;
    explorer-gw = small;
    monitoring  = medium;
  };
  inherit (nix) pkgs;
} // lib.optionalAttrs (builtins.getEnv "BUILD_ONLY" == "true") {
  defaults = {
    users.users.root.openssh.authorizedKeys.keys = lib.mkForce [""];
  };
}
