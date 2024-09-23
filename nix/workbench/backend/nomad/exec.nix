{ pkgs
, lib
, stateDir
, basePort # Ignored here and just returned to be used by `runner.nix`!
## `useCabalRun` not used here unlike `supervisor.nix`.
, ...
}:
let

  # The exec task driver can run in a local environment were it starts a Nomad
  # Server and Nomad Agents instances and used webfs to distribute the genesis
  # folder.
  name = "nomadexec";

  # Unlike the supervisor backend `useCabalRun` is always false here.
  useCabalRun = false;

  extraShellPkgs =
    [
      # SRE's patched Nomad 1.6.3 that enables `nix_installables` as artifacts.
      (
        # Only x86_64-linux is supported.
        if builtins.currentSystem != "x86_64-linux"
        then builtins.abort "Nomad backends only available for x86_64-linux"
        else (import ./patch.nix {})
      )
      # The HTTP server to upload/download the genesis tar file in a local env.
      pkgs.webfs
    ]
  ;

  # Nomad-generic "container-specs.json"
  # Build a Nomad Job specification for this Nomad "sub-backend".
  materialise-profile =
    let params = {
      inherit pkgs lib stateDir;
      subBackendName = "exec";
    };
    in (import ../nomad.nix params).materialise-profile
  ;

  service-modules = {
    node = { config, ... }:
      let selfCfg = config.services.cardano-node;
          i       = toString selfCfg.nodeId;
      in {
          services.cardano-node.stateDir = stateDir + "/node-${i}";
        }
    ;
  };

in
{
  inherit name;

  inherit extraShellPkgs;
  inherit materialise-profile;
  inherit service-modules;
  inherit stateDir basePort;

  inherit useCabalRun;
}
