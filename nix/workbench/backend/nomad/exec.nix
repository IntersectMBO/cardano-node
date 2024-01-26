{ pkgs
, lib
, stateDir
, basePort # Ignored here and just returned to be used by `runner.nix`!
## `useCabalRun` not used here unlike `supervisor.nix`.
, ...
}:
let

  # The exec (non podman) task driver can run in a local environment were it
  # starts a Nomad Server and Nomad Agents instances and used webfs to
  # distribute the genesis folder.
  name = "nomadexec";

  # Unlike the supervisor backend `useCabalRun` is always false here.
  useCabalRun = false;

  # The versions of Nomad and the Nomad plugins needed are defined here instead
  # of inside the flake!
  extraShellPkgs = let
    # If we are going to use the `exec` driver we use the SRE patched version of
    # Nomad that allows to use `nix_installables` as artifacts.
    commit = "8f3b74796a8f56f38a812813c64dba995956a66e"; # Patched 1.6.3
    nomad-sre = (__getFlake "github:input-output-hk/cardano-perf/${commit}").packages.x86_64-linux.nomad;
  in
    [ nomad-sre
      # The HTTP server to upload/download the genesis tar file in a local env.
      pkgs.webfs
    ]
  ;

  validateNodeSpecs = { nodeSpecsValue }:
    builtins.all (r: r == "loopback")
      (lib.attrsets.mapAttrsToList
        (name: value: value.region)
        nodeSpecsValue
      )
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

  overlay =
    proTopo: self: super:
    {
    }
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
  inherit validateNodeSpecs materialise-profile;
  inherit overlay service-modules;
  inherit stateDir basePort;

  inherit useCabalRun;
}
