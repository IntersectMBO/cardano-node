{ pkgs
, lib
, stateDir
, basePort # Ignored here and just returned to be used by `runner.nix`!
## `useCabalRun` not used here unlike `supervisor.nix`.
, ...
}:
let

  # The exec (non podman) task driver can run in a cloud environment using SRE's
  # Nomad servers with the "nix_installable" patch and Amazon S3 to distribute
  # ther genesis files. All credentials are obtained using Vault.
  name = "nomadcloud";

  # Unlike the supervisor backend `useCabalRun` is always false here.
  useCabalRun = false;

  # No override of the Nomad and Vault binaries in "workbench/shell.nix"
  extraShellPkgs =
    [
      # Amazon S3 HTTP to upload/download the genesis tar file.
      pkgs.awscli
    ]
  ;

  # Nomad-generic "container-specs.json"
  # Build a Nomad Job specification for this Nomad "sub-backend".
  materialise-profile =
    let params = {
      inherit pkgs lib stateDir;
      subBackendName = "cloud";
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
  inherit materialise-profile;
  inherit overlay service-modules;
  inherit stateDir basePort;

  inherit useCabalRun;
}
