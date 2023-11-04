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

  validateNodeSpecs = { nodeSpecsValue }:
    let
      # There's a region mismatch between the workbench (specifically Haskell
      # code in cardano-topology) and Cardano World's Nomad cluster that both
      # use "us-east-2" while our dedicated Nomad cluster is using "us-east-1",
      # what SRE deployed.
      # - Cardano World cluster: "eu-central-1", "us-east-2"
      # - Workbench (Nix level): "eu-central-1", "us-east-2", and "ap-southeast-2"
      # - Dedicated P&T cluster: "eu-central-1", "us-east-1", and "ap-southeast-2"
      datacenters = [ "eu-central-1" "us-east-1" "us-east-2" "ap-southeast-2" ];
      regions = lib.attrsets.mapAttrsToList
        (name: value: value.region)
        nodeSpecsValue
      ;
    in if builtins.all (r: builtins.elem r datacenters) regions
       then true
       else builtins.throw (
         "The only compatible regions for Nomad cloud are \"${toString datacenters}\" but found \"${toString regions}\""
       )
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
  inherit validateNodeSpecs materialise-profile;
  inherit overlay service-modules;
  inherit stateDir basePort;

  inherit useCabalRun;
}
