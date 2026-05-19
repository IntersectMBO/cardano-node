{ pkgs
, haskellProject
, stateDir
, basePort # ignored, just passed to the runner (unlike `supervisor.nix`).
## `useCabalRun` overridden parameter (unlike `supervisor.nix`).
, profiling # Checks below for no profiled builds and no info-table support.
, ...
}:
let

  # The exec task driver can run in a cloud environment using SRE's Nomad
  # servers with the "nix_installable" patch and Amazon S3 to distribute the
  # genesis files (Buckets needs write permissions for the deployer machine).
  name = "nomadcloud";

  # Unlike the supervisor backend `useCabalRun` is always `false` here.
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
      # Amazon S3 HTTP to upload/download the genesis tar file.
      pkgs.awscli2
      # Used to download the logs.
      pkgs.rsync
    ]
  ;

  # Nomad-generic "container-specs.json"
  # Build a Nomad Job specification for this Nomad "sub-backend".
  materialise-profile =
    let params = {
      inherit pkgs;
      inherit haskellProject;
      inherit stateDir;
      subBackendName = "cloud";
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
  inherit stateDir;

  # Returns something only to be compatible with what `runner.nix` expects.
  inherit basePort;

  # Ignores the parameters and always returns `false` and `"none"`.
  inherit useCabalRun;
  # Nomad cloud backend clients/nodes get their dependencies from the flake.
  profiling =
         # Being extra cautious, no Makefile target for this combination.
         if profiling.profiledBuild or false
    then throw "Backend \"nomadcloud\" does not support profiled builds."
         # The binaries from the flake outputs won't have info-table.
    else if profiling.infoTable or false
    then throw "Backend \"nomadcloud\" does not support info-table builds."
    else profiling
  ;
}
