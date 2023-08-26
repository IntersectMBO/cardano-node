{ pkgs
, lib
, stateDir
, basePort # Ignored here and just returned to be used by `runner.nix`!
## `useCabalRun` not used here unlike `supervisor.nix`.
, ...
}:
let

  # The podman (non exec) task driver can only run in a local environment
  # because we mount the genesis and mainnet mirror local folders, it's the
  # simplest Nomad testing environment and doesn't need root privileges or
  # Amazon S3 to run!
  name = "nomadpodman";

  # Unlike the supervisor backend `useCabalRun` is always false here.
  useCabalRun = false;

  # The versions of Nomad and the Nomad plugins needed are defined here instead
  # of inside the flake!
  extraShellPkgs = let
    nomad = (pkgs.buildGo119Module rec {
      pname = "nomad";
      version = "1.4.3"; # Both Nomad versions are using 1.4.3
      subPackages = [ "." ];
      doCheck = true;
      src = pkgs.fetchFromGitHub {
        owner = "hashicorp";
        repo = pname;
        rev = "v${version}";
        # nix-prefetch-url --unpack https://github.com/hashicorp/nomad/archive/v1.4.3.tar.gz
        sha256 = "0j2ik501sg6diyabwwfrqnz1wxx485w5pxry4bfkg5smgyp5y18r";
      };
      # error: either `vendorHash` or `vendorSha256` is required
      # https://discourse.nixos.org/t/buildgomodule-how-to-get-vendorsha256/9317
      vendorSha256 = "sha256-JQRpsQhq5r/QcgFwtnptmvnjBEhdCFrXFrTKkJioL3A=";
    });
    # This plugin is defined but only used if `execTaskDriver` is false.
    nomad-driver-podman = (pkgs.buildGo119Module rec {
      pname = "nomad-driver-podman";
      version = "0.4.2";
      subPackages = [ "." ];
      doCheck = false; # some tests require a running podman service to pass
      src = pkgs.fetchFromGitHub {
        owner = "hashicorp";
        repo = pname;
        rev = "v${version}";
        # nix-prefetch-url --unpack https://github.com/hashicorp/nomad-driver-podman/archive/v0.4.2.tar.gz
        # sha256 = "03856ws02xkqg5374x35zzz5900456rvpsridsjgwvvyqnysn9ls";
        sha256 = "1wz3pqv0ib52g378sgq3ahhp3p4fm6xm41c53lliavrxcy6msy58";
      };
      # error: either `vendorHash` or `vendorSha256` is required
      # https://discourse.nixos.org/t/buildgomodule-how-to-get-vendorsha256/9317
      vendorSha256 = "sha256-UIUavFdBuSiaUsNaibPjRMURMLLK5UjNHVoyNSIRNQ4=";
    });
  in
    [
      nomad pkgs.podman nomad-driver-podman
      # Network tools to be able to use bridge networking and the HTTP server
      # to upload/download the genesis tar file.
      pkgs.cni-plugins
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
      subBackendName = "podman";
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
