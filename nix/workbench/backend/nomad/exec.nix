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
    nomad-sre = (pkgs.buildGo119Module rec {
      pname = "nomad";
      version = "1.4.3";
      subPackages = [ "." ];
      doCheck = true;
      src = pkgs.fetchFromGitHub { # "github:input-output-hk/nomad/release/1.4.3"
        owner = "input-output-hk";
        repo = pname;
        rev = "2b8a93390"; # Use to be "release/${version}" but it changes.
        # nix-prefetch-url --unpack https://github.com/input-output-hk/nomad/archive/2b8a93390/1.4.3.tar.gz
        sha256 = "0l2sfhpg0p5mjdbipib7q63wlsrczr2fkq9xi641vhgxsjmprvwm";
      };
      # error: either `vendorHash` or `vendorSha256` is required
      # https://discourse.nixos.org/t/buildgomodule-how-to-get-vendorsha256/9317
      vendorSha256 = "sha256-JQRpsQhq5r/QcgFwtnptmvnjBEhdCFrXFrTKkJioL3A=";
    });
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
  # Build a Nomad Job specification for each available Nomad "sub-backend".
  materialise-profile =
    (import ../nomad.nix {inherit pkgs lib stateDir;}).materialise-profile
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
