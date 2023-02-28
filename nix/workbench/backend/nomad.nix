{ pkgs
, lib
, stateDir
, basePort
## `useCabalRun` not used here unlike `supervisor.nix`.
# TODO: Fetch this from config services inside materialise-profile !
, eventlogged ? true
# The exec driver uses SRE's plugin that allows Nix derivation artifacts!
, execTaskDriver
, ...
}:
let

  name = "nomad";

  # Unlike the supervisor backend `useCabalRun` is always false here.
  useCabalRun = false;

  extraShellPkgs =
    let
      nomad = (pkgs.buildGo119Module rec {
        pname = "nomad";
        version = "1.4.3";
        subPackages = [ "." ];
        doCheck = true;
        src = pkgs.fetchFromGitHub {
          # fetchFromGit:
          # - url = "https://github.com/hashicorp/${pname}.git";
          # - rev = "f464aca721d222ae9c1f3df643b3c3aaa20e2da7";
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
      nomad-driver-podman = (pkgs.buildGo119Module rec {
        pname = "nomad-driver-podman";
        version = "0.4.1";
        subPackages = [ "." ];
        doCheck = false; # some tests require a running podman service
        src = pkgs.fetchFromGitHub {
          # fetchFromGit:
          # - url = "https://github.com/hashicorp/${pname}.git";
          # - rev = "3f8a8c03d26afe73388546b6235152224bafd6c1";
          owner = "hashicorp";
          repo = pname;
          rev = "v${version}";
          # nix-prefetch-url --unpack https://github.com/hashicorp/nomad-driver-podman/archive/v0.4.1.tar.gz
          sha256 = "03856ws02xkqg5374x35zzz5900456rvpsridsjgwvvyqnysn9ls";
        };
        # error: either `vendorHash` or `vendorSha256` is required
        # https://discourse.nixos.org/t/buildgomodule-how-to-get-vendorsha256/9317
        vendorSha256 = "sha256-AtgxHAkNzzjMQoSqROpuNoSDum/6JR+mLpcHLFL9EIY=";
      });
    in
      [
        nomad
        nomad-driver-podman
      ]
      ++
      (with pkgs; [
        # https://docs.podman.io/en/latest/markdown/podman.1.html#rootless-mode
        podman
        # Was not needed even thou it says so!
        # https://docs.podman.io/en/latest/markdown/podman.1.html#note-unsupported-file-systems-in-rootless-mode
        # fuse-overlayfs
      ]);

  # Backend-specific Nix bits:
  materialise-profile =
    { profileNix }:
      let
        ociImages =
          import ./oci-images.nix
            { inherit pkgs lib;
              inherit
                (pkgs.cardanoNodePackages)
                cardano-node cardano-tracer tx-generator;

              cardano-node-eventlogged = pkgs.cardanoNodePackages.cardano-node.passthru.eventlogged;
            };
        supervisorConf =
          import ./supervisor-conf.nix
            { inherit profileNix;
              inherit pkgs lib stateDir;
              unixHttpServerPort = "/tmp/supervisor.sock";
            };
        nomadJobJSON =
          import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileNix;
              inherit ociImages;
              inherit supervisorConf;
              # Actually always "false", may evolve to a "cloud" flag!
              oneTracerPerNode = false;
            };
      in pkgs.runCommand "workbench-backend-output-${profileNix.profileName}-nomad"
        ({
          ociImagesJSON = ociImages.JSON;
          inherit nomadJobJSON;
        })
        ''
        mkdir $out
        ln -s $ociImagesJSON                           $out/oci-images.json
        ln -s $nomadJobJSON                            $out/nomad-job.json
        '';

  overlay =
    proTopo: self: super:
    {
    };

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
  name = "nomad";

  inherit extraShellPkgs materialise-profile overlay stateDir basePort useCabalRun service-modules;
}
