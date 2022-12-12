{ pkgs
, lib
## `useCabalRun` not used here like in `supervisor.nix`.
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

  materialise-profile =
    { stateDir, profileNix }:
      let
        unixHttpServerPort = "/tmp/supervisor.sock";
        supervisorConfPath =
          import ./supervisor-conf.nix
            { inherit (profileNix) node-services;
              inherit pkgs lib stateDir;
              inherit unixHttpServerPort;
            };
        ociImages =
          import ./oci-images.nix
            { inherit pkgs;
              inherit
                (pkgs.cardanoNodePackages)
                cardano-node cardano-tracer tx-generator;
            };
        nomadJobJSONPath =
          import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileNix;
              inherit (ociImages) clusterImage;
              inherit unixHttpServerPort;
            };
      in pkgs.runCommand "workbench-backend-output-${profileNix.name}-${name}"
        (rec {
          inherit supervisorConfPath;
          # All In One
          clusterImage = ociImages.clusterImage;
          clusterImageCopyToPodman = clusterImage.copyToPodman;
          clusterImageName = clusterImage.imageName;
          clusterImageTag = clusterImage.imageTag;
          inherit nomadJobJSONPath;
        })
        ''
        mkdir $out

        ln -s $supervisorConfPath                      $out/supervisor.conf

        ln -s $clusterImage                            $out/clusterImage
        echo $clusterImageName                       > $out/clusterImageName
        echo $clusterImageTag                        > $out/clusterImageTag
        ln -s $clusterImageCopyToPodman/bin/copy-to-podman $out/clusterImageCopyToPodman

        ln -s $nomadJobJSONPath                        $out/nomad-job.json
        '';
in
{
  inherit name useCabalRun extraShellPkgs materialise-profile;
}
