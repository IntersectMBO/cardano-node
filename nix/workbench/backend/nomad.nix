{ pkgs
, lib
## `useCabalRun` not used here like in `supervisor.nix`.
, ...
}:
let
  name = "nomad";

  # Unlike the supervisor backend `useCabalRun` is always false here.
  useCabalRun = false;

  extraShellPkgs = with pkgs; [
    # https://docs.podman.io/en/latest/markdown/podman.1.html#rootless-mode
    podman
    # Was not needed even thou it says so!
    # https://docs.podman.io/en/latest/markdown/podman.1.html#note-unsupported-file-systems-in-rootless-mode
    # fuse-overlayfs
    nomad
    nomad-driver-podman
  ];

  materialise-profile =
    { stateDir, profileNix }:
      let
        supervisorConfPath =
          import ./supervisor-conf.nix
            { inherit (profileNix) node-services;
              inherit pkgs lib stateDir;
              unixHttpServerPort = "/tmp/supervisor.sock";
            };
        nomadConf =
          import ./nomad-conf.nix
            { inherit pkgs;
              inherit
                (pkgs.cardanoNodePackages)
                cardano-node cardano-tracer tx-generator;
            };
      in pkgs.runCommand "workbench-backend-output-${profileNix.name}-${name}"
        (rec {
          inherit supervisorConfPath;
          # All In One
          clusterImage = nomadConf.clusterImage;
          clusterImageCopyToPodman = clusterImage.copyToPodman;
          clusterImageName = clusterImage.imageName;
          clusterImageTag = clusterImage.imageTag;
        })
        ''
        mkdir $out

        ln -s $supervisorConfPath                      $out/supervisor.conf

        ln -s $clusterImage                            $out/clusterImage
        echo $clusterImageName                       > $out/clusterImageName
        echo $clusterImageTag                        > $out/clusterImageTag
        ln -s $clusterImageCopyToPodman/bin/copy-to-podman $out/clusterImageCopyToPodman
        '';
in
{
  inherit name useCabalRun extraShellPkgs materialise-profile;
}
