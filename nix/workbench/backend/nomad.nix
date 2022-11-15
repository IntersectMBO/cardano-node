let
  basePort              = 30000;
  cacheDirDefault       = "${__getEnv "HOME"}/.cache/cardano-workbench";
  stateDir              = "run/current";
in
{ pkgs
, lib, nix2container
, workbench
##
, cacheDir              ? cacheDirDefault
, extraBackendConfig    ? {}
## `useCabalRun` not used here like in `supervisor.nix`.
, enableEKG             ? true
##
, ...
}:
let
  backend =
    rec
    { name = "nomad";

      # Unlike the supervisor backend `useCabalRun` is always false here.
      useCabalRun = false;

      services-config = import ./services-config.nix {inherit lib workbench basePort stateDir; useCabalRun = false; inherit enableEKG;};

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
        { profileNix }:
          let
            supervisorConfPath =
              import ./supervisor-conf.nix
                { inherit (profileNix) node-services;
                  inherit
                    pkgs lib stateDir
                    basePort
                    extraBackendConfig;
                  unixHttpServerPort = "/tmp/supervisor.sock";
                };
            nomadConf =
              import ./nomad-conf.nix
                { inherit pkgs;
                  inherit
                    (pkgs.cardanoNodePackages)
                    cardano-node cardano-tracer tx-generator;
                  inherit nix2container;
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
    };
in
{
  inherit cacheDir stateDir basePort;
  inherit workbench;
  inherit backend;
}
