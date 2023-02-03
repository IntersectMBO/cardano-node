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
    # Both Nomad source versions are using 1.4.3 but this one returns:
    # > nomad --version
    # Nomad v1.4.4-dev
    nomad-sre = (pkgs.buildGo119Module rec {
      pname = "nomad";
      version = "1.4.3";
      subPackages = [ "." ];
      doCheck = true;
      src = pkgs.fetchFromGitHub { # "github:input-output-hk/nomad/release/1.4.3"
        owner = "input-output-hk";
        repo = pname;
        rev = "release/${version}";
        # nix-prefetch-url --unpack https://github.com/input-output-hk/nomad/archive/release/1.4.3.tar.gz
        sha256 = "0z4bdx0nx6460q9r032l8ghx337h3fpi6cx8wh7i3qllpyi51a2k";
      };
      # error: either `vendorHash` or `vendorSha256` is required
      # https://discourse.nixos.org/t/buildgomodule-how-to-get-vendorsha256/9317
      vendorSha256 = "sha256-JQRpsQhq5r/QcgFwtnptmvnjBEhdCFrXFrTKkJioL3A=";
    });
    # This plugin is defined but only used if `execTaskDriver` is false.
    nomad-driver-podman = (pkgs.buildGo119Module rec {
      pname = "nomad-driver-podman";
      version = "0.4.1";
      subPackages = [ "." ];
      doCheck = false; # some tests require a running podman service to pass
      src = pkgs.fetchFromGitHub {
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
    (if !execTaskDriver
      then [ nomad pkgs.podman nomad-driver-podman ]
      # If we are going to use the `exec` driver we use the SRE patched
      # version of Nomad that allows to use `nix_installables` as artifacts.
      else [ nomad-sre ]
    )
    ++
    # Network tools to be able to use bridge networking and the HTTP server
    # to upload/download the genesis tar file.
    [ pkgs.cni-plugins pkgs.webfs ]
  ;

  # Backend-specific Nix bits:
  materialise-profile =
    { profileNix }:
      let
        supervisorConf = import ./supervisor-conf.nix
          { inherit profileNix;
            inherit pkgs lib stateDir;
            unixHttpServerPort = if execTaskDriver
              # ''{{ env "NOMAD_TASK_DIR" }}/supervisor.sock''
              then "/tmp/supervisor.sock" # TODO: Is this OK?
              else "/tmp/supervisor.sock"
            ;
          }
        ;
        # Intermediate / workbench-adhoc container specifications
        containerSpecs = rec {
          #
          diretories = rec {
            work = "/local";
            state = stateDir;
            run = lib.strings.concatStringsSep "/" [ work state ];
          };
          # The actual commit.
          gitrev = pkgs.gitrev;
          # Binaries.
          containerPkgs = {
            bashInteractive = rec {
              nix-store-path  = pkgs.bashInteractive;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.bashInteractive";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            coreutils = rec {
              nix-store-path  = pkgs.coreutils;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.coreutils";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            findutils = rec {
              nix-store-path  = pkgs.findutils;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.findutils";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            gnutar = rec {
              nix-store-path  = pkgs.gnutar;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.gnutar";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            zstd = rec {
              nix-store-path  = pkgs.zstd;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.gnutar";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            supervisor = rec {
              nix-store-path  = pkgs.python3Packages.supervisor;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.python3Packages.supervisor";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            # TODO: profileNix.node-services."node-0".serviceConfig.value.eventlog
            # builtins.trace (builtins.attrNames profileNix.node-services."node-0".serviceConfig.value.eventlog) XXXX
            cardano-node = rec {
              nix-store-path  = with pkgs;
                if eventlogged
                  then cardanoNodePackages.cardano-node.passthru.eventlogged
                  else cardanoNodePackages.cardano-node
              ;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output =
                if eventlogged
                  then "cardanoNodePackages.cardano-node.passthru.eventlogged"
                  else "cardanoNodePackages.cardano-node"
              ;
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            cardano-tracer = rec {
              nix-store-path  = pkgs.cardanoNodePackages.cardano-tracer;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "cardanoNodePackages.cardano-tracer";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            tx-generator = rec {
              nix-store-path  = pkgs.cardanoNodePackages.tx-generator;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "cardanoNodePackages.tx-generator";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
          };
          supervisord = {
            url = "unix:///tmp/supervisor.sock";
            conf = lib.strings.concatStringsSep "/"
              [ diretories.run "supervisor" "supervisord.conf"]
            ;
          };
          ociImage = import ./oci-images.nix
            { inherit pkgs lib;
              inherit containerPkgs;
            }
          ;
          nomadJob = {
            podman = {
              # TODO: oneTracerPerGroup
              oneTracerPerCluster = import ./nomad-job.nix
                { inherit lib stateDir;
                  inherit profileNix;
                  inherit containerSpecs;
                  inherit supervisorConf;
                  # May evolve to a "cloud" flag!
                  execTaskDriver = false;
                  oneTracerPerNode = false;
                };
              oneTracerPerNode = import ./nomad-job.nix
                { inherit lib stateDir;
                  inherit profileNix;
                  inherit containerSpecs;
                  inherit supervisorConf;
                  # May evolve to a "cloud" flag!
                  execTaskDriver = false;
                  oneTracerPerNode = true;
                };
            };
            exec = {
              # TODO: oneTracerPerGroup
              oneTracerPerCluster = import ./nomad-job.nix
                { inherit lib stateDir;
                  inherit profileNix;
                  inherit containerSpecs;
                  inherit supervisorConf;
                  # May evolve to a "cloud" flag!
                  execTaskDriver = true;
                  oneTracerPerNode = false;
                };
              oneTracerPerNode = import ./nomad-job.nix
                { inherit lib stateDir;
                  inherit profileNix;
                  inherit containerSpecs;
                  inherit supervisorConf;
                  # May evolve to a "cloud" flag!
                  execTaskDriver = true;
                  oneTracerPerNode = true;
                };
            };
          };
        };
      in pkgs.runCommand "workbench-backend-output-${profileNix.profileName}-${name}"
        ({
          containerSpecsJSON = pkgs.writeText "workbench-cluster-container-pkgs.json"
            (lib.generators.toJSON {} containerSpecs);
        })
        ''
        mkdir $out
        ln -s $containerSpecsJSON      $out/container-specs.json
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
