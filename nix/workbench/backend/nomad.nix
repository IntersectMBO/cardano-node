{ pkgs
, lib
, stateDir
# TODO: Fetch this from config services inside materialise-profile !
, eventlogged ? true
, ...
}:
let

  # Backend-specific Nix bits:
  materialise-profile =
    { profileData }:
      let
        supervisorConf = import ./supervisor-conf.nix
          { inherit profileData;
            inherit pkgs lib stateDir;
            # ''{{ env "NOMAD_TASK_DIR" }}/supervisor.sock''
            unixHttpServerPort = "/tmp/supervisor.sock";
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
              flake-output = "legacyPackages.x86_64-linux.zstd";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            wget = rec {
              nix-store-path  = pkgs.wget;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.wget";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            supervisor = rec {
              nix-store-path  = pkgs.python3Packages.supervisor;
              flake-reference = "github:input-output-hk/cardano-node";
              flake-output = "legacyPackages.x86_64-linux.python3Packages.supervisor";
              installable = "${flake-reference}/${gitrev}#${flake-output}";
            };
            # TODO: profileData.node-services."node-0".serviceConfig.value.eventlog
            # builtins.trace (builtins.attrNames profileData.node-services."node-0".serviceConfig.value.eventlog) XXXX
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
                  inherit profileData;
                  inherit containerSpecs;
                  inherit supervisorConf;
                  # May evolve to a "cloud" flag!
                  execTaskDriver = false;
                  oneTracerPerNode = false;
                };
              oneTracerPerNode = import ./nomad-job.nix
                { inherit lib stateDir;
                  inherit profileData;
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
                  inherit profileData;
                  inherit containerSpecs;
                  inherit supervisorConf;
                  # May evolve to a "cloud" flag!
                  execTaskDriver = true;
                  oneTracerPerNode = false;
                };
              oneTracerPerNode = import ./nomad-job.nix
                { inherit lib stateDir;
                  inherit profileData;
                  inherit containerSpecs;
                  inherit supervisorConf;
                  # May evolve to a "cloud" flag!
                  execTaskDriver = true;
                  oneTracerPerNode = true;
                };
            };
          };
        };
      in pkgs.runCommand "workbench-backend-output-${profileData.profileName}-nomad"
        ({
          containerSpecsJSON = pkgs.writeText "workbench-cluster-container-pkgs.json"
            (lib.generators.toJSON {} containerSpecs);
        })
        ''
        mkdir $out
        ln -s $containerSpecsJSON      $out/container-specs.json
        '';

in { inherit materialise-profile; }
