{ pkgs
, lib
, stateDir
, subBackendName
, ...
}:
let

  # Backend-specific Nix bits:
  materialise-profile = { profileBundle }:
    # Intermediate / workbench-adhoc container specifications
    let containerSpecs = rec {
      ##########################################################################
      # Where to deploy inside the Task, needed to send commands (`nomad exec`).
      diretories = rec {
        work = "/local";
        state = stateDir;
        run = lib.strings.concatStringsSep "/" [ work state ];
      };
      # How to configure supervisor and how to access it from the deployer.
      supervisord = {
        url = "unix:///tmp/supervisor.sock";
        conf = lib.strings.concatStringsSep "/"
          [ diretories.run "supervisor" "supervisord.conf"]
        ;
      };
      # Binaries. Flake references to the local nix store or remote repos.
      containerPkgs = installables;
      # The Nomad Job description for the requested sub-backend.
      nomadJob = nomad-job {inherit profileBundle installables;};
      ##########################################################################
    };
    in pkgs.runCommand "workbench-backend-data-${profileBundle.profile.value.name}-nomad"
      ({
        containerSpecsJSON = lib.generators.toJSON {} containerSpecs;
        passAsFile = ["containerSpecsJSON"];
      })
      ''
      mkdir $out
      cp $containerSpecsJSONPath $out/container-specs.json
      ''
    ;

  # The installables are all the Nix packages that are needed inside the Nomad
  # Task. These dependencies are defined inside the Nomad Job as strings that
  # can either be a path to the local nix store or a flake reference to fetch
  # from a repo.
  # In the case of cloud deployment the installables must always reference a
  # commit accessible from every Nomad client machine, for local / "exec" the
  # "nix-store-path" property is used for simplicity.
  # The workbench will by default insert in the Nomad Job description the local
  # path of this packages while keeping the extra details as a `jq` friendly
  # reference that are used to change it later appending the desired commit
  # (interchanging commits it's still an untested feature).
  # For "nomadcloud" runs workbench's `$WB_GITREV` is used as commit:
  # installable="${flakeReference}/${commit}#${flakeOutput}"
  installables =
    {
      ############################################################################
      # The "default" / basic environment where the node will run. ###############
      # This pkgs rarely change and are almost always cached.      ###############
      ############################################################################
      coreutils = {
        nix-store-path  = pkgs.coreutils;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.coreutils";
      };
      bashInteractive = {
        nix-store-path  = pkgs.bashInteractive;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.bashInteractive";
      };
      findutils = {
        nix-store-path  = pkgs.findutils;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.findutils";
      };
      iputils = {
        nix-store-path  = pkgs.iputils;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.iputils";
      };
      gnutar = {
        nix-store-path  = pkgs.gnutar;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.gnutar";
      };
      zstd = {
        nix-store-path  = pkgs.zstd;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.zstd";
      };
      wget = {
        nix-store-path  = pkgs.wget;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.wget";
      };
      cacert = {
        nix-store-path  = pkgs.cacert;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.cacert";
      };
      supervisor = {
        nix-store-path  = pkgs.python3Packages.supervisor;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.python3Packages.supervisor";
      };
      gnugrep = {
        nix-store-path  = pkgs.gnugrep;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.gnugrep";
      };
      jq = {
        nix-store-path  = pkgs.jq;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.jq";
      };
    }
    //
    ############################################################################
    # Optional container enabled OpenSSH to properly fetch ~1TB cloud logs. ####
    ############################################################################
    lib.attrsets.optionalAttrs (subBackendName == "cloud") {
      openssh_hacks = rec {
        commit = "5d8c5913c70723318acf47496e2abf7d2c99384f"; # OpenSSH version 9.8 (Branch "9.8");
        # Not used locally. Needed to create the SSH "start.sh" script.
        nix-store-path  = (__getFlake "github:fmaste/openssh-portable-hacks/${commit}").packages.x86_64-linux.openssh_hacks;
        flake-reference = "github:fmaste/openssh-portable-hacks";
        flake-output = "packages.x86_64-linux.openssh_hacks";
        installable = "${flake-reference}/${commit}#${flake-output}";
      };
      rsync = rec {
        nix-store-path  = pkgs.rsync; # Not used locally.
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "legacyPackages.x86_64-linux.rsync";
      };
    }
    //
    ############################################################################
    # Binaries from `cardanoNodePackages` (node / tracer / generator). #########
    ############################################################################
    {
      # Provided that all the "start.sh" scripts are taking it into account,
      # this packages could be configured to come from a different commit than
      # the one used to enter the shell.
      cardano-node = rec {
        # Local reference only used if not "cloud".
        # Avoid rebuilding on every commit because of `set-git-rev`.
        nix-store-path = pkgs.cardanoNodePackages.cardano-node.passthru.noGitRev;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "cardanoNodePackages.cardano-node.passthru.noGitRev"
        ;
      };
      cardano-cli = rec {
        # Local reference only used if not "cloud".
        # Avoid rebuilding on every commit because of `set-git-rev`.
        nix-store-path = pkgs.cardanoNodePackages.cardano-cli.passthru.noGitRev;
        flake-reference = "github:input-output-hk/cardano-cli";
        flake-output = "cardanoNodePackages.cardano-cli.passthru.noGitRev";
      };
      cardano-tracer = rec {
        # Local reference only used if not "cloud".
        nix-store-path = pkgs.cardanoNodePackages.cardano-tracer;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "cardanoNodePackages.cardano-tracer";
      };
      tx-generator = rec {
        # Local reference only used if not "cloud".
        nix-store-path = pkgs.cardanoNodePackages.tx-generator;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "cardanoNodePackages.tx-generator";
      };
    }
  ;

  # The "exec" or "cloud" Nomad Job description.
  nomad-job = {profileBundle, installables}:
    # TODO: Repeated code, add the generator's node name to profile.json
    let generatorTaskName = if builtins.hasAttr "explorer" profileBundle.node-specs.value
      then "explorer"
      else "node-0"
      ;
    in
      { inherit generatorTaskName; }
      //
      lib.attrsets.optionalAttrs (subBackendName == "exec") {
        exec = {
          # TODO: oneTracerPerGroup
          oneTracerPerCluster = import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileBundle;
              inherit generatorTaskName;
              inherit installables;
              oneTracerPerNode = false;
              withSsh = false;
            };
          oneTracerPerNode = import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileBundle;
              inherit generatorTaskName;
              inherit installables;
              oneTracerPerNode = true;
              withSsh = false;
            };
        };
      }
      //
      lib.attrsets.optionalAttrs (subBackendName == "cloud") {
        cloud = {
          # Always "oneTracerPerNode"
          # TODO: oneTracerPerCluster and oneTracerPerGroup
          nomadExec = import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileBundle;
              inherit generatorTaskName;
              inherit installables;
              oneTracerPerNode = true;
              withSsh = false;
            };
          ssh = import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileBundle;
              inherit generatorTaskName;
              inherit installables;
              oneTracerPerNode = true;
              withSsh = true;
            };
          # AWS S3 bucket that will be used to deploy the genesis files.
          s3 = rec {
            bucket = "cardano-perf-deploy";
            region = "eu-central-1";
            host   = "s3.${region}.amazonaws.com";
            uri    = "https://${bucket}.${host}";
          };
        };
      }
    ;

in { inherit materialise-profile; }
