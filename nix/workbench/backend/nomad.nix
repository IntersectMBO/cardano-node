{ pkgs
, lib
, stateDir
, subBackendName
# TODO: Fetch this from config services inside materialise-profile !
, eventlogged ? true
, ...
}:
let

  # Backend-specific Nix bits:
  materialise-profile = { profileData }:
    # Intermediate / workbench-adhoc container specifications
    let containerSpecs = rec {
      ##########################################################################
      # The actual commit. The one used when entering the workbench.
      gitrev = pkgs.gitrev;
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
      containerPkgs = installables {inherit gitrev;};
      # The Nomad Job description for the requested sub-backend.
      nomadJob = nomad-job {inherit profileData containerPkgs;};
      ##########################################################################
    };
    in pkgs.runCommand "workbench-backend-output-${profileData.profileName}-nomad"
      ({
        containerSpecsJSON = pkgs.writeText "workbench-cluster-container-pkgs.json"
          (lib.generators.toJSON {} containerSpecs);
      })
      ''
      mkdir $out
      ln -s $containerSpecsJSON      $out/container-specs.json
      ''
    ;

  # The installables are all the Nix packages that will be avaiable inside the
  # Nomad Task. This dependencies are defined inside the Nomad Job as strings
  # that can be either a path to the local nix store or a flake reference to
  # fetch from some repo.
  # In the case of cloud deployment the "installable" must always reference
  # a commit accesible from every Nomad client machine and for local / "exec"
  # the "nix-store-path" property is used to allow to run with local changes.
  # The workbench will by default insert in the Nomad Job description the
  # "installable" property as defined here while keeping the extra details as a
  # `jq` friendly reference that are used to change it later.
  installables = {gitrev}:
    ############################################################################
    # The "default" / basic environment where the node will run. ###############
    # This pkgs rarely change and are almost always cached.      ###############
    ############################################################################
    (lib.attrsets.mapAttrs
      (name: attr:
        # The installable property is always the same.
        let flakeReference = attr.flake-reference;
            flakeOutput = attr.flake-output;
            # The commit must come from `pkgs` because all script are using
            # this for the shebang and other basic tools ("coreutils") and are
            # also the dependencies used inside the workbench, like `jq`.
            commit = pkgs.gitrev;
        in attr // {installable="${flakeReference}/${commit}#${flakeOutput}";}
      )
      {
        coreutils = {
          nix-store-path  = pkgs.coreutils;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.coreutils";
          installable = null;
        };
        bashInteractive = {
          nix-store-path  = pkgs.bashInteractive;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.bashInteractive";
          installable = null;
        };
        findutils = {
          nix-store-path  = pkgs.findutils;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.findutils";
          installable = null;
        };
        iputils = {
          nix-store-path  = pkgs.iputils;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.iputils";
          installable = null;
        };
        gnutar = {
          nix-store-path  = pkgs.gnutar;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.gnutar";
          installable = null;
        };
        zstd = {
          nix-store-path  = pkgs.zstd;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.zstd";
          installable = null;
        };
        wget = {
          nix-store-path  = pkgs.wget;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.wget";
          installable = null;
        };
        cacert = {
          nix-store-path  = pkgs.cacert;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.cacert";
          installable = null;
        };
        supervisor = {
          nix-store-path  = pkgs.python3Packages.supervisor;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.python3Packages.supervisor";
          installable = null;
        };
        gnugrep = {
          nix-store-path  = pkgs.gnugrep;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.gnugrep";
          installable = null;
        };
        jq = {
          nix-store-path  = pkgs.jq;
          flake-reference = "github:intersectmbo/cardano-node";
          flake-output = "legacyPackages.x86_64-linux.jq";
          installable = null;
        };
      }
    )
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
        # Same commit as the basic packages.
        installable = "${flake-reference}/${pkgs.gitrev}#${flake-output}";
      };
    }
    //
    ############################################################################
    # Binaries from `cardanoNodePackages` (node / tracer / generator). #########
    ############################################################################
    {
      # Provided that all the "start.sh" scripts are taking it into account,
      # this packages could be configured to come from a different commit than
      # the one used to enter the shell ??????????
      cardano-node = rec {
        # Local reference only used if not "cloud".
        nix-store-path = with pkgs;
          # TODO: - cardano-node.passthru.profiled
          #       - cardano-node.passthru.eventlogged
          #       - cardano-node.passthru.asserted
          # profileData.node-services."node-0".serviceConfig.value.eventlog
          # builtins.trace (builtins.attrNames profileData.node-services."node-0".serviceConfig.value.eventlog) XXXX
          if eventlogged
            then cardanoNodePackages.cardano-node.passthru.eventlogged
            else cardanoNodePackages.cardano-node
        ;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output =
          if eventlogged
            then "cardanoNodePackages.cardano-node.passthru.eventlogged"
            else "cardanoNodePackages.cardano-node"
        ;
        installable = "${flake-reference}/${gitrev}#${flake-output}";
      };
      cardano-cli = rec {
        # Local reference only used if not "cloud".
        nix-store-path = pkgs.cardanoNodePackages.cardano-cli;
        flake-reference = "github:input-output-hk/cardano-cli";
        flake-output = "cardanoNodePackages.cardano-cli";
        installable = "${flake-reference}/${gitrev}#${flake-output}";
      };
      cardano-tracer = rec {
        # Local reference only used if not "cloud".
        nix-store-path = pkgs.cardanoNodePackages.cardano-tracer;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "cardanoNodePackages.cardano-tracer";
        installable = "${flake-reference}/${gitrev}#${flake-output}";
      };
      tx-generator = rec {
        # Local reference only used if not "cloud".
        nix-store-path = pkgs.cardanoNodePackages.tx-generator;
        flake-reference = "github:intersectmbo/cardano-node";
        flake-output = "cardanoNodePackages.tx-generator";
        installable = "${flake-reference}/${gitrev}#${flake-output}";
      };
    }
  ;

  # The "exec" or "cloud" Nomad Job description.
  nomad-job = {profileData, containerPkgs}:
    # TODO: Repeated code, add the generator's node name to profile.json
    let generatorTaskName = if builtins.hasAttr "explorer" profileData.node-specs.value
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
              inherit profileData;
              inherit generatorTaskName;
              inherit containerPkgs;
              oneTracerPerNode = false;
              withSsh = false;
            };
          oneTracerPerNode = import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileData;
              inherit generatorTaskName;
              inherit containerPkgs;
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
              inherit profileData;
              inherit generatorTaskName;
              inherit containerPkgs;
              oneTracerPerNode = true;
              withSsh = false;
            };
          ssh = import ./nomad-job.nix
            { inherit pkgs lib stateDir;
              inherit profileData;
              inherit generatorTaskName;
              inherit containerPkgs;
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
