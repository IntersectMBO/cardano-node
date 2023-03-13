/*
  This file defines tullia tasks and cicero actions.

  Tullia is a sandboxed multi-runtime DAG task runner with Cicero support.
  Tasks can be written in different languages and are compiled for each runtime using Nix.
  It comes with essential building blocks for typical CI/CD scenarios.
  Learn more: https://github.com/input-output-hk/tullia

  Cicero is an if-this-then-that machine on HashiCorp Nomad.
  It can run any event-and-state-driven automation actions
  and hence CI/CD pipelines are a natural fit.
  In tandem with Tullia, an action could be described as
  the rule that describes when a Tullia task is to be invoked.
  Learn more: https://github.com/input-output-hk/cicero
*/

let
  ciInputName = "GitHub event";
  repository = "input-output-hk/cardano-node";
in
rec {
  tasks =
    let
      common =
        { config
        , ...
        }: {
          preset = {
            nix.enable = true;

            github.ci = {
              enable = config.actionRun.facts != { };
              inherit repository;
              remote = config.preset.github.lib.readRepository ciInputName null;
              revision = config.preset.github.lib.readRevision ciInputName null;
            };
          };

          nomad.driver = "exec";
        };


      mkBulkJobsTask = jobsAttrs: { config
                                  , lib
                                  , ...
                                  }: {
        imports = [ common ];

        command.text = ''
          # filter out systems that we cannot build for:
          systems=$(nix eval .#legacyPackages --apply __attrNames --json |
            nix-systems -i |
            jq -r 'with_entries(select(.value)) | keys | .[]')
          targets=$(for s in $systems; do echo .#legacyPackages."$s".hydraJobs.${jobsAttrs}; done)
          # shellcheck disable=SC2086
          nix build -L $targets
        '';

        env.NIX_CONFIG = ''
          # `kvm` for NixOS tests
          # `benchmark` for benchmarks
          extra-system-features = kvm benchmark
          # bigger timeouts (900 by default on cicero) needed for some derivations (especially on darwin)
          max-silent-time = 1800
        '';

        memory = 1024 * 32;
        nomad.resources.cpu = 10000;
      };
    in
    {
      "ci/pr/nix/required" = mkBulkJobsTask "pr.required";
      "ci/pr/nix/nonrequired" = mkBulkJobsTask "pr.nonrequired --keep-going";
      "ci/push/nix/required" = mkBulkJobsTask "required";
      "ci/push/nix/nonrequired" = mkBulkJobsTask "nonrequired --keep-going";

      "ci/cardano-deployment" = { lib, ... } @ args: {
        imports = [ common ];
        command.text = ''
          nix build -L .#hydraJobs.cardano-deployment
        '';
        memory = 1024 * 16;
        nomad.resources.cpu = 10000;
      };

      "ci/pr/system-tests" = {lib, ...} @ args: {
        imports = [common];
        after = [];

        command.text = ''
          nix run -L .#system-tests
        '';

        memory = 1024 * 64;
        nomad.resources.cpu = 40000;
      };
    };

  actions =
    let
      prIo = ''
        #lib.io.github_pr
        #input: "${ciInputName}"
        #repo: "${repository}"
      '';
      prAndBorsIo = ''
        let github = {
          #input: "${ciInputName}"
          #repo: "${repository}"
        }
        let borsBranches = {
          #branch: "staging|trying"
        }
        #lib.merge
        #ios: [
          #lib.io.github_pr & github,
          #lib.io.github_push & borsBranches & github,
        ]
      '';
      pushIo = ''
        #lib.io.github_push
        #input: "${ciInputName}"
        #repo: "${repository}"
        #branch: "master|release/.*"
      '';
    in
    {

      "cardano-node/ci/push/nix/required" = {
        task = "ci/push/nix/required";
        io = pushIo;
      };
      "cardano-node/ci/push/nix/nonrequired" = {
        task = "ci/push/nix/nonrequired";
        io = pushIo;
      };
      "cardano-node/ci/push/cardano-deployment" = {
        task = "ci/cardano-deployment";
        io = pushIo;
      };
      "cardano-node/ci/pr/nix/required" = {
        task = "ci/pr/nix/required";
        io = prIo;
      };
      "cardano-node/ci/pr/nix/nonrequired" = {
        task = "ci/pr/nix/nonrequired";
        io = prIo;
      };
      "cardano-node/ci/pr/cardano-deployment" = {
        task = "ci/cardano-deployment";
        io = prIo;
      };
      "cardano-node/ci/pr/system-tests" = {
        task = "ci/pr/system-tests";
        io = prIo;
      };
    };
}
