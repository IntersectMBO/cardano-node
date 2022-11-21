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
in rec {
  tasks = let
    mkTask = top: {config, lib, ...}: {
      preset = {
        nix.enable = true;

        github.ci = {
          enable = config.actionRun.facts != {};
          inherit repository;
          remote = config.preset.github.lib.readRepository ciInputName null;
          revision = config.preset.github.lib.readRevision ciInputName null;
        };
      };

      command.text = config.preset.github.status.lib.reportBulk {
        bulk.text = ''
          echo '["x86_64-linux", "x86_64-darwin"]' | nix-systems -i \
          | jq 'with_entries(.key |= {"x86_64-linux": "linux", "x86_64-darwin": "macos"}[.])'
        '';
        each.text = ''nix build -L .#${lib.escapeShellArg top}."$1".required'';
        skippedDescription = lib.escapeShellArg "No nix builder available for this platform";
      } + "\n" + ''
        nix build -L .#${lib.escapeShellArg top}.cardano-deployment
      '';

      # some hydra jobs run NixOS tests
      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 32;
      nomad.resources.cpu = 10000;
    };
  in {
    "ci/push" = mkTask "hydraJobs";
    "ci/pr" = mkTask "hydraJobsPr";
  };

  actions = {
    "cardano-node/ci/push" = {
      task = "ci/push";
      io = ''
        #lib.io.github_push
        #input: "${ciInputName}"
        #repo: "${repository}"
        #branch: "master|staging|trying"
      '';
    };

    "cardano-node/ci/pr" = {
      task = "ci/pr";
      io = ''
        #lib.io.github_pr
        #input: "${ciInputName}"
        #repo: "${repository}"
      '';
    };
  };
}
