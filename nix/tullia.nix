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

self: system:

let
  ciInputName = "GitHub event";
  repo = "input-output-hk/cardano-node";
in rec {
  tasks = let
    common = {
      config,
      ...
    }: {
      preset = {
        # needed on top-level task to set runtime options
        nix.enable = true;

        github-ci = {
          enable = config.actionRun.facts != {};
          inherit repo;
          sha = config.preset.github-ci.lib.getRevision ciInputName null;
          clone.enable = false;
        };
      };

      # some hydra jobs run NixOS tests
      env.NIX_CONFIG = ''
        extra-system-features = kvm
      '';

      memory = 1024 * 32;
      nomad.resources.cpu = 10000;
    };

    # the attribute name in `hydraJobs` for the current system
    os = {
      x86_64-linux = "linux";
      x86_64-darwin = "macos";
    }.${system};

    flakeUrl = {config, lib, ...}:
      lib.escapeShellArg "github:${repo}/${config.preset.github-ci.lib.getRevision ciInputName null}";
  in
    {
      "ci/push" = {lib, ...} @ args: {
        imports = [common];

        command.text = ''
          nix build -L \
            ${flakeUrl args}#hydraJobs.${lib.escapeShellArg os}.required \
            ${flakeUrl args}#hydraJobs.cardano-deployment
        '';
      };

      "ci/pr" = {lib, ...} @ args: {
        imports = [common];

        command.text = ''
          nix build -L \
            ${flakeUrl args}#hydraJobsPr.${lib.escapeShellArg os}.required \
            ${flakeUrl args}#hydraJobsPr.cardano-deployment
        '';
      };
    };

  actions = {
    "cardano-node/ci/push" = {
      task = "ci/push";
      io = ''
        #lib.io.github_push
        #input: "${ciInputName}"
        #repo: "${repo}"
      '';
    };

    "cardano-node/ci/pr" = {
      task = "ci/pr";
      io = ''
        #lib.io.github_pr
        #input: "${ciInputName}"
        #repo: "${repo}"
        #target_default: false
      '';
    };
  };
}
