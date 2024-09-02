# SRE patched version of Nomad allowing `nix_installables` as artifacts.
{...}@args:
let
  # Patched 1.6.3.
  commit = "8f3b74796a8f56f38a812813c64dba995956a66e";
  flake = (__getFlake "github:input-output-hk/cardano-perf/${commit}");
  nomad-sre = flake.packages.${builtins.currentSystem}.nomad;
in
  nomad-sre
