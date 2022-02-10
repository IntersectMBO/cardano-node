{ ... }@args:
with (import ./flake-compat.nix args);
defaultNix.legacyPackages.${builtins.currentSystem}
