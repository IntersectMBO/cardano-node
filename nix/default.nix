{ ... }@args:
with (import ./flake-compat.nix args);
defaultNix.legacyPackages.${args.system or builtins.currentSystem}
