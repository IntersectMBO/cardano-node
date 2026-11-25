# The tools a remote bench-parquet run needs, as store paths for each system a
# remote may run on: from the nixpkgs revision the enclosing flake's nixpkgs
# input is locked to (flake.lock, follows resolved), without user overlays or
# config. Evaluated locally (nothing is built); a remote fetches its system's
# paths from its substituters. bench-parquet remote reads the result.
#
# Output: one line per system, "SYSTEM PATH..", bash first.
{ lockFile }:
let
  lock = builtins.fromJSON (builtins.readFile lockFile);

  # A node's input: a node name, or a follows path from the root.
  input = node: name:
    let ref = lock.nodes.${node}.inputs.${name};
    in if builtins.isList ref then builtins.foldl' input lock.root ref else ref;

  src = builtins.fetchTree lock.nodes.${input lock.root "nixpkgs"}.locked;

  tools = [ "bashInteractive" "duckdb" "coreutils" "gawk" "gnused" "findutils" ];
  systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

  line = system:
    let pkgs = import src { inherit system; config = { }; overlays = [ ]; };
    in builtins.concatStringsSep " " ([ system ] ++ map (t: pkgs.${t}.outPath) tools);
in
builtins.concatStringsSep "\n" (map line systems) + "\n"
