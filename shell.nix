with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "srcEnv";
  buildInputs = [ tmux systemd zlib gmp ];
}
