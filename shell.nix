with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "srcEnv";
  buildInputs = [ tmux
                ];
}
