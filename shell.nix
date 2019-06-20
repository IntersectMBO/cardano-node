with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "srcEnv";
  nativeBuildInputs = [
                        tmux
                      ];
  buildInputs = [
                ];
}
