with import <nixpkgs> {};

let
  common = let
    version = "1.0.0";
  in
    stdenv.mkDerivation {
      name = "common-${version}";

      # Using this build support function to fetch it from github
      src = fetchFromGitHub {
        owner = "ArturWieczorek";
        repo = "node-test-scripts";
        # The git tag to fetch
        rev = "b0d6c9076cc68a93b962f033c7e6e0a4ca18df6a";
        # Hashes must be specified so that the build is purely functional
        sha256 = "0lw4ygl6lp8spd5xq01azvnhwk44brb5g0nhv25yqd7sgg77k98y";
      };

      # We override the install phase, as the project doesn't use make
      installPhase = ''
        # Make the output directory
        mkdir -p $out/bin

        # Copy the script there and make it executable
        cp e2e-automation-scripts/common.sh $out/bin/
        chmod +x $out/bin/common.sh
      '';
    };
in
stdenv.mkDerivation {
  name = "common-environment";
  buildInputs = [ common ];
}
