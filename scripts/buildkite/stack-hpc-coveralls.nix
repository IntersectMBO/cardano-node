{ mkDerivation, aeson, base, bytestring, containers, deepseq
, directory, docopt, fetchgit, filepath, hlint, hpc, hspec
, hspec-contrib, http-client, HUnit, lens, lens-aeson, process
, pureMD5, stdenv, text, time, unordered-containers, utf8-string
, wreq, yaml
}:
mkDerivation {
  pname = "stack-hpc-coveralls";
  version = "0.0.4.0";
  src = fetchgit {
    url = "https://github.com/input-output-hk/stack-hpc-coveralls";
    rev = "c1a6b5a8be71a3d4ae3f304ac174bf2c93cc88c8";
    sha256 = "0d6iv6s0pxbvp51w16www7xmdxfk0yhy0n9vx0jj8fl71sslhhn7";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory filepath hpc http-client
    lens lens-aeson process pureMD5 text unordered-containers
    utf8-string wreq yaml
  ];
  executableHaskellDepends = [ aeson base bytestring docopt ];
  testHaskellDepends = [
    aeson base containers deepseq hlint hpc hspec hspec-contrib HUnit
    time
  ];
  homepage = "http://github.com/rubik/stack-hpc-coveralls";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.isc;
}
