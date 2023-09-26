{ lib, rustPlatform }:

rustPlatform.buildRustPackage {
  pname = "cddl";
  version = "1.0.0";

  src = lib.cleanSource ./.;

  cargoHash = "sha256-OjfZ/bq1JhyVTYKPh1KQpNSuaYjDto6A/YcLZnqlN3Q=";
}
