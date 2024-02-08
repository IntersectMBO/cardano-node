let profileData = builtins.getEnv "WB_SHELL_PROFILE_DATA"; in
{
  # NOTE: this is a bit of a roundabout way to import legacyPackages as defined in the flake.
  # It goes thorugh flake-compat.
  pkgs ? import ../. { }
, profile-json ? "${profileData}/profile.json"
, node-specs ? "${profileData}/node-specs.json"
}:

with pkgs.lib;

evalModules {
  modules = [
    ./modules/documentation.nix
    ./modules/profile.nix
    { _module.args = { inherit pkgs; }; }
    { _file = profile-json; inherit (importJSON profile-json) composition derived genesis; }
    { _file = node-specs; node-specs = importJSON node-specs; }
  ];
}
