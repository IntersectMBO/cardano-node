{ options, lib, pkgs, ... }:

#
# Documentation generation
#

with lib;

let docs = pkgs.nixosOptionsDoc { inherit options; };
in

{
  options.documentation = mkOption {
    description = "Documentation";
    type = with types; attrsOf package;
  };

  config.documentation = {
    inherit (docs) optionsAsciiDoc optionsCommonMark optionsJSON;
  };
}
