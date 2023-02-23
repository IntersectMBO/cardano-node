{ pkgs, ... }@args:
with pkgs;
let
  allow-all-to-tcp-port =portName: port: { region, org, pkgs, ... }@args: {
    "allow-all-to-${portName}-${region}-${org}" = {resources, ...}: {
      inherit region;
      accessKeyId = pkgs.globals.ec2.credentials.accessKeyIds.${org};
      _file = ./allow-all-to-tcp-port.nix;
      description = "Allow All to TCP/${toString port}";
      rules = [{
        protocol = "tcp";
        fromPort = port;
        toPort = port;
        sourceIp = "0.0.0.0/0";
      }];
    } // pkgs.lib.optionalAttrs (args ? vpcId) {
      vpcId = resources.vpc.${args.vpcId};
    };
  };
in allow-all-to-tcp-port "cardano" globals.cardanoNodePort args
