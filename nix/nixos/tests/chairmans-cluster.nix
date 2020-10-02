{ pkgs
, config
, interactive ? config.interactive or false
, ... }:

with pkgs.lib;
let
  inherit (pkgs) svcLib commonLib;
  pkgs' = pkgs;
  byron-proxy-src = commonLib.sources.cardano-byron-proxy;
  cardano-sl-src  = commonLib.sources.cardano-sl;
  # byron-proxy-src = ../../../../cardano-byron-proxy;
  # cardano-sl-src  = ../../../../cardano-sl;
  cardano-sl-config = pkgs.runCommand "cardano-sl-config" {} ''
    mkdir -p $out
    cp ${../../../configuration/mainnet-ci/log-config-cluster.yaml} $out/log-config-cluster.yaml
    cp ${../../../configuration/mainnet-ci/configuration.yaml}      $out/configuration.yaml
    cp ${../../../configuration/mainnet-ci/genesis.json}            $out/genesis.json
    ln -s . $out/lib  ## Workaround for proxy configuration specified differently in its service definition.
  '';
  cardano-cluster-config  = {
    enable = true;
    ## NOTE:  Node counts
    ##
    ## 1. The cluster is currently hard-wired to have 7 nodes in total.
    ## 2. Beware of setting the legacy slice too large, as it might cause
    ##    issues due to the connection handling inefficiency between the legacy nodes,
    ##    as the topology is fully connected within both segments (all-to-all).
    legacy-node-count  = 4;
    shelley-node-count = 3;
    genesis-dir        = "${../../../configuration/mainnet-ci}";
    inherit cardano-sl-config cardano-sl-src;
  };
  chairman-runner = svcLib.mkChairmanScript {
    inherit cardano-cluster-config;
    chairman-config  = {
      timeout        = 450;
      testnet-magic  = 10000000;
      security-parameter = 2160;
    };
  };
in {
  name = "chairmans-cluster-test";
  nodes = {
    machine = { lib, config, pkgs, ... }: {
      nixpkgs.overlays = commonLib.overlays;
      imports = [
        (byron-proxy-src + "/nix/nixos")
        ../cardano-node-service.nix
        ../cardano-node-legacy-service.nix
        ../chairman-as-a-service.nix
        ../cardano-cluster-service.nix
      ];
      virtualisation.memorySize = 2048;
      virtualisation.diskSize   = 2048;
      virtualisation.qemu.options = [
        ## systemStart == Unix 1bn = Sun Sep  9 01:46:40 UTC 2001
        "-rtc base=2001-09-09T01:46:29"
      ];

      services.cardano-cluster = cardano-cluster-config;
      services.cardano-node.cardanoNodePkgs = pkgs';

      ## Time
      services.timesyncd.enable = false;

      ## Network
      services.dnsmasq.enable = true;
      services.dnsmasq.servers = [ "127.0.0.1" ];
      networking.extraHosts = ''
        127.1.0.1 c-a-1.cardano
        127.1.0.2 c-a-2.cardano
        127.1.0.3 c-b-1.cardano
        127.1.0.4 c-b-2.cardano
        127.1.0.5 c-c-1.cardano
        127.1.0.6 c-c-2.cardano
        127.1.0.7 c-d-1.cardano
        127.1.0.8 r-a-1.cardano
        127.1.0.9 proxy.cardano
      '';
      environment.systemPackages = [ pkgs.tcpdump ];
    };
  };
  testScript = ''
    start_All()
    machine.succeed("mkdir -p /var/lib/cardano-node")
    machine.succeed("chown -R cardano-node.cardano-node /var/lib/cardano-node")
    machine.succeed("lscpu | systemd-cat --identifier=lscpu --priority=crit")
    machine.succeed("date +%s | systemd-cat --identifier=timekeeper --priority=crit")
    machine.succeed("{ now=\$(date +%s); if test 1000000000 -gt \$now; then echo 'Waiting until 1000000000'; sleep \$((1000000000 - now)); fi; } | systemd-cat --identifier=wait-until-1000000000")
    machine.succeed("systemctl start cardano-cluster || true")
    machine.wait_until_succeeds("nc -z 127.1.0.1 3001")
    machine.wait_until_succeeds("nc -z 127.1.0.2 3002")
    machine.wait_until_succeeds("nc -z 127.1.0.3 3003")
    machine.wait_until_succeeds("nc -z 127.1.0.4 3004")
    machine.wait_until_succeeds("nc -z 127.1.0.5 3005")
    machine.wait_until_succeeds("nc -z 127.1.0.6 3006")
    machine.wait_until_succeeds("nc -z 127.1.0.7 3007")
    machine.wait_until_succeeds("nc -z 127.1.0.9 5555")
    machine.wait_until_succeeds("nc -z 127.1.0.9 7777")
    machine.succeed("netstat -pltn | grep LISTEN | systemd-cat --identifier=netstat")
    machine.succeed("bash -c 'set -o pipefail; { ${chairman-runner} 2>&1; status=\$?; echo END-OF-CHAIRMAN-OUTPUT-MARKER; sleep 1; exit \$status; } | systemd-cat --identifier=chairman --priority=crit'")
  '' + optionalString interactive
  ''
    machine.wait_until_succeeds("netstat -ptn | grep ESTABLISHED | systemd-cat --identifier=netstat --priority=crit; sleep 9s; false")
  '';
}
