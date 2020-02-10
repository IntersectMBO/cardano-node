{ pkgs
, config
, interactive ? config.interactive or false
, ... }:

with pkgs.lib;
let
  inherit (pkgs) svcLib commonLib;
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
    legacy-node-count  = 4;
    shelley-node-count = 3;
    genesis-dir        = "${../../../configuration/mainnet-ci}";
    inherit cardano-sl-config cardano-sl-src;
  };
  chairman-runner = svcLib.mkChairmanScript {
    inherit cardano-cluster-config;
    chairman-config = {
      timeout    = 450;
      maxBlockNo = 20;
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
    startAll
    $machine->succeed("mkdir -p /var/lib/cardano-node");
    $machine->succeed("chown -R cardano-node.cardano-node /var/lib/cardano-node");
    $machine->succeed("lscpu | systemd-cat --identifier=lscpu --priority=crit");
    $machine->succeed("date +%s | systemd-cat --identifier=timekeeper --priority=crit");
    $machine->succeed("cat /proc/sys/net/ipv4/tcp_syn_retries | systemd-cat --identifier tcp_syn_retries");
    $machine->succeed("bash -c 'tcpdump -i lo tcp and port 5555 2>&1 | systemd-cat --identifier=proxy-tcpdump --priority=crit' &");
    $machine->succeed("{ now=\$(date +%s); if test 1000000000 -gt \$now; then echo 'Waiting until 1000000000'; sleep \$((1000000000 - now)); fi; } | systemd-cat --identifier=wait-until-1000000000");
    $machine->succeed("systemctl start cardano-cluster || true");
    $machine->waitUntilSucceeds("nc -z 127.1.0.1 3001");
    $machine->waitUntilSucceeds("nc -z 127.1.0.2 3002");
    $machine->waitUntilSucceeds("nc -z 127.1.0.3 3003");
    $machine->waitUntilSucceeds("nc -z 127.1.0.4 3004");
    $machine->waitUntilSucceeds("nc -z 127.1.0.5 3005");
    $machine->waitUntilSucceeds("nc -z 127.1.0.6 3006");
    $machine->waitUntilSucceeds("nc -z 127.1.0.7 3007");
    $machine->waitUntilSucceeds("nc -z 127.1.0.9 5555");
    $machine->waitUntilSucceeds("nc -z 127.1.0.9 7777");
    $machine->succeed("ip r l                      | systemd-cat --identifier=routing");
    $machine->succeed("cat /etc/resolv.conf        | systemd-cat --identifier=resolv");
    $machine->succeed("netstat -pltn | grep LISTEN | systemd-cat --identifier=netstat");
    $machine->succeed("ls -l /var/lib/cardano-node/*.socket | systemd-cat --identifier=sockets");
    $machine->succeed("bash -c 'set -o pipefail; { ${chairman-runner} 2>&1; status=\$?; echo END-OF-CHAIRMAN-OUTPUT-MARKER; sleep 3; exit \$status; } | systemd-cat --identifier=chairman --priority=crit'");
  '' + optionalString interactive
  ''
    $machine->waitUntilSucceeds("netstat -ptn | grep ESTABLISHED | systemd-cat --identifier=netstat --priority=crit; sleep 9s; false");
  '';
}
