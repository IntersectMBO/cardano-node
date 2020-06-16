{ pkgs
, config
, genesisSpec ? config.genesisSpec or {}
, genesis-keys ? 3
, utxo-keys ? 2
, supply ? 11000000000000000
, interactive ? config.interactive or false
, additionalSystemPackages ? []
, testScript ? "true"
, ... }:

with pkgs.commonLib;
let

  genesisSpecFile = builtins.toFile "genesis.spec.json" (builtins.toJSON (recursiveUpdate
    (recursiveUpdate (builtins.fromJSON (builtins.readFile environments.ff.networkConfig.GenesisFile)) {protocolParams.minPoolCost = 0;})
    genesisSpec));

  nodeConfig = environments.ff.nodeConfig // {
    GenesisFile = "genesis.json";
  };

  nodeConfigFile = builtins.toFile "config.json" (builtins.toJSON nodeConfig);

  inherit (pkgs.cardanoNodeHaskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (pkgs.cardanoNodeHaskellPackages.cardano-node.components.exes) cardano-node;

  configDir = pkgs.runCommand "qa-cluster-genesis-keys" {
    nativeBuildInputs = [ cardano-cli ];
  } ''
    mkdir -p $out

    cp ${genesisSpecFile} $out/genesis.spec.json

    cardano-cli shelley genesis create --genesis-dir $out --testnet-magic 42 \
      --gen-genesis-keys ${toString genesis-keys} \
      --gen-utxo-keys ${toString utxo-keys} \
      --supply ${toString supply} \
      --start-time "2001-09-09T01:46:40Z" \

    rm $out/genesis.spec.json

    cp ${nodeConfigFile} $out/config.json

    mkdir -p $out/node-keys
    cd $out/node-keys
    for i in {1..${toString genesis-keys}}; do
      cardano-cli shelley node key-gen-KES --verification-key-file node-kes$i.vkey --signing-key-file node-kes$i.skey
      cardano-cli shelley node issue-op-cert --hot-kes-verification-key-file node-kes$i.vkey --cold-signing-key-file ../delegate-keys/delegate$i.skey --operational-certificate-issue-counter ../delegate-keys/delegate$i.counter --kes-period 0 --out-file node$i.opcert
    done
  '';

  nodes = listToAttrs (map (i: rec {
    name = "core${toString i}";
    value = recursiveUpdate (mkNode name) {
      services.cardano-node = {
        vrfKey = "${configDir}/delegate-keys/delegate${toString i}.vrf.skey";
        kesKey = "${configDir}/node-keys/node-kes${toString i}.skey";
        operationalCertificate = "${configDir}/node-keys/node${toString i}.opcert";
      };
    };
  }) (genList (i: i + 1) genesis-keys)
  ++ [(rec {
    name = "edge";
    value = mkNode name;
  })]);

  socketPath = "/run/cardano-node/node.socket";

  mkNode = name: let
    topology = mkEdgeTopology {
      edgeNodes = map (n: "${n}.local") (filter (n: n != name) (attrNames nodes));
    };
  in {
    nixpkgs = { inherit pkgs; };
    imports = [
      ../cardano-node-service.nix
    ];
    virtualisation.memorySize = 2048;
    virtualisation.diskSize   = 2048;
    ## Time
    services.timesyncd.enable = false;
    virtualisation.qemu.options = [
      ## systemStart == Unix 1bn = Sun Sep  9 01:46:40 UTC 2001
      "-rtc base=2001-09-09T01:46:05"
    ];

    environment.variables = {
      CARDANO_NODE_SOCKET_PATH = socketPath;
      CONFIG_DIR = "${configDir}";
      TOPOLOGY = "${topology}";
    };

    environment.systemPackages = with pkgs; [ cardano-cli cardano-node tcpdump dnsutils ] ++ additionalSystemPackages;

    services.cardano-node = {
      enable = true;
      inherit socketPath nodeConfig topology;
      systemdSocketActivation = true;
      cardanoNodePkgs = pkgs;
      nodeConfigFile = "${configDir}/config.json";
      hostAddr = "0.0.0.0";
    };

    ## Network
    services.dnsmasq = {
      enable = true;
      servers = [ "127.0.0.1" ];
      extraConfig = ''
        local=/localnet/
        expand-hosts
        domain=local
      '';
    };
    networking.firewall.allowedTCPPorts = [ 3001 ];
  };

in {
  name = "qa-tests-cluster";
  inherit nodes;
  testScript = ''
    startAll
  '' + concatMapStringsSep "\n" (n: ''
    ${"$"}${n}->waitForUnit("cardano-node.service");
    ${"$"}${n}->waitForOpenPort(3001);
    ${"$"}${n}->succeed("stat /run/cardano-node/node.socket");
  '') (attrNames nodes)
  + ''
    $edge->succeed("{ now=\$(date +%s); if test 1000000000 -gt \$now; then echo 'Waiting until 1000000000'; sleep \$((1000000000 - now)); fi; } | systemd-cat --identifier=wait-until-1000000000");
    $edge->succeed("${testScript}");
  '' + optionalString interactive
  ''
    $edge->waitUntilSucceeds("netstat -ptn | grep ESTABLISHED | systemd-cat --identifier=netstat --priority=crit; sleep 9s; false");
  '';
}
