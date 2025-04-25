{
  pkgs,
  cardano-node-linux,
  ...
}: let
  inherit (builtins) attrNames fromJSON readFile toString;
  inherit (cardanoLib) environments;
  inherit (cardanoNodePackages) cardano-cli;
  inherit (lib) getAttrs getExe foldl' makeBinPath recursiveUpdate;
  inherit (pkgs) cardanoLib cardanoNodePackages gnutar gzip jq lib;

  # NixosTest script fns supporting a timeout have a default of 900 seconds.
  timeout = toString 30;

  environments' = getAttrs ["mainnet" "preprod" "preview"] environments;
  envs = attrNames environments';
  getMagic = env: toString (fromJSON (readFile environments.${env}.nodeConfig.ShelleyGenesisFile)).networkMagic;

  mkSvcTest = env: {
    "cardano-node-${env}" = {
      serviceConfig = {
        WorkingDirectory = "/var/lib/cardano-node-${env}";
        StateDirectory = "cardano-node-${env}";
      };

      preStart = ''
        mkdir config
        cp -v /opt/cardano-node-linux/share/${env}/* ./
      '';

      script = ''
        /opt/cardano-node-linux/bin/cardano-node run \
          --config config.json \
          --topology topology.json \
          --database-path db \
          --socket-path node.socket \
          --port 3001
      '';
    };
  };

  mkScriptTest = env: ''
    machine.systemctl("start cardano-node-${env}")
    machine.wait_until_succeeds("[ -S /var/lib/cardano-node-${env}/node.socket ]", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 12798", timeout=${timeout})
    machine.wait_until_succeeds("nc -z localhost 3001", timeout=${timeout})
    out = machine.succeed(
      "${getExe cardano-cli} ping -h 127.0.0.1 -c 1 -m ${getMagic env} -q --json | ${getExe jq} -c"
    )
    print("ping ${env}:", out)
    machine.succeed("systemctl status cardano-node-${env}")
    machine.succeed("systemctl stop cardano-node-${env}")
    machine.wait_until_fails("nc -z localhost 12798", timeout=${timeout})
    machine.wait_until_fails("nc -z localhost 3001", timeout=${timeout})
  '';
in {
  name = "cardano-node-artifact-test";
  nodes = {
    machine = {config, ...}: {
      nixpkgs.pkgs = pkgs;

      # The default disk size of 1024 MB is insufficient for the binary artifact
      # and tar gzip expansion.
      virtualisation.diskSize = 2048;

      system.activationScripts.prepTest.text = let
        binPath = makeBinPath [gnutar gzip];
      in ''
        export PATH=${binPath}:$PATH
        mkdir -p /opt/cardano-node-linux
        cp -v ${cardano-node-linux}/cardano-node-*-linux.tar.gz /opt/cardano-node-linux.tar.gz
        tar -zxvf /opt/cardano-node-linux.tar.gz -C /opt/cardano-node-linux
      '';

      systemd.services = foldl' (acc: env: recursiveUpdate acc (mkSvcTest env)) {} envs;
    };
  };

  testScript =
    ''
      start_all()
    ''
    + lib.concatMapStringsSep "\n" mkScriptTest envs;
}
