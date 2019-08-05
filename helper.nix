let
  self = import ./. {};
in self.pkgs.writeScript "helper" ''
#!/bin/sh

${self.nix-tools.cexes.cardano-node.cardano-node}/bin/cardano-node --log-config configuration/log-configuration.yaml --system-start '1970-01-01 00:00:00' node --trace-handshake -n 42 -t ./mainnet.json  --host-addr 127.0.0.1 --port 3001 --real-pbft
''
