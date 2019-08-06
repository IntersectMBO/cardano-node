let
  self = import ./. {};
in self.pkgs.writeScript "helper" ''
#!/bin/sh

${self.nix-tools.cexes.cardano-node.cardano-node}/bin/cardano-node --log-config configuration/log-configuration.yaml --system-start '1970-01-01 00:00:00' node --trace-handshake -n 42 -t ./mainnet.json  --host-addr 127.0.0.1 --port 3001 --real-pbft --genesis-hash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb
''
