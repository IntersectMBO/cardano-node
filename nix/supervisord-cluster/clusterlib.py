import subprocess
import json

class CLIError(Exception):
    pass

class clusterlib:
    """Cluster Lib"""
    networkMagic = 42
    def __init__(self, networkMagic, stateDir):
        self.networkMagic = networkMagic
        self.stateDir = stateDir
        self.genesis = json.load(open(f"{stateDir}/keys/genesis.json"))
        self.genesisUtxoVkey = f"{stateDir}/keys/genesis-utxo.vkey"
        self.genesisUtxoSkey = f"{stateDir}/keys/genesis-utxo.skey"
        self.genesisUtxoAddr = self.getGenesisAddress(self.genesisUtxoVkey)
        self.pparamsFile = f"{self.stateDir}/pparams.json"
        self.refreshPParams()
        self.slotLength = self.genesis["slotLength"]
        self.epochLength = self.genesis["epochLength"]

    def CLI(self, args):
        p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout,stderr = p.communicate()
        if (p.returncode != 0):
            print("the command that failed is {}".format(p.args))
            print(stderr)

            raise CLIError("An error occurred running a CLI command!")
        return stdout

    def prependFlag(self, flag, contents):
        return sum(list(map( lambda x: [ flag, x ], contents)),[])

    def queryCLI(self, args):
        return self.CLI(["cardano-cli", "shelley", "query"] + args + ["--testnet-magic", str(self.networkMagic) ])

    def estimateFee(self, txbody, txins=1, txouts=1, witnesses=1, byron_witnesses=0):
        self.refreshPParams()
        stdout = self.CLI(["cardano-cli", "shelley", "transaction", "calculate-min-fee", "--testnet-magic", str(self.networkMagic), "--protocol-params-file", self.pparamsFile,  "--tx-in-count", str(txins), "--tx-out-count", str(txouts), "--byron-witness-count", str(byron_witnesses), "--witness-count", str(witnesses), "--tx-body-file", txbody])
        return int(stdout.decode().split(" ")[1])

    # TODO: add withdrawal support
    # TODO: Move fee calculation here
    def sendTx(self, txins=[], txouts=[], certificates=[], signing_keys=[], change_address=None, withdrawals=[], fee=0, proposal=None):
        # TODO: make change_address work - Needs CLI endpoint to query utxo by txid
        # If no change_address specified send to utxo change address
        #if not change_address:
        #    change_address = self.genesisUtxoAddr

        # TODO: calculate from current tip
        ttl=100000
        # Estimate Fee
        #fee = self.estimateFee(ttl=ttl, txins=len(txins), txouts=len(txouts), signing_keys = [ self.genesisUtxoSkey ] + signing_keys)
        # TODO: unhardcode genesis utxo
        change = (self.genesisUtxoAddr, 0)
        txouts.append(change)
        total_input_amount = 0
        # TODO: use a fold to be less hacky
        for txin in txins:
            total_input_amount += txin[2]

        deposit_amount = 0


        txins_combined = list(map( lambda x: f"{x[0]}#{x[1]}", txins))
        txouts_combined = list(map( lambda x: f"{x[0]}+{x[1]}", txouts))

        txin_args = self.prependFlag("--tx-in", txins_combined)

        txout_args = self.prependFlag("--tx-out", txouts_combined)

        cert_args = self.prependFlag("--certificate-file", certificates)

        build_args = [ "--ttl", str(ttl), "--fee", "0", "--out-file", "tx.body" ] + txin_args + txout_args + cert_args

        if proposal:
            build_args.extend(["--update-proposal-file", proposal])


        # Build TX

        self.CLI(["cardano-cli", "shelley", "transaction", "build-raw"] + build_args )
        # Estimate Fee
        fee = self.estimateFee("tx.body", txins=len(txins), txouts=len(txouts), witnesses=len(signing_keys))

        txouts[-1] = (self.genesisUtxoAddr, (total_input_amount - fee - deposit_amount))
        txouts_combined = list(map( lambda x: f"{x[0]}+{x[1]}", txouts))
        txout_args = self.prependFlag("--tx-out", txouts_combined)
        build_args = [ "--ttl", str(ttl), "--fee", str(fee), "--out-file", "tx.body" ] + txin_args + txout_args + cert_args

        if proposal:
            build_args.extend(["--update-proposal-file", proposal])

        self.CLI(["cardano-cli", "shelley", "transaction", "build-raw"] + build_args )

        if proposal:
            build_args.extend(["--update-proposal-file", proposal])


        # Sign TXw

        key_args = self.prependFlag("--signing-key-file", signing_keys)

        sign_args = [ "--tx-body-file", "tx.body", "--out-file", "tx.signed", "--testnet-magic", str(self.networkMagic) ] + key_args
        self.CLI(["cardano-cli", "shelley", "transaction", "sign"] + sign_args )

        # Submit TX
        try:
            self.CLI(["cardano-cli", "shelley", "transaction", "submit", "--testnet-magic", str(self.networkMagic), "--tx-file", "tx.signed"])
        except CLIError as e:
            print("Failed to submit transaction!")
            print(f"txins: {txins} txouts: {txouts} signing keys: {signing_keys}")
            raise e

    def getAddress(self, payment=None, stake=None):
        args = []
        if not payment and stake:
            raise CLIError("Must set payment, stake or both")
        elif payment:
            args.extend("--payment-verification-key-file", payment)
        elif stake:
            args.extend("--stake-verification-key-file", stake)
        return self.CLI(["cardano-cli", "shelley", "address", "build", "--testnet-magic", str(self.networkMagic)] ++ args).rstrip().decode('ascii')

    def getGenesisAddress(self, vkey):
        return self.CLI(["cardano-cli", "shelley", "genesis", "initial-addr", "--testnet-magic", str(self.networkMagic), "--verification-key-file", vkey]).rstrip().decode('ascii')

    def getUtxo(self, address):
        self.queryCLI(["utxo", "--address", address, "--out-file", "utxo.json"])
        return json.load(open("utxo.json"))


    def sendTxGenesis(self, txouts=[], certificates=[], signing_keys=[], deposit_amount=0, proposal=None):

        # TODO: calculate from current tip
        ttl=100000
        signing_keys.append(self.genesisUtxoSkey)

        utxo = self.getUtxo(address=self.genesisUtxoAddr)
        txins = []
        for k,v in utxo.items():
            txin = k.split("#")
            txin = (txin[0], txin[1], v["amount"])
            txins.append(txin)

        # Build, Sign and Send TX to chain

        try:
            self.sendTx(txins=txins, txouts=txouts, certificates=certificates, signing_keys=signing_keys, proposal=proposal, change_address=self.genesisUtxoAddr)
        except CLIError as e:
            print("Sending a genesis transaction failed!")
            print(f"utxo: {utxo}")
            raise e

    def submitUpdateProposal(self, args):
        epoch=1
        self.CLI(["cardano-cli", "shelley", "governance", "create-update-proposal"] + args + [ "--out-file", "update.proposal", "--epoch", str(epoch), "--genesis-verification-key-file", f"{self.stateDir}/keys/genesis-keys/genesis1.vkey"])
        self.sendTxGenesis(proposal="update.proposal", signing_keys=[f"{self.stateDir}/keys/delegate-keys/delegate1.skey"])

    def printTip(self):
        print(self.queryCLI(["tip"]))


    def refreshPParams(self):
        self.queryCLI(["protocol-parameters", "--out-file", self.pparamsFile])
        self.pparams = json.load(open(self.pparamsFile))
