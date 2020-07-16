"""Cardano Python Library."""
import functools
import json
import subprocess
from copy import copy
from pathlib import Path
from time import sleep


class CardanoCLIError(Exception):
    pass

class CardanoCluster:
    """Cardano Cluster Tools."""

    def __init__(self, num_delegs, cli):
        self.cli = cli
        self.byron_delegate_keys = list(map(lambda x: f"{self.cli.state_dir}/byron/delegate-keys.{x:03}.key", list(range(0,num_delegs))))
        self.shelley_delegate_skeys = list(map(lambda x: f"{self.cli.state_dir}/shelley/delegate-keys/delegate{x}.skey", list(range(1,num_delegs+1))))
        self.shelley_genesis_vkeys = list(map(lambda x: f"{self.cli.state_dir}/shelley/genesis-keys/genesis{x}.key", list(range(1,num_delegs+1))))

    def hard_fork_byron(self, version):
        version_params = version.split(".")
        proposal_path = f"{self.cli.state_dir}/{version}.proposal"
        self.cli.cli(["cardano-cli", "byron", "create-update-proposal", "--testnet-magic", str(self.cli.network_magic), "--signing-key", self.byron_delegate_keys[0], "--protocol-version-major", version_params[0], "--protocol-version-minor", version_params[1], "--protocol-version-alt", version_params[2], "--application-name", "cardano-sl", "--software-version-num", "1", "--system-tag", "linux", "--installer-hash", "0", "--filepath", proposal_path])
        self.cli.cli(["cardano-cli", "byron", "submit-update-proposal", "--testnet-magic", str(self.cli.network_magic), "--filepath", proposal_path])
        for index,key in enumerate(self.byron_delegate_keys, start=1):
            proposal_vote_path = f"{proposal_path}-vote{index}"
            self.cli.cli(["cardano-cli", "byron", "create-proposal-vote", "--proposal-filepath", proposal_path, "--testnet-magic", str(self.cli.network_magic), "--signing-key", key, "--vote-yes", "--output-filepath", proposal_vote_path])
            self.cli.cli(["cardano-cli", "byron", "submit-proposal-vote", "--testnet-magic", str(self.cli.network_magic), "--filepath", proposal_vote_path])
        self.update_config_version(version_params)
        for i in range(len(self.byron_delegate_keys)):
            self.restart_node(f"bft{i+1}")
            sleep(1)

    def restart_node(self, node):
        self.cli.cli(["supervisorctl", "restart", node])

    def update_config_version(self, version_params, config_file="config.json"):
        config_file = self.cli.state_dir / config_file
        with open(config_file) as in_json:
            config = json.load(in_json)
        config["LastKnownBlockVersion-Major"] = int(version_params[0])
        config["LastKnownBlockVersion-Minor"] = int(version_params[1])
        config["LastKnownBlockVersion-Alt"] = int(version_params[2])
        with open(config_file, "w") as out_json:
            json.dump(config, out_json)





class CardanoCLIWrapper:
    """Cardano CLI Wrapper."""

    def __init__(self, network_magic, state_dir, shelley_keys="keys", byron_keys="keys", protocol="shelley"):
        self.network_magic = network_magic

        self.state_dir = Path(state_dir).expanduser().absolute()
        self.shelley_genesis_json = self.state_dir / shelley_keys / "genesis.json"
        self.shelley_genesis_vkey = self.state_dir / shelley_keys / "genesis-keys" / "genesis1.vkey"
        self.shelley_delegate_skey = self.state_dir / shelley_keys / "delegate-keys" / "delegate1.skey"
        self.pparams_file = self.state_dir / "pparams.json"

        self.check_state_dir()

        with open(self.shelley_genesis_json) as in_json:
            self.shelley_genesis = json.load(in_json)

        self.pparams = None
        if protocol == "shelley":
            self.refresh_pparams()
            self.slot_length = self.shelley_genesis["slotLength"]
            self.epoch_length = self.shelley_genesis["epochLength"]


    def check_state_dir(self):
        if not self.state_dir.exists():
            raise CardanoCLIError(f"The state dir `{self.state_dir}` doesn't exist.")

        for file_name in (
            self.shelley_genesis_json,
            self.shelley_genesis_vkey,
            self.shelley_delegate_skey,
        ):
            if not file_name.exists():
                raise CardanoCLIError(f"The file `{file_name}` doesn't exist.")

    @staticmethod
    def cli(cli_args):
        p = subprocess.Popen(cli_args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        if p.returncode != 0:
            raise CardanoCLIError(f"An error occurred running a CLI command `{p.args}`: {stderr}")
        return stdout

    @staticmethod
    def prepend_flag(flag, contents):
        return sum(([flag, x] for x in contents), [])

    def query_cli(self, cli_args):
        return self.cli(
            [
                "cardano-cli",
                "shelley",
                "query",
                *cli_args,
                "--testnet-magic",
                str(self.network_magic),
            ]
        )

    def refresh_pparams(self):
        self.query_cli(["protocol-parameters", "--out-file", str(self.pparams_file)])
        with open(self.pparams_file) as in_json:
            self.pparams = json.load(in_json)

    def estimate_fee(self, txbody_file, txins=1, txouts=1, witnesses=1, byron_witnesses=0):
        self.refresh_pparams()
        stdout = self.cli(
            [
                "cardano-cli",
                "shelley",
                "transaction",
                "calculate-min-fee",
                "--testnet-magic",
                str(self.network_magic),
                "--protocol-params-file",
                str(self.pparams_file),
                "--tx-in-count",
                str(txins),
                "--tx-out-count",
                str(txouts),
                "--byron-witness-count",
                str(byron_witnesses),
                "--witness-count",
                str(witnesses),
                "--tx-body-file",
                str(txbody_file),
            ]
        )
        fee, __ = stdout.decode().split(" ")
        return int(fee)

    def get_tx_fee(
        self, txins=None, txouts=None, certificates=None, signing_keys=None, proposal_file=None,
    ):
        txins = txins or []
        txouts_copy = copy(txouts) if txouts else []
        signing_keys = signing_keys or []
        certificates = certificates or []

        # TODO: calculate from current tip
        ttl = 100000

        # TODO: unhardcode genesis utxo
        change = (self.shelley_genesis_utxo_addr, 0)
        txouts_copy.append(change)

        txins_combined = [f"{x[0]}#{x[1]}" for x in txins]
        txouts_combined = [f"{x[0]}+{x[1]}" for x in txouts_copy]

        txin_args = self.prepend_flag("--tx-in", txins_combined)
        txout_args = self.prepend_flag("--tx-out", txouts_combined)
        cert_args = self.prepend_flag("--certificate-file", certificates)

        build_args_estimate = [
            "cardano-cli",
            "shelley",
            "transaction",
            "build-raw",
            "--ttl",
            str(ttl),
            "--fee",
            "0",
            "--out-file",
            "tx.body_estimate",
            *txin_args,
            *txout_args,
            *cert_args,
        ]

        if proposal_file:
            build_args_estimate.extend(["--update-proposal-file", proposal_file])

        # Build TX for estimate
        self.cli(build_args_estimate)

        # Estimate fee
        fee = self.estimate_fee(
            "tx.body_estimate",
            txins=len(txins),
            txouts=len(txouts_copy),
            witnesses=len(signing_keys),
        )

        return fee

    # TODO: add withdrawal support
    def build_tx(
        self,
        out_file="tx.body",
        txins=None,
        txouts=None,
        certificates=None,
        fee=0,
        proposal_file=None,
    ):
        txins = txins or []
        txouts_copy = copy(txouts) if txouts else []
        certificates = certificates or []

        # TODO: make change_address work - Needs CLI endpoint to query utxo by txid
        # If no change_address specified send to utxo change address
        # if not change_address:
        #    change_address = self.shelley_genesis_utxo_addr

        # TODO: calculate from current tip
        ttl = 100000

        # TODO: unhardcode genesis utxo
        change = (self.shelley_genesis_utxo_addr, 0)
        txouts_copy.append(change)

        deposit_amount = 0
        total_input_amount = functools.reduce(lambda x, y: x + y[2], txins, 0)
        txouts_copy[-1] = (self.shelley_genesis_utxo_addr, (total_input_amount - fee - deposit_amount))
        txins_combined = [f"{x[0]}#{x[1]}" for x in txins]
        txouts_combined = [f"{x[0]}+{x[1]}" for x in txouts_copy]

        txin_args = self.prepend_flag("--tx-in", txins_combined)
        txout_args = self.prepend_flag("--tx-out", txouts_combined)
        cert_args = self.prepend_flag("--certificate-file", certificates)

        build_args = [
            "cardano-cli",
            "shelley",
            "transaction",
            "build-raw",
            "--ttl",
            str(ttl),
            "--fee",
            str(fee),
            "--out-file",
            str(out_file),
            *txin_args,
            *txout_args,
            *cert_args,
        ]

        if proposal_file:
            build_args.extend(["--update-proposal-file", proposal_file])

        self.cli(build_args)

    def sign_tx(self, tx_body_file="tx.body", out_file="tx.signed", signing_keys=None):
        signing_keys = signing_keys or []
        key_args = self.prepend_flag("--signing-key-file", signing_keys)
        self.cli(
            [
                "cardano-cli",
                "shelley",
                "transaction",
                "sign",
                "--tx-body-file",
                str(tx_body_file),
                "--out-file",
                str(out_file),
                "--testnet-magic",
                str(self.network_magic),
                *key_args,
            ]
        )

    def submit_tx(self, tx_file="tx.signed"):
        self.cli(
            [
                "cardano-cli",
                "shelley",
                "transaction",
                "submit",
                "--testnet-magic",
                str(self.network_magic),
                "--tx-file",
                str(tx_file),
            ]
        )

    def get_payment_address(self, payment_vkey, stake_vkey=None):
        if not payment_vkey:
            raise CardanoCLIError("Must set payment key.")

        cli_args = ["--payment-verification-key-file", str(payment_vkey)]
        if stake_vkey:
            cli_args.extend("--stake-verification-key-file", str(stake_vkey))

        return (
            self.cli(
                [
                    "cardano-cli",
                    "shelley",
                    "address",
                    "build",
                    "--testnet-magic",
                    str(self.network_magic),
                    *cli_args,
                ]
            )
            .rstrip()
            .decode("ascii")
        )

    def get_genesis_addr(self, vkey_path):
        return (
            self.cli(
                [
                    "cardano-cli",
                    "shelley",
                    "genesis",
                    "initial-addr",
                    "--testnet-magic",
                    str(self.network_magic),
                    "--verification-key-file",
                    str(vkey_path),
                ]
            )
            .rstrip()
            .decode("ascii")
        )

    def get_utxo(self, address):
        self.query_cli(["utxo", "--address", address, "--out-file", "utxo.json"])
        with open("utxo.json") as in_json:
            utxo = json.load(in_json)
        return utxo

    def get_tip(self):
        return json.loads(self.query_cli(["tip"]))

    def send_tx_genesis(
        self, txouts=None, certificates=None, signing_keys=None, proposal_file=None,
    ):
        txouts = txouts or []
        certificates = certificates or []
        signing_keys = signing_keys or []

        utxo = self.get_utxo(address=self.shelley_genesis_utxo_addr)
        total_input_amount = 0
        txins = []
        for k, v in utxo.items():
            total_input_amount += v["amount"]
            txin = k.split("#")
            txin = (txin[0], txin[1])
            txins.append(txin)

        # TODO: calculate from current tip
        signing_keys.append(str(self.shelley_genesis_utxo_skey))
        utxo = self.get_utxo(address=self.shelley_genesis_utxo_addr)
        txins = []
        for k, v in utxo.items():
            txin = k.split("#")
            txin = (txin[0], txin[1], v["amount"])
            txins.append(txin)

        # Build, Sign and Send TX to chain
        try:
            fee = self.get_tx_fee(txins, txouts, certificates, signing_keys, proposal_file)
            self.build_tx(
                txins=txins,
                txouts=txouts,
                certificates=certificates,
                fee=fee,
                proposal_file=proposal_file,
            )
            self.sign_tx(signing_keys=signing_keys)
            self.submit_tx()
        except CardanoCLIError as err:
            raise CardanoCLIError(
                f"Sending a genesis transaction failed!\n"
                f"utxo: {utxo}\n"
                f"txins: {txins} txouts: {txouts} signing keys: {signing_keys}\n{err}"
            )

    def submit_update_proposal(self, cli_args, epoch=1):
        self.cli(
            [
                "cardano-cli",
                "shelley",
                "governance",
                "create-update-proposal",
                *cli_args,
                "--out-file",
                "update.proposal",
                "--epoch",
                str(epoch),
                "--genesis-verification-key-file",
                str(self.shelley_genesis_vkey),
            ]
        )
        self.send_tx_genesis(
            proposal_file="update.proposal", signing_keys=[str(self.shelley_delegate_skey)],
        )
    # Byron specific commands
    def byron_txin_format(self, tx):
        return "(\"{}\",{})".format(tx[0],tx[1])

    def byron_txout_format(self, fund):
        return "(\"{}\",{})".format(fund["address"], fund["value"])

    # TODO:
    def byron_generate_tx_slice(self, start, slice_size, tx_filename, snapshot, wallet, fee):
        s = slice(start, start+slice_size)
        records = snapshot[s]
        txouts = list(map( lambda x: getTxOut(x), records))
        txouts_total = sum(map(lambda x: x["value"], records))

        wallet["value"] = wallet["value"] - txouts_total - fee
        txout_args = self.prependFlag("--txout", txouts)
        self.CLI(["cardano-cli", "issue-utxo-expenditure", "--tx", tx_filename, "--testnet-magic", str(self.networkMagic), "--byron-formats", "--txin", getTxIn(current_tx), "--wallet-key", str(wallet["key"]), "--txout", getTxOut(wallet) ] + txout_args)
        return wallet
