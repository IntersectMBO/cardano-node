#!/usr/bin/env nix-shell
#!nix-shell -p python3Packages.requests -i python3

import requests
import os
import time
import json

HYDRA_BASE_URL = "https://hydra.iohk.io"

BUILDKITE_BRANCH = os.getenv("BUILDKITE_BRANCH", None)
BUILDKITE_PR = os.getenv("BUILDKITE_PULL_REQUEST", None)
BUILDKITE_REPO = os.getenv("BUILDKITE_PIPELINE_NAME", None)

hydra_pr_url = f"{HYDRA_BASE_URL}/jobset/Cardano/{BUILDKITE_REPO}-pr-{BUILDKITE_PR}"

if BUILDKITE_PR == "false":
    print("Please open a PR for hydra to evaluate")
    exit(1)
else:
    print(f"PR: {BUILDKITE_PR}")

hydra_eval = requests.get(hydra_pr_url, headers={"Content-Type": "application/json"})

retry_count = 0
while hydra_eval.status_code != 200:
    print("Hydra PR not created yet - sleeping 1 minute")
    retry_count = retry_count + 1
    time.sleep(60)
    hydra_eval = requests.get(hydra_pr_url, headers={"Content-Type": "application/json"})
    if retry_count > 60:
        print("Retried 1 hour - exiting")
        exit(1)

hydra_eval_data = json.loads(hydra_eval.text)
errormsg = hydra_eval_data["errormsg"]

if errormsg != "":
    print(f"An error occurred in evaluation:\n{errormsg}")
    exit(1)

exit(0)
