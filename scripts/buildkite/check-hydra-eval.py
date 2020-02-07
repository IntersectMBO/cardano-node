#!/usr/bin/env nix-shell
#!nix-shell -p python3Packages.requests -i python3

import requests
import os

HYDRA_BASE_URL = "https://hydra.iohk.io"

BUILDKITE_BRANCH = os.getenv("BUILDKITE_BRANCH", None)
BUILDKITE_PR = os.getenv("BUILDKITE_PULL_REQUEST", None)
BUILDKITE_REPO = os.getenv("BUILDKITE_PIPELINE_NAME", None)

hydra_pr_url = f"{HYDRA_BASE_URL}/jobset/Cardano/{BUILDKITE_REPO}-pr-{BUILDKITE_PR}"


print(f"{hydra_pr_url}")
