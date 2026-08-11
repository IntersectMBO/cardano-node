# The "voting-setup" workload (phase "setup") shares its script with the
# "voting" workload (phase "load"): same functions, different entrypoint.
args: import ./voting.nix args
