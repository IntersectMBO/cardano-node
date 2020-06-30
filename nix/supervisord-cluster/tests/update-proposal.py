from base import cluster
import time

sleep_time = cluster.slotLength * cluster.epochLength
print(f"Waiting 1 epoch to submit proposal ({sleep_time} seconds)")
time.sleep(sleep_time)
cluster.submitUpdateProposal(["--decentralization-parameter", "0.5"])
print(f"Update Proposal submited. Sleeping until next epoch ({sleep_time} seconds)")
time.sleep(sleep_time + 15)
cluster.refreshPParams()
d = cluster.pparams["decentralisationParam"]
if d == 0.5:
    print("Cluster update proposal successful!")
    exit(0)
else:
    print(f"Cluster update proposal failed! Expected 0, got {d}")
    cluster.printTip()
    exit(1)
