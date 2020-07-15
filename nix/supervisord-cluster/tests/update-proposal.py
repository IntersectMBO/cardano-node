import sys
import time

from base import cluster

sleep_time = cluster.slot_length * cluster.epoch_length
print(f"Waiting 1 epoch to submit proposal ({sleep_time} seconds)")
time.sleep(sleep_time)
cluster.submit_update_proposal(["--decentralization-parameter", "0.5"])
print(f"Update Proposal submited. Sleeping until next epoch ({sleep_time} seconds)")
time.sleep(sleep_time + 15)
cluster.refresh_pparams()
d = cluster.pparams["decentralisationParam"]
if d == 0.5:
    print("Cluster update proposal successful!")
    sys.exit(0)
else:
    print(f"Cluster update proposal failed! Expected 0, got {d}")
    print(cluster.get_tip())
    sys.exit(1)
