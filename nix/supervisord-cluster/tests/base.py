"""Usage: cluster-test.py [--network-magic=MAGIC] [--state-dir=DIR]

Options:
    -n, --network-magic <magic>  network magic [default: 42]
    -s, --state-dir <dir>  state directory [default: "./state-cluster-test"]
"""

from docopt import docopt
from clusterlib import ClusterLib

arguments = docopt(__doc__)
cluster = ClusterLib(arguments['--network-magic'], arguments["--state-dir"])
cluster.refresh_pparams()
