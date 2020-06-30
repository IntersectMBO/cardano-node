"""Usage: cluster-test.py [--network-magic=MAGIC] [--state-dir=DIR]

Options:
    -n, --network-magic <magic>  network magic [default: 42]
    -s, --state-dir <dir>  state directory [default: "./state-cluster-test"]
"""

from docopt import docopt
from clusterlib import clusterlib

arguments = docopt(__doc__)
cluster = clusterlib(arguments['--network-magic'], arguments["--state-dir"])
cluster.refreshPParams()
