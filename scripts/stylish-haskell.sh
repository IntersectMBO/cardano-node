#!/usr/bin/env bash

# The top-level directory for the project.
REPO_TOP_DIR=${REPO_TOP_DIR:=$(git rev-parse --show-toplevel)}

# The YAML configuration file for stylish-haskell.
SH_YAML=${REPO_TOP_DIR}/.stylish-haskell.yaml

# Starting from all the cabal files in the project, found via git, this
# descends into the directory containing the cabal file so that
# stylish-haskell can find it. Its search strategy is to ascend from its
# current directory at the time of invocation, not to scrutinise the
# path of the file given. So this strategy is required to accommodate
# it. And it needs the cabal files for language extensions affecting
# parsing for code reformatting.
for cabal_file in $(git ls-files '*.cabal')
do
	# REPO_TOP_DIR makes the path absolute, so there are no issues
	# with having to cd back to a starting point at a loop end etc.
	cd ${REPO_TOP_DIR}/$(dirname ${cabal_file});			\
	stylish-haskell -c ${SH_YAML}					\
			-i $(git ls-files '*.hs' '*.lhs')		\
		|| exit ${?}
done
