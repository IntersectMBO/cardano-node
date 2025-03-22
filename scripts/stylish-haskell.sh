#!/usr/bin/env bash

# The top-level directory for the project.
REPO_TOP_DIR=${REPO_TOP_DIR:=$(git rev-parse --show-toplevel)}

# The YAML configuration file for stylish-haskell.
SH_YAML=${REPO_TOP_DIR}/.stylish-haskell.yaml

# Set the return code variable to a default of success. The global scope
# doesn't actually help the two groups communicate because the pipe
# demands subshells. So it has to be communicated over file descriptors.
rc=0

# Get free file descriptors to pass the exit code between
# subshells and a temporary fd to swap stdout and stderr.
eval "exec {rcfd}<> <(:)"
eval "exec {swpfd}<> <(:)"

# Starting from all the cabal files in the project, found via git, this
# descends into the directory containing the cabal file so that
# stylish-haskell can find it. Its search strategy is to ascend from its
# current directory at the time of invocation, not to scrutinise the
# path of the file given. So this strategy is required to accommodate
# it. And it needs the cabal files for language extensions affecting
# parsing for code reformatting.
{
	for cabal_file in $(git ls-files '*.cabal')
	do
		# REPO_TOP_DIR makes the path absolute, so there are no
		# issues with having to cd back to a starting point at a
		# loop end etc.
		cd ${REPO_TOP_DIR}/$(dirname ${cabal_file});		\
		stylish-haskell						\
			-c ${SH_YAML}					\
			-i $(git ls-files '*.hs' '*.lhs')
		shrc=${?}
		if [ ${shrc} -ne 0 ]
		then
			rc=${shrc}
			break
		fi
	done

	# Pass the exit code through one anonymous fd.
	echo "${rc}" 1>&${rcfd}

# Swap stdout and stderr to post-process stderr via the other.
} {swpfd}>&1 1>&2 2>&${swpfd}						\
| {

	# read tokenises according to IFS, so that changing IFS to
	# be stylish-haskell's delimiter for the file name at the
	# beginning of error report lines will yield the file name to
	# be re-relativised to be relative to the top directory of
	# the project.
	OLDIFS=${IFS}
	IFS=":"
	while read fpath rest
	do
		# git happily has utility functions for both carrying
		# out a search to check whether a string is a path to
		# a git-controlled file and re-relativising file path
		# names relative to the project root. If we discover
		# that it is under git's control, the rewritten path
		# gets put into the reassembled stderr line.
		if git ls-files --error-unmatch				\
				  "*/${fpath}"				\
					> /dev/null 2>&1
		then
			fpath=$(git ls-files				\
				    --full-name				\
				    "*/${fpath}")
		fi
		IFS="${OLDIFS}"
		echo "${fpath}:${rest}"
		IFS=":"
		done

  # Swap stdout and stderr back to normal.
  } {swpfd}>&1 1>&2 2>&${swpfd}
read -u ${rcfd} rc
exit ${rc}
