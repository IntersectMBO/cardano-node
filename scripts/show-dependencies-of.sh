#!/usr/bin/env bash

# This script shows all unique Hackage dependencies introduced by particular package.
# For example, if we run this script in the root of 'cardano-node' repository like this
#
# ./show-dependencies-of.sh lobemo-backend-ekg
#
# it shows the list of all unique dependencies introduced by 'lobemo-backend-ekg' package only.
#
# The mechanism is simple:
# 1. take all dependencies of the project with target package, using 'cabal freeze'.
# 2. take all dependencies of the project without target package, using 'cabal freeze'.
# 3. take the unique diff between these two lists - it will be the list of packages introduced by target package only.

readonly PACKAGE=$1

readonly DEPS_FILE=cabal.project.freeze
readonly DEPS_FILE_WITH_PACKAGE=/tmp/$DEPS_FILE.with
readonly DEPS_FILE_WITHOUT_PACKAGE=/tmp/$DEPS_FILE.without
readonly UNIQUE_DEPS=/tmp/dependencies.unique

readonly ALL_CABAL_FILES=$(find . -name '*.cabal')

# Cleanup old dependencies files...
rm -f $DEPS_FILE_WITH_PACKAGE $DEPS_FILE_WITHOUT_PACKAGE $UNIQUE_DEPS

collect_deps () {
  # Create the list of all unique Hackage dependencies for the project at this moment.
  cabal freeze 2>&1 >/dev/null
  mv $DEPS_FILE $1
}

echo "Check unique dependencies introduced by ${PACKAGE} package..."

# Collect all unique Hackage dependencies with ${PACKAGE}.
collect_deps $DEPS_FILE_WITH_PACKAGE

# Remove the ${PACKAGE} from all cabal files...
for cabalFile in $ALL_CABAL_FILES; do
  grep -v "${PACKAGE}" $cabalFile > temp && mv temp $cabalFile
done

# Collect all unique Hackage dependencies without ${PACKAGE}. So, all unique Hackage dependencies introduced by ${PACKAGE} will be removed.
collect_deps $DEPS_FILE_WITHOUT_PACKAGE

# Take the raw diff between two lists of unique Hackage dependencies.
diff --new-line-format="" --unchanged-line-format="" $DEPS_FILE_WITH_PACKAGE $DEPS_FILE_WITHOUT_PACKAGE > $UNIQUE_DEPS

# Remove unnecessary any. prefixes.
sed -i 's/any.//' $UNIQUE_DEPS

# Remove all except the name of Hackage package.
awk '{ print $1 }' $UNIQUE_DEPS > temp && mv temp $UNIQUE_DEPS

# Remove all duplicated packages if needed.
sort -u $UNIQUE_DEPS > temp && mv temp $UNIQUE_DEPS

# Add list prefixes in the beginning of each line.
sed -i 's/^/ - /' $UNIQUE_DEPS

# Count the number of unique Hackage packages introduced by ${PACKAGE} only.
readonly UNIQUE_DEPS_NUM=$(wc -l $UNIQUE_DEPS | awk '{ print $1 }')
echo "The number of unique dependencies is ${UNIQUE_DEPS_NUM}, they are:"
cat $UNIQUE_DEPS

# Restore the state of all cabal files...
for cabalFile in $ALL_CABAL_FILES; do
  git restore $cabalFile
done
