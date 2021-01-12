#!/bin/bash
# Build haddock documentation and an index for all projects in the repository.
#
# usage:
# ./haddocks.sh directory [true|false]
#
# $1 - where to put the generated pages, this directory contents will be wiped
#      out (so don't pass `/` or `./` - the latter will delete your 'dist-newstyle')
#      (the default is './haddocks')
# $2 - weahter to re-build haddocjs with `cabal haddock` command or a component name
#      (the default is true)
# $3 - cabal build directory
#      (the default is "dist-newstyle")

set -euo pipefail

OUTPUT_DIR=${1:-"./haddocks"}
REGENERATE=${2:-"true"}
BUILD_DIR=${3:-"dist-newstyle"}

GHC_VERSION=$(ghc --numeric-version)
OS_ARCH="$(cat "$BUILD_DIR/cache/plan.json" | jq -r '.arch + "-" + .os' | head -n 1 | xargs)"


# we don't include `--use-index` option, because then quickjump data is not
# generated.  This is not ideal, but there is no way to generate only top level
# `doc-index.html` file.  With this approach we get:
# * `doc-index.json` and `doc-index.html` per package
# * we can generate top level `doc-index.json` (which will only work at the top
#   level).
# * we could ammend package level `doc-index.json` files, but it's enough ...
#   this should be fixed upstream.
HADDOCK_OPTS=(
    --builddir "${BUILD_DIR}"
    --disable-optimization
    --haddock-all
    --haddock-internal
    --haddock-html
    --haddock-quickjump
    --haddock-hyperlink-source
    --haddock-option "--show-all"
    --haddock-option "--use-unicode"
    --haddock-option "--use-contents=\"../index.html\""
  )

# build documentation of all modules
if [ ${REGENERATE} == "true" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" cardano-api
elif [ ${REGENERATE} != "false" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" ${REGENERATE}
fi

if [[ !( -d ${OUTPUT_DIR} ) ]]; then
  mkdir -p ${OUTPUT_DIR}
fi

# copy the new docs
for noopt_dir in $(ls "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}"/noopt); do
  dir="$(dirname "$noopt_dir")"
  package=$(echo "${dir}" | sed 's/-[0-9]\+\(\.[0-9]\+\)*//')
  cp -r "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}/${dir}/noopt/doc/html/${package}" ${OUTPUT_DIR}
done

# --read-interface options
interface_options () {
  for package in $(ls "${OUTPUT_DIR}"); do
    echo "--read-interface=${package},${OUTPUT_DIR}/${package}/${package}.haddock"
  done
}

# Generate top level index using interface files
#
haddock \
  -o ${OUTPUT_DIR} \
  --title "cardano-node API" \
  --package-name "Cardano Node API" \
  --gen-index \
  --gen-contents \
  --quickjump \
  --prolog ./scripts/prolog \
  $(interface_options)

# Assemble a toplevel `doc-index.json` from package level ones.
#
echo "[]" > "${OUTPUT_DIR}/doc-index.json"
for file in $(ls $OUTPUT_DIR/*/doc-index.json); do
  project=$(basename $(dirname $file));
  jq -s \
    ".[0] + [.[1][] | (. + {link: (\"${project}/\" + .link)}) ]" \
    "${OUTPUT_DIR}/doc-index.json" \
    ${file} \
    > /tmp/doc-index.json
  mv /tmp/doc-index.json "${OUTPUT_DIR}/doc-index.json"
done
