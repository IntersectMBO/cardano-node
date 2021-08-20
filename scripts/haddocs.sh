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
DRY_RUN="${DRY_RUN:-}"

GHC_VERSION=$(ghc --numeric-version)
OS_ARCH="$(cat "$BUILD_DIR/cache/plan.json" | jq -r '.arch + "-" + .os' | head -n 1 | xargs)"

if [ "${DRY_RUN}" == 1 ]; then
  DRY_RUN_ARGS="--dry-run"
else
  DRY_RUN_ARGS=""
fi

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
    --disable-tests
    $DRY_RUN_ARGS
  )

# build documentation of all modules
if [ ${REGENERATE} == "true" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" \
    cardano-api \
    cardano-cli \
    cardano-config \
    cardano-node \
    hedgehog-extras \
    exe:cardano-cli \
    exe:cardano-node \
    exe:cardano-node-chairman \
    exe:cardano-testnet
elif [ ${REGENERATE} != "false" ]; then
  cabal haddock "${HADDOCK_OPTS[@]}" ${REGENERATE}
fi

if [ "${DRY_RUN}" == 1 ]; then
  echo "Exiting dry run"
  exit 0
fi

if [[ !( -d ${OUTPUT_DIR} ) ]]; then
  mkdir -p ${OUTPUT_DIR}
fi

mkdir -p "${OUTPUT_DIR}/_plan/"

# Build plan
for row in $(
  cat dist-newstyle/cache/plan.json | \
    jq -r '
      .
      | ."install-plan"[]
      | select(.style == "local")
      | { "dist-dir": ."dist-dir"
        , "pkg-name": ."pkg-name"
        , "component-name": ."component-name" | split(":") | join("_")
        }
      | @base64'
); do
  json="$(echo "${row}" | base64 --decode)"

  dist_dir="$(echo "$json" | jq -r '."dist-dir"')"

  if [ -d "${dist_dir}" ]; then
    echo "$json" > "${OUTPUT_DIR}/_plan/$(echo "$json" | sha256sum | cut -d ' ' -f 1).json"
  fi
done

# Copy the docs
for json in "${OUTPUT_DIR}"/_plan/*.json; do
  dist_dir="$(cat "$json" | jq -r '."dist-dir"')"
  pkg_name="$(cat "$json" | jq -r '."pkg-name"')"
  component_name="$(cat "$json" | jq -r '."component-name"')"

  if [ -d "${dist_dir}" ]; then
    for doc_index in $(find "${dist_dir}" -name doc-index.html); do
      package_dir="$(dirname "$doc_index")"
      package="$(echo "$(basename "${package_dir}")" | sed 's/-[0-9]\+\(\.[0-9]\+\)*//')"
      echo "Copying package: ${package}"
      mkdir -p "${OUTPUT_DIR}/${pkg_name}/${component_name}"
      cp -r "${package_dir}"/* "${OUTPUT_DIR}/${pkg_name}/${component_name}"
      echo "${OUTPUT_DIR}/${pkg_name}/${component_name}"
    done
  fi
done

# --read-interface options
interface_options () {
  for json in "${OUTPUT_DIR}"/_plan/*.json; do
    dist_dir="$(cat "$json" | jq -r '."dist-dir"')"
    pkg_name="$(cat "$json" | jq -r '."pkg-name"')"
    component_name="$(cat "$json" | jq -r '."component-name"')"

    if [ -d "${dist_dir}" ]; then
      for doc_index in $(find "${dist_dir}" -name doc-index.html); do
        package_dir="$(dirname "$doc_index")"
        package="$(echo "$(basename "${package_dir}")" | sed 's/-[0-9]\+\(\.[0-9]\+\)*//')"
        if [ -f "${OUTPUT_DIR}/${pkg_name}/${component_name}/${package}.haddock" ]; then
          echo "--read-interface=${pkg_name}/${component_name},${OUTPUT_DIR}/${pkg_name}/${component_name}/${package}.haddock"
        fi
      done
    fi
  done
}

haddock \
  -o ${OUTPUT_DIR} \
  --title "cardano-node API" \
  --package-name "Cardano Node API" \
  --gen-index \
  --gen-contents \
  --quickjump \
  $(interface_options)

# Assemble a toplevel `doc-index.json` from package level ones.
#
echo "[]" > "${OUTPUT_DIR}/doc-index.json"
for file in $(ls $OUTPUT_DIR/*/*/doc-index.json); do
  project=$(basename $(dirname $file));
  jq -s \
    ".[0] + [.[1][] | (. + {link: (\"${project}/\" + .link)}) ]" \
    "${OUTPUT_DIR}/doc-index.json" \
    ${file} \
    > /tmp/doc-index.json
  mv /tmp/doc-index.json "${OUTPUT_DIR}/doc-index.json"
done
