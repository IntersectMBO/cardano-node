#!/usr/bin/env bash

# This file extracts HLINT rules in '*.hs' files and injects them into the .hlint.yaml file
# to work around the hlint issue with haskell-language-server documented here:
#
#     https://github.com/haskell/haskell-language-server/issues/638
#
# To use, simply run the script from the project's top-level directory.

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac

extract_rules() {
  for x in $(find . -type f -name '*.hs' | grep -v dist); do
    module="$(grep '^module' $x | cut -d ' ' -f 2 | head -n 1)"
    grep '{- HLINT ignore "' $x | cut -d '"' -f 2 | \
    $SED "s|^|$module,|g"
  done
}

gen_rules() {
  extract_rules | sort | while read line; do
    module="$(echo "$line" | cut -d ',' -f 1)"
    rule="$(echo "$line" | cut -d ',' -f 2)"
    echo "- ignore:"
    echo "    name: \"$rule\""
    echo "    within: $module"
  done
}

(
  echo "# This file is generated from .hlint.template.yaml by scripts/reconfigure-hlint.sh"
  echo ""
  cat .hlint.template.yaml \
    | $SED '/^# BEGIN-GENERATED/,/^# END-GENERATED/{/^# BEGIN-GENERATED/!{/^# END-GENERATED/!d}}' \
    | $SED -e "/^# BEGIN-GENERATED/ r "<(gen_rules)
) > ".hlint.yaml"
