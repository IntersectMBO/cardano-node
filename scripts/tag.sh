#!/usr/bin/env bash

set -euo pipefail

main_branch="main"

if [[ -n "$(git status --porcelain --untracked-files=no)" ]]; then
  echo -e "\e[31mRefusing to run because ther are untracked changes in the repository.\e[0m"
  exit 1
fi

cabal build all --dry-run > /dev/null

components_file="$(mktemp)-version.json"

cat dist-newstyle/cache/plan.json \
  | jq -rc '
        ."install-plan"[]
      | select(.style == "local" and ."component-name" == "lib")
      | { "component": ."component-name"
        , "name": ."pkg-name"
        , "version": ."pkg-version"
        }
      ' > "$components_file"

mapfile -t lines < "$components_file"

git fetch origin --tags > /dev/null 2> /dev/null

# Iterate over each line in the array
for line in "${lines[@]}"; do
  # Do something with each line
  name="$(echo "$line" | jq -r '.name')"
  tag="$(echo "$line" | jq -r '.name + "-" + .version')"

  head_commit="$(git rev-parse --quiet --verify HEAD)"
  tag_commit="$(git rev-parse --quiet --verify "refs/tags/$tag" || true)"
  remote_commit="$(git ls-remote --quiet origin --verify "refs/tags/$tag" | awk '{print $1}' || true)"

  if [ "$tag_commit" == "" ]; then
    if [ "$remote_commit" == "" ]; then
      if git branch -r --contains "$head_commit" | sed 's|^ \+||g' | cut -d ' ' -f 1 | grep -q "^origin/\\($main_branch\\|$name-[0-9].*-branch\\)$"; then
        git tag "$tag" > /dev/null 2> /dev/null
        git push origin "$tag" > /dev/null 2> /dev/null
        echo -e "\e[32m$tag created and pushed.\e[0m"
      else
        echo -e "\e[31m$tag error.  $main_branch branch nor any release branches contain commit $head_commit.\e[0m"
      fi
    else
      echo "$remote_commit"
      git pull origin "$tag"
      echo -e "\e[32m$tag pulled from remote.\e[0m"
    fi
  else
    if [ "$remote_commit" == "" ]; then
      echo -e "\e[31m$tag remote missing. Check if the existing tag is correct.  If so, push it manually.\e[0m"
    else
      if [ "$tag_commit" == "$remote_commit" ]; then
        if [ "$tag_commit" != "$head_commit" ]; then
          echo -e "\e[33m$tag skipped.  Tag already exists on another commit. $tag_commit\e[0m"
        else
          echo -e "\e[32m$tag skipped.  Tag already exists.\e[0m"
        fi
      else
        echo -e "\e[31m$tag error.  Inconsistent tag and remote tag found. $tag_commit != $remote_commit\e[0m"
      fi
    fi
  fi
done
