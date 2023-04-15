#!/bin/bash

TOKEN="$GITHUB_TOKEN"
REPO_OWNER="input-output-hk"
REPO_NAME="cardano-node"
START_DATE="2022-06-25"
END_DATE="2023-04-17"
OUTPUT_FILE="merged_prs.md"

# Initialize output file
echo "# Merged PRs between $START_DATE and $END_DATE" > $OUTPUT_FILE

# Function to fetch merged PRs from a specific page
fetch_merged_prs_page() {
  PAGE=$1
  curl -s -H "Authorization: token $TOKEN" -H "Accept: application/vnd.github+json" "https://api.github.com/repos/$REPO_OWNER/$REPO_NAME/pulls?state=closed&sort=updated&direction=desc&per_page=100&page=$PAGE" | jq '.[] | select(.merged_at != null) | select(.merged_at >= "'$START_DATE'T00:00:00Z") | select(.merged_at <= "'$END_DATE'T23:59:59Z") | "[" + .title + "](" + .html_url + ")"' -r
}

# Fetch merged PRs with pagination
PAGE=1
while true; do
  PRS=$(fetch_merged_prs_page $PAGE)
  if [ -z "$PRS" ]; then
    break
  fi
  echo "$PRS" >> $OUTPUT_FILE
  PAGE=$((PAGE+1))
done

echo "Output written to $OUTPUT_FILE"
