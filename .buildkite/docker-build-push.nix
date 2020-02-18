# This script will load nix-built docker images of cardano-node
# into the Docker daemon (must be running), and then push to the
# Docker Hub. Credentials for the hub must already be installed with
# "docker login".
#
# There is a little bit of bash logic to replace the default repo and
# tag from the nix-build (../nix/docker.nix).
#
# 1. The repo (default "inputoutput/cardano-node") is changed to match
#    the logged in Docker user's credentials.
#
# 2. The tag (default "VERSION") is changed to reflect the
#    branch which is being built under this Buildkite pipeline.
#
#    - All commits are tagged with commit hash.
#    - If the branch is master then master tag is updated
#    - If there is a tag associated with the commit then that tag is pushed as well

{ nodePackages ?  import ../. {}

# Build system's Nixpkgs. We use this so that we have the same docker
# version as the docker daemon.
, hostPkgs ? import <nixpkgs> {}

# Dockerhub repository for image tagging.
, dockerHubRepoName ? null
}:

with hostPkgs;
with hostPkgs.lib;

let
  images = map impureCreated [
    nodePackages.dockerImage.base
    nodePackages.dockerImage.testnet
    nodePackages.dockerImage.mainnet
  ];

  # Override Docker image, setting its creation date to the current time rather than the unix epoch.
  impureCreated = image: image.overrideAttrs (oldAttrs: { created = "now"; }) // { inherit (image) version; };

in
  writeScript "docker-build-push" (''
    #!${runtimeShell}

    set -euo pipefail

    export PATH=${lib.makeBinPath [ docker gnused ]}

    ${if dockerHubRepoName == null then ''
    reponame=cardano-node
    username="$(docker info | sed '/Username:/!d;s/.* //')"
    fullrepo="$username/$reponame"
    '' else ''
    fullrepo="${dockerHubRepoName}"
    ''}

  '' + concatMapStringsSep "\n" (image: ''
    branch="''${BUILDKITE_BRANCH:-}"
    tag="''${BUILDKITE_TAG:-}"
    tagged="$fullrepo:$tag"
    gitrev="${image.imageTag}"
    echo "Loading $fullrepo:$gitrev"
    docker load -i ${image}
    echo "Pushing $fullrepo:$gitrev"
    docker push "$fullrepo:$gitrev"
    if [[ "$branch" = master ]]; then
      echo "Tagging as master"
      docker tag $fullrepo:$gitrev $fullrepo:$branch
      echo "Pushing $fullrepo:$branch"
      docker push "$fullrepo:$branch"
    fi
    if [[ "$tag" ]]; then
      echo "Tagging as ${image.imageTag}"
      docker tag $fullrepo:$gitrev $fullrepo:$tag
      echo "Pushing $fullrepo:$tag"
      docker push "$fullrepo:$tag"
    fi
  '') images)
