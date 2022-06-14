# # [Dockerfile reference](https://docs.docker.com/engine/reference/builder/)

# Docker can build images automatically by reading the instructions from a
# `Dockerfile`. A `Dockerfile` is a text document that contains all the commands
# a user could call on the command line to assemble an image. Using
# `docker build` users can create an automated build that executes several
# command-line instructions in succession.

# ## [`FROM`](https://docs.docker.com/engine/reference/builder/#from)

# The `FROM` instruction initializes a new build stage and sets the
# [`Base Image`](https://docs.docker.com/glossary/#base-image) for subsequent
# instructions. As such, a valid `Dockerfile` must start with a `FROM`
# instruction. The image can be any valid image – it is especially easy to start
# by ***pulling an image*** from the
# [Public Repositories](https://docs.docker.com/docker-hub/repos/).

# Whenever possible, use current official images as the basis for your images.
# We recommend the [Alpine image](https://hub.docker.com/_/alpine/) as it is
# tightly controlled and small in size (currently under 6 MB), while still being
# a full Linux distribution.

# The optional `--platform` flag can be used to specify the platform of the
# image in case `FROM` references a multi-platform image. For example,
# `linux/amd64`, `linux/arm64`, or `windows/amd64`. By default, the target
# platform of the build request is used. Global build arguments can be used in
# the value of this flag, for example
# [automatic platform ARGs](https://docs.docker.com/engine/reference/builder/#automatic-platform-args-in-the-global-scope)
# allow you to force a stage to native build platform
# (`--platform=$BUILDPLATFORM`), and use it to cross-compile to the target
# platform inside the stage.

#FROM --platform=linux/amd64 alpine:latest
#FROM --platform=linux/amd64 ubuntu:22.04
FROM --platform=linux/amd64 ubuntu:20.04
# Inside a container built with 22.04 I got:
# root@1f03219c653c:/# ldd run/cardano-node
#    linux-vdso.so.1 (0x00007ffdb21ce000)
#    libsystemd.so.0 => /lib/x86_64-linux-gnu/libsystemd.so.0 (0x00007f26bf6f5000)
#    libssl.so.1.1 => not found
#    libcrypto.so.1.1 => not found
#    libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f26bf6d7000)
#    libsodium.so.23 => not found
#    libsecp256k1.so.0 => not found
#    libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f26bf6d2000)
#    librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f26bf6cd000)
#    libutil.so.1 => /lib/x86_64-linux-gnu/libutil.so.1 (0x00007f26bf6c8000)
#    libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f26bf6c1000)
#    libgmp.so.10 => /lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f26bf63f000)
#    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f26bf417000)
#    libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f26bf330000)
#    libffi.so.8 => /lib/x86_64-linux-gnu/libffi.so.8 (0x00007f26bf323000)
#    libnuma.so.1 => not found
#    liblzma.so.5 => /lib/x86_64-linux-gnu/liblzma.so.5 (0x00007f26bf2f6000)
#    libzstd.so.1 => /lib/x86_64-linux-gnu/libzstd.so.1 (0x00007f26bf227000)
#    liblz4.so.1 => /lib/x86_64-linux-gnu/liblz4.so.1 (0x00007f26bf207000)
#    libcap.so.2 => /lib/x86_64-linux-gnu/libcap.so.2 (0x00007f26bf1fc000)
#    libgcrypt.so.20 => /lib/x86_64-linux-gnu/libgcrypt.so.20 (0x00007f26bf0be000)
#    /nix/store/q29bwjibv9gi9n86203s38n0577w09sx-glibc-2.33-117/lib/ld-linux-x86-64.so.2 => /lib64/ld-linux-x86-64.so.2 (0x00007f26bf7c0000)
#    libgpg-error.so.0 => /lib/x86_64-linux-gnu/libgpg-error.so.0 (0x00007f26bf096000)
# Example of how the community managed it:
# https://hub.docker.com/layers/cardano-node/cardanocommunity/cardano-node/latest/images/sha256-2aeeb0b0dde07639c00713042dd77b3bc286eb35f374a71d53ba364e2079c5e0?context=explore
RUN apt-get update
# Installs libssl.so and libssl.so.3 in 22.04
# RUN apt-get --no-install-recommends -y install libssl-dev
RUN apt-get --no-install-recommends -y install libssl1.1
RUN apt-get -y purge && apt-get -y clean && apt-get -y autoremove
RUN rm -rf /var/lib/apt/lists/*

# ## [`USER`](https://docs.docker.com/engine/reference/builder/#user)

# The `USER` instruction sets the user name (or UID) and optionally the user
# group (or GID) to use when running the image and for any `RUN`, `CMD` and
# `ENTRYPOINT` instructions that follow it in the `Dockerfile`.

USER root:root

# ## [`WORKDIR`](https://docs.docker.com/engine/reference/builder/#workdir)

# The `WORKDIR` instruction sets the working directory for any `RUN`, `CMD`,
# `ENTRYPOINT`, `COPY` and `ADD` instructions that follow it in the
# `Dockerfile`. If the `WORKDIR` doesn’t exist, it will be created even if it’s
# not used in any subsequent `Dockerfile` instruction.

WORKDIR /

# # [`ARG`](https://docs.docker.com/engine/reference/builder/#arg)

# The `ARG` instruction defines a variable that users can pass at build-time to
# the builder with the `docker build` command using the
# `--build-arg <varname>=<value>` flag. If a user specifies a build argument
# that was not defined in the Dockerfile, the build outputs a warning.

# An ARG instruction can optionally include a default value.

ARG NODE_ID=

# ## `ADD` and `COPY`

# [`ADD`](https://docs.docker.com/engine/reference/builder/#add)
# [`COPY`](https://docs.docker.com/engine/reference/builder/#copy)

# Although `ADD` and `COPY` are functionally similar, generally speaking, `COPY`
# is preferred. That’s because it’s more transparent than `ADD`. `COPY` only
# supports the basic copying of local files into the container, while ADD has
# some features (like local-only tar extraction and remote URL support) that are
# not immediately obvious. Consequently, the best use for ADD is local tar file
# auto-extraction into the image, as in `ADD rootfs.tar.xz /`.

# ## `COPY`

COPY ./cardano-node ./run/
#COPY ../../../../../dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-node-1.33.0/x/cardano-node/build/cardano-node/cardano-node /run
#COPY config.json ./run
#COPY db-testnet ./run
#COPY topology.json ./run
#COPY "node-vrf${NODE_ID}.skey" ./run/node.skey
#COPY "node-kes${NODE_ID}.skey" ./run/node.skey
#COPY "node${NODE_ID}.opcert" ./run/node.opcert

# ## [`ENTRYPOINT`](https://docs.docker.com/engine/reference/builder/#entrypoint)

ENTRYPOINT ["./run/cardano-node", "run"]

# ## [`CMD`](https://docs.docker.com/engine/reference/builder/#cmd)

#CMD ["+RTS -sghc-rts-report.txt -RTS", "run", "--config /run/config.json", "--database-path /run/current/node-5/db-testnet", "--topology /run/topology.json", "--host-addr 127.0.0.1", "--socket-path /run/node.socket", "--tracer-socket-path-connect /run/tracer.socket", "--shelley-vrf-key /run/node.skey", "--shelley-kes-key /run/node.skey", "--shelley-operational-certificate /run/node.opcert", "--shutdown-on-slot-synced 300", "+RTS -N2 -I0 -A16m -qg -qb --disable-delayed-os-memory-return -RTS"]

# TODO
# ## [`HEALTHCHECK`](https://docs.docker.com/engine/reference/builder/#healthcheck)
