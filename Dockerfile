FROM debian:stable-slim as build
RUN apt-get update -y \
    && apt-get install -y automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf libsqlite3-dev m4 ca-certificates gcc libc6-dev curl python3 htop nload \
    && apt-get clean
RUN mkdir secp256k1-sources && cd secp256k1-sources && git clone https://github.com/bitcoin-core/secp256k1.git && cd secp256k1 && git reset --hard ac83be33d0956faf6b7f61a60ab524ef7d6a473a && ./autogen.sh && ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && make && make check && make install
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7 BOOTSTRAP_HASKELL_CABAL_VERSION=3.6.2.0 BOOTSTRAP_HASKELL_INSTALL_STACK=1 BOOTSTRAP_HASKELL_INSTALL_HLS=1 BOOTSTRAP_HASKELL_ADJUST_BASHRC=P sh

ENV PATH="/root/.cabal/bin:/root/.ghcup/bin:/root/.local/bin:$PATH"
RUN apt-get -y install libsodium23 libsodium-dev
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" \
    PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
RUN echo "Building tags/$VERSION..." 
RUN touch cabal.project.local
RUN ls
RUN  cabal configure --with-compiler=ghc-8.10.7 \
    && echo "package cardano-crypto-praos" >>  cabal.project.local \
    && echo "  flags: -external-libsodium-vrf" >>  cabal.project.local \
    && cabal build all \
    && mkdir -p /root/.local/bin/ \
    && cp -p dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-node-*/x/cardano-node/build/cardano-node/cardano-node /root/.local/bin/ \
    && cp -p dist-newstyle/build/x86_64-linux/ghc-8.10.7/cardano-cli-*/x/cardano-cli/build/cardano-cli/cardano-cli /root/.local/bin/

FROM debian:stable-slim
COPY --from=build /root/.local/bin/ /bin/
COPY --from=build /usr/local/lib/ /lib/
RUN apt-get update && apt-get install git  -y
RUN apt-get -y install libsodium23 libsodium-dev
RUN apt-get install -y automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf libsqlite3-dev m4 ca-certificates gcc libc6-dev
RUN mkdir secp256k1-sources && cd secp256k1-sources && git clone https://github.com/bitcoin-core/secp256k1.git && cd secp256k1 && git reset --hard ac83be33d0956faf6b7f61a60ab524ef7d6a473a && ./autogen.sh && ./configure --prefix=/usr --enable-module-schnorrsig --enable-experimental && make && make check && make install
RUN groupadd -g 1001 cardano
RUN useradd -rm -d /home/cardano -s /bin/bash -g 1001 -G sudo -u 1001 cardano
RUN mkdir -p /home/cardano/data/db
RUN mkdir /home/cardano/ipc
RUN cd /home/cardano && git clone https://github.com/input-output-hk/cardano-configurations.git
RUN chown -R 1001:1001 /home/cardano/ipc 
RUN chown -R 1001:1001 /home/cardano/data
RUN chown -R 1001:1001 /home/cardano/cardano-configurations
USER 1001:1001
WORKDIR /home/cardano
ENTRYPOINT cardano-node run --database-path /home/cardano/data/db --host-addr 0.0.0.0 --port "$PORT" --socket-path /home/cardano/ipc/node.socket --topology /home/cardano/cardano-configurations/network/"$NETWORK"/cardano-node/topology.json --config /home/cardano/cardano-configurations/network/"$NETWORK"/cardano-node/config.json
