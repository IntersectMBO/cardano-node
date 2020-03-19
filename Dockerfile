# docker build --build-arg NETWORK=mainnet to build with default network config, otherwise all config must be mounted at runtime
ARG NETWORK=runtimeBase

### Base builder with development dependencies
FROM haskell:8.6.5
RUN apt-get update && apt-get install -y \
  build-essential \
  libtinfo-dev \
  libssl-dev \
  libsystemd-dev \
  zlib1g-dev
ADD . /src
WORKDIR /src
RUN stack install

FROM debian:stretch AS runtimeBase
# Shared libs
COPY --from=builder /root/.local/bin /root/.local/bin
COPY --from=builder /usr/lib /usr/lib
COPY --from=builder /etc /etc
COPY ./scripts/run.sh ./run.sh
RUN chmod +x /run.sh
ENV PATH="/root/.local/bin:${PATH}"

### Default config for supported networks
FROM runtimeBase AS mainnet
COPY ./configuration/log-configuration.yaml /config/config.yaml
COPY ./configuration/mainnet-genesis.json /config/genesis.json
COPY ./configuration/mainnet-topology.json /config/topology.json

FROM ${NETWORK} AS cardano-node
ENTRYPOINT ["./run.sh"]