FROM ubuntu:20.04 AS builder

ENV BOOTSTRAP_HASKELL_CABAL_VERSION=3.4.0.0
ENV BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1

ENV PATH="${PATH}:/root/.local/bin:/root/.ghcup/bin"
ENV LD_LIBRARY_PATH="/usr/local/lib:${LD_LIBRARY_PATH}"

ENV TZ=Europe/Warsaw
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt-get update
RUN apt-get install -y \
    curl \
    automake \
    build-essential \
    pkg-config \
    libffi-dev \
    libgmp-dev \
    libssl-dev \
    libtinfo-dev \
    libsystemd-dev \
    zlib1g-dev \
    make \
    g++ \
    tmux \
    git \
    jq \
    wget \
    libncursesw5 \
    libtool \
    autoconf

RUN echo ${BOOTSTRAP_HASKELL_CABAL_VERSION}

RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
RUN bash -c "curl -sSL https://get.haskellstack.org/ | sh"

RUN mkdir -p /src
WORKDIR /src
COPY ./ ./

RUN cd libsodium && \
    ./autogen.sh && \
    ./configure && \
    make && \
    make install

RUN echo "package cardano-crypto-praos" >>  cabal.project.local
RUN echo "  flags: -external-libsodium-vrf" >>  cabal.project.local

# takes really, really long
RUN cabal build all

RUN mkdir /output
RUN find -name cardano-cli -type f -exec mv {} /output/ \;
RUN find -name cardano-node -type f -exec mv {} /output/ \;

########################################################################
FROM ubuntu:20.04

RUN mkdir -p /root/.local/bin/ /usr/local/lib
COPY --from=builder /output/cardano-cli /root/.local/bin/
COPY --from=builder /output/cardano-node /root/.local/bin/
COPY --from=builder /usr/local/lib/libsodium* /usr/local/lib/


ENV PATH="${PATH}:/root/.local/bin"
ENV LD_LIBRARY_PATH="/usr/local/lib:${LD_LIBRARY_PATH}"

ENTRYPOINT ["bash", "-c"]
