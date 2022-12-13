#!/bin/bash
# I don't understand why this just vanishes.
export PATH=/usr/bin:$PATH

echo ======== env =======
env | grep CI_
echo ========
echo $PATH
echo ========
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git switch $SECP256K1_REF --detach
./autogen.sh
./configure $CI_SECP_FLAGS --enable-module-schnorrsig --enable-experimental
make
make check
$CI_SECP_INSTALL_CMD make install
cd ..
