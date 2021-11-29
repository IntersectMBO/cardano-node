#!/bin/sh

if [ $# -eq 0 ]; then
  echo "Usage: ./run.sh FMODE, where FMODE is forwarders' mode, Responder or Initiator."
  exit 1
fi

rm -rf cardano-tracer demo-forwarder

cabal build cardano-tracer && cabal install cardano-tracer --installdir=./ --overwrite-policy=always

readonly FORWARDER_MODE=$1
readonly DELAY_IN_SECS=$2

readonly TMP_DIR=/run/user/1000
readonly TRACER_LOG_ROOT=${TMP_DIR}/cardano-tracer-demo-logs

readonly SOCK_1=${TMP_DIR}/cardano-tracer-demo-1.sock
readonly SOCK_2=${TMP_DIR}/cardano-tracer-demo-2.sock
readonly SOCK_3=${TMP_DIR}/cardano-tracer-demo-3.sock

rm -rf ${TRACER_LOG_ROOT} ${SOCK_1} ${SOCK_2} ${SOCK_3}
mkdir ${TRACER_LOG_ROOT}

echo "Run 3 demo-forwarders..."
if [ "$FORWARDER_MODE" == "Responder" ]
then
  # Run demo-forwarders in Responder mode, so they will accept connections from the tracer via 3 sockets.
  FORWARDER_1_PID=$(nohup ./demo-forwarder ${SOCK_1} ${FORWARDER_MODE} &>/dev/null & echo $!)
  FORWARDER_2_PID=$(nohup ./demo-forwarder ${SOCK_2} ${FORWARDER_MODE} &>/dev/null & echo $!)
  FORWARDER_3_PID=$(nohup ./demo-forwarder ${SOCK_3} ${FORWARDER_MODE} &>/dev/null & echo $!)
else
  # Run demo-forwarders in Initiator mode, so they will connect to the tracer via the same socket.
  FORWARDER_1_PID=$(nohup ./demo-forwarder ${SOCK_1} ${FORWARDER_MODE} &>/dev/null & echo $!)
  FORWARDER_2_PID=$(nohup ./demo-forwarder ${SOCK_1} ${FORWARDER_MODE} &>/dev/null & echo $!)
  FORWARDER_3_PID=$(nohup ./demo-forwarder ${SOCK_1} ${FORWARDER_MODE} &>/dev/null & echo $!)
fi

echo "Run cardano-tracer..."
if [ "$FORWARDER_MODE" == "Responder" ]
then
  TRACER_CONFIG=active-tracer-config.json
else
  TRACER_CONFIG=passive-tracer-config.json
fi
./cardano-tracer --config ${TRACER_CONFIG} +RTS -M8m -RTS
