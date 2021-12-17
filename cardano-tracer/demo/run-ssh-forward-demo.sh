#!/bin/sh

if [ $# -eq 0 ]; then
  echo "Usage: ./run-ssh-forward-demo.sh PASS FMODE, where PASS is your ssh password and FMODE is forwarder's mode, Responder or Initiator."
  exit 1
fi

cabal build cardano-tracer && cabal install cardano-tracer --installdir=./ --overwrite-policy=always

readonly SSH_PASS=$1
readonly FORWARDER_MODE=$2

readonly TMP_DIR=/run/user/1000
readonly TRACER_SOCK=${TMP_DIR}/cardano-tracer-demo.sock
readonly TRACER_LOG_ROOT=${TMP_DIR}/cardano-tracer-demo-logs
readonly FORWARDER_SOCK=${TMP_DIR}/demo-forwarder.sock

rm -rf ${TRACER_LOG_ROOT} ${TRACER_SOCK} ${FORWARDER_SOCK}
mkdir ${TRACER_LOG_ROOT}

# Please note that TRACER_SOCK and FORWARDER_SOCK are different,
# so direct connection between parts is impossible. It can be established
# only via SSH local forwarding.

echo "Enable SSH local forwarding..."
if [ "$FORWARDER_MODE" == "Responder" ]
then
  SSHPASS_PID=$(nohup sshpass -p ${SSH_PASS} ssh -nNT -L ${TRACER_SOCK}:${FORWARDER_SOCK} -o "ExitOnForwardFailure yes" -- localhost &>/dev/null & echo $!)
else
  SSHPASS_PID=$(nohup sshpass -p ${SSH_PASS} ssh -nNT -L ${FORWARDER_SOCK}:${TRACER_SOCK} -o "ExitOnForwardFailure yes" -- localhost &>/dev/null & echo $!)
fi
sleep 1

echo "Run demo-forwarder..."
FORWARDER_PID=$(nohup ./demo-forwarder ${FORWARDER_SOCK} ${FORWARDER_MODE} &>/dev/null & echo $!)

echo "Run cardano-tracer..."
if [ "$FORWARDER_MODE" == "Responder" ]
then
  TRACER_CONFIG=active-tracer-config.json
else
  TRACER_CONFIG=passive-tracer-config.json
fi
TRACER_PID=$(nohup ./cardano-tracer --config ${TRACER_CONFIG} &>/dev/null & echo $!)

echo "Wait..."
sleep 10

echo "Stop both sides and SSH forwarding..."
kill ${FORWARDER_PID}
kill ${TRACER_PID}
kill ${SSHPASS_PID}
SSH_PID=$(ps aux | grep 'ssh' | grep 'demo-forwarder.sock' | awk '{print $2}')
kill ${SSH_PID}
rm -rf cardano-tracer
rm -rf demo-forwarder

# If the log root dir isn't empty, it means that there was forwarding of TraceObjects
# from demo-forwarder to cardano-tracer. It proves that SSH local forwarder works.
echo "Check the content of log root dir..."
ls ${TRACER_LOG_ROOT}
