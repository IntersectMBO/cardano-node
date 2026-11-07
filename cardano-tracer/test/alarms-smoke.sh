#!/usr/bin/env bash
# Level-2 smoke test for the alarm HTTP surface. Level 1 (property tests
# in test/Cardano/Tracer/Test/Alarms/) is run separately with
# `cabal test cardano-tracer`.
set -euo pipefail

work=$(mktemp -d)
prod=secret-producer
rdr=secret-reader
url=http://127.0.0.1:3210/alarms/v1/events

printf '%s' "$prod" > "$work/p.token"
printf '%s' "$rdr"  > "$work/r.token"

cat > "$work/tracer.yaml" <<EOF
networkMagic: 42
network: {tag: AcceptAt, contents: "$work/forwarder.sock"}
logging: [{logRoot: "$work/logs", logMode: FileMode, logFormat: ForMachine}]
alarms:
  alEndpoint: {epHost: "127.0.0.1", epPort: 3210, epForceSSL: false}
  alAllowInsecure: true
  alAuthentication:
    aacProducers: [{pcName: p, pcTokenFile: "$work/p.token", pcSource: hermod-recon}]
    aacReaders:   [{rcName: r, rcTokenFile: "$work/r.token", rcAllowHistory: true}]
  alConsumers: [{type: log, name: audit-log, enabled: true}]
  alTraceRules: [{ruleId: error-traces, threshold: error, suppressForSecs: 60}]
EOF

"${CARDANO_TRACER:-cardano-tracer}" --config "$work/tracer.yaml" &>"$work/tracer.log" &
pid=$!
trap 'kill "$pid" 2>/dev/null; rm -rf "$work"' EXIT

for _ in {1..60}; do
  curl -sSo /dev/null -m1 "$url" 2>/dev/null && break
  sleep 0.25
done

body='{"sourceEventId":"s1","raisedAt":"2026-08-11T12:00:00Z","ruleId":"cg","severity":"warning","summary":"hi"}'
ap="Authorization: Bearer $prod"
ar="Authorization: Bearer $rdr"
ct="Content-Type: application/json"

check() {  # check <expected-code> <curl-args...>
  local got exp=$1; shift
  got=$(curl -sSo "$work/out" -w '%{http_code}' "$@")
  [[ $got == "$exp" ]] || { echo "expected $exp, got $got: $(cat "$work/out")"; exit 1; }
}

check 201 -H "$ap" -H "$ct" -d "$body" "$url"        ; grep -q '"created":true'  "$work/out"
check 200 -H "$ap" -H "$ct" -d "$body" "$url"        ; grep -q '"created":false' "$work/out"
check 200 -H "$ar" "$url"                            ; grep -q '"eventId"'       "$work/out"
check 401 "$url"
check 401 -H "Authorization: Bearer wrong" "$url"
check 401 -H "$ar" -H "$ct" -d "$body" "$url"
check 400 -H "$ap" -H "$ct" -d '{no' "$url"

echo OK
