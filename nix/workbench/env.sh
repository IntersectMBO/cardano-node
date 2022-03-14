WORKBENCH_ENV_DEFAULT='
{ "type":         "supervisor"
, "cacheDir":     "'$HOME'/.cache/cardano-workbench"
, "basePort":     30000
, "staggerPorts": true
}'

export WORKBENCH_ENV=$WORKBENCH_ENV_DEFAULT

envjq() {
    jq ".$1" <<<$WORKBENCH_ENV
}

envjqr() {
    jq -r ".$1" <<<$WORKBENCH_ENV
}

setenvjq() {
    export WORKBENCH_ENV=$(jq ". * { $1: $2 }" <<<$WORKBENCH_ENV)
}

setenvjqstr() {
    setenvjq "$1" "\"$2\""
}
