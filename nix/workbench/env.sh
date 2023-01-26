WB_ENV_DEFAULT='
{ "type":         "supervisor"
, "cacheDir":     "'${XDG_CACHE_HOME:-$HOME/.cache}'/cardano-workbench"
, "basePort":     30000
, "staggerPorts": true
}'

export WB_ENV=$WB_ENV_DEFAULT

envjq() {
    jq ".$1" <<<$WB_ENV
}

envjqr() {
    jq -r ".$1" <<<$WB_ENV
}

setenvjq() {
    export WB_ENV=$(jq ". * { $1: $2 }" <<<$WB_ENV)
}

setenvjqstr() {
    setenvjq "$1" "\"$2\""
}
