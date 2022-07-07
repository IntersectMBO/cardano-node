to_jsonlist() {
    for x in "$@"
    do echo "\"$x\""
    done | jq --slurp '.'
}

jq_tolist() {
    local exp=$1; shift
    jq "$exp | join (\" \")" --raw-output "$@"
}

jq_fmutate() {
    local f=$1; shift
    test -f "$f" || { echo '{}' > "$f"; }
    jq "$@" "$f" | sponge "$f"
}

jq_check_json() {
    jq '.' "$1" >/dev/null
}

__usage() {
    local op=$1 desc=$2
    cat >&2 <<EOF
USAGE:  $(basename "$0") OPTIONS.. $op SUBOP SUBOP-ARGS..

  $desc:

$(cat)

EOF
}

usage() {
    __usage "$@"
    exit 1
}

declare -A colormap
colormap=(
    [black]=30
    [red]=31
    [green]=32
    [yellow]=33
    [blue]=34
    [magenta]=35
    [cyan]=36
    [white]=37
    [reset]=0
)

color() {
    echo -ne "\033[${colormap[$1]}m"
}

with_color() {
    local color=$1; shift
    color $color
    echo -ne "$*"
    color reset
}

colorise_colors=(
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
    red green yellow blue cyan white reset red green yellow blue cyan white reset
)
colorise() {
    for ((i=0; $#!=0; i++))
    do echo -n "$(with_color ${colorise_colors[$i]} $1) "
       shift
    done
}

msg() {
    echo "workbench:  $*" >&2
}

msg_ne() {
    echo -ne "workbench:  $*" >&2
}

green() {
    with_color green $*
}

blue() {
    with_color blue $*
}

white() {
    with_color white $*
}

yellow() {
    with_color yellow $*
}

red() {
    with_color red $*
}

progress() {
    local subsys=$1; shift
    msg "$(with_color green $subsys):  $(with_color blue $*)"
}

progress_ne() {
    local subsys=$1; shift
    msg_ne "$(with_color green $subsys):  $(with_color blue $*)"
}

warn() {
    local subsys=$1; shift
    msg "$(with_color green $subsys):  $(with_color yellow $*)"
}

fail() {
    msg "$(with_color red $*)"
    exit 1
}

fatal() {
    fail "FATAL: $*"
}

jqtest() {
    jq --exit-status "$@" > /dev/null
}

git_repo_commit_description() {
    local repo=$1
    local commit=$(git -C "$repo" rev-parse 'HEAD' 2>/dev/null || true)

    test -n "$commit" && {
        git -C "$repo" describe --match '[0-9].*' --tags $commit 2>/dev/null |
            cut -d'-' -f1,2 | tr -d '\n'
        git -C "$repo" diff --exit-code --quiet || echo '-modified'
    } || echo "unknown-not-a-git-repo"
}
