# shellcheck shell=bash

# This is a small wrapper around nix-instantiate, which is exposed as `wb evaluate`.

usage_evaluate() {
	cat <<-EOF
		$(red USAGE:)

		   $(helpcmd evaluate \[FLAGS..] \[ATTRS...])

		   $(blue FLAGS):
		      $(helpopt --profile-json PROFILE-JSON)     Import the specified profile
		            defaults to $(yellow \$WB_SHELL_PROFILE_DATA/profile.json) if set

		      $(helpopt --node-specs NODE-SPECS)         Import the specified environment specification
		            defaults to $(yellow \$WB_SHELL_PROFILE_DATA/node-specs.json) if set

		      $(helpopt --help)                          This help message

		   $(blue ATTRS):
		          List of attributes to evaluate, defaults to the whole configuration.

		   Example:

		   wb evaluate evaluate genesis.create-staked-args
	EOF
}

evaluate() {
	local profile_json node_specs
	local attrs=()
	local defaultAttrs=(--attr config)

	while [ "$#" -gt 0 ]; do
		case "$1" in
		--)
			extra_args+=("$@")
			shift 1
			break
			;;
		--help)
			usage_evaluate
			exit 1
			;;
		--profile)
			profile_json=${2:?No value for argument $1}
			shift 2
			;;
		--node-specs)
			node_specs=${2:?No value for argument $1}
			shift 2
			;;
		*)
			attrs+=(--attr "config.$1")
			shift 1
			;;
		esac
	done

  local err_missing_profile="You need to provide a profile: WB_SHELL_PROFILE_DATA is not set and at least one of --profile-json or --node-specs is missing."
  profile_json=${profile_json:-${WB_SHELL_PROFILE_DATA:?$err_missing_profile}/profile.json}
  node_specs=${node_specs:-${WB_SHELL_PROFILE_DATA:?$err_missing_profile}/node-specs.json}

	nix-instantiate --eval --strict --json --no-warn-dirty --show-trace \
		--argstr profile-json "${profile_json}" \
		--argstr node-specs "${node_specs}" \
		"$WB_CARDANO_NODE_REPO_ROOT/nix/workbench/wb.nix" \
		"${attrs[@]:-${defaultAttrs[@]}}" \
		"${extra_args[@]}"
}
