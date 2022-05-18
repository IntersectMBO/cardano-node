cluster-shell: CMD = start-cluster               ## Enter Nix shell and start the workbench cluster
cluster-shell:
	nix-shell --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE}
