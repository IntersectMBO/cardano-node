{ pkgs
, runJq
, backend
, profile
, nodeSpecs
}:

with pkgs.lib;

let
  ##
  ## generator-service :: (TracerConfig, NixosServiceConfig, Config, StartScript)
  ##
  healthcheck-service =
    (nodeSpecs:
    let
      bashInteractive = pkgs.bashInteractive;
      coreutils       = pkgs.coreutils;
      supervisor      = pkgs.supervisor;
    in {
      startupScript = rec {
        JSON = pkgs.writeScript "startup-healthcheck.sh" value;
        value = ''
          #!${bashInteractive}/bin/sh

          # Store the entrypoint env vars for debugging purposes
          ${coreutils}/bin/env > /local/healthcheck.env

          # Only needed for "exec" ?
          if test "''${TASK_DRIVER}" = "exec"
          then
            cd "''${TASK_WORKDIR}"
          fi

          sleep 100000
          '';
      };
    })
    nodeSpecs.value;
in
  { inherit healthcheck-service; }
