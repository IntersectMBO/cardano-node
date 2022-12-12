{ pkgs
# Cardano packages/executables.
, cardano-node, cardano-tracer, tx-generator
# OCI Image builder.
}:

let

  # Why `nix2container` instead of the built-in `dockerTools` ?:
  # - https://lewo.abesis.fr/posts/nix-build-container-image/
  # - https://discourse.nixos.org/t/nix2container-another-dockertools-buildimage-implementation-based-on-skopeo/21688
  n2c = pkgs.nix2container.outputs.packages.x86_64-linux.nix2container;

  clusterImage = n2c.buildImage {
    name = "registry.ci.iog.io/workbench-cluster-aio-image";
    # Adds `/etc/protocols` and ``/etc/services` to the root directory.
    # FIXME: Inside the container still can't resolve `localhost` but can
    # resolve WAN domains using public DNS servers.
    # Running `bash-5.1# /nix/store/*-glibc-#-bin/bin/getent hosts localhost`
    # inside the container returns nothing and python stuff like `supervisord`
    # breaks: "error: <class 'socket.gaierror'>, [Errno -2] Name or service not known: file: /nix/store/hb1lzaisgx2m9n29hqhh6yp6hasplq1v-python3-3.9.10/lib/python3.9/socket.py line: 954"
    # Further reading for hints:
    # https://stackoverflow.com/questions/39965432/docker-container-unable-to-resolve-localhost
    copyToRoot = with pkgs; [ iana-etc ];
    # All these layers are added to /nix/store, nothing is in `$PATH`.
    maxLayers = 25;
    layers = with pkgs; [
      # Runtime to be able run bash commands from `podman`/`nomad`.
      (n2c.buildLayer {deps = [ bashInteractive coreutils  ];})
      # Supervisor.
      (n2c.buildLayer {deps = [ python3Packages.supervisor ];})
      # Cardano packages.
      (n2c.buildLayer {deps = [ cardano-node               ];})
      (n2c.buildLayer {deps = [ cardano-tracer             ];})
      (n2c.buildLayer {deps = [ tx-generator               ];})
    ];
    # OCI container specification:
    # https://github.com/opencontainers/image-spec/blob/3a7f492d3f1bcada656a7d8c08f3f9bbd05e7406/specs-go/v1/config.go#L24
    config = {
      # Volumes are mounted as user `0:0`, I have no choice here.
      User = "0:0";
      # The stanza `WorkingDir` is not used because the config file of
      # `supervisord` depends on the working directory.
      Entrypoint =
        let
          entrypoint = pkgs.writeShellApplication {
            name = "entrypoint";
            runtimeInputs = with pkgs; [
              coreutils
              bashInteractive
              python3Packages.supervisor
            ];
            text = ''
              # The SUPERVISOR_NIX variable must be set
              [ -z "''${SUPERVISOR_NIX:-}" ] && echo "SUPERVISOR_NIX env var must be set -- aborting" && exit 1

              # The SUPERVISORD_CONFIG variable must be set
              [ -z "''${SUPERVISORD_CONFIG:-}" ] && echo "SUPERVISORD_CONFIG env var must be set -- aborting" && exit 1

              # Create a link to the `supervisor` Nix folder.
              # First check if already exists to be able to restart containers.
              if ! test -e "$SUPERVISOR_NIX"
              then
                "${pkgs.coreutils}"/bin/ln -s "${pkgs.python3Packages.supervisor}" "$SUPERVISOR_NIX"
              fi

              # The SUPERVISORD_LOGLEVEL defaults to "info"
              # The logging level at which supervisor should write to the
              # activity log. Valid levels are trace, debug, info, warn, error
              # and critical.
              LOGLEVEL="''${SUPERVISORD_LOGLEVEL:-info}"

              # Start `supervisord` on the foreground.
              "${pkgs.python3Packages.supervisor}"/bin/supervisord --nodaemon --configuration "$SUPERVISORD_CONFIG" --loglevel="$LOGLEVEL"
            '';
          };
        in
          [ "${entrypoint}/bin/entrypoint" ];
    };
  };

in {

  inherit clusterImage;

}
