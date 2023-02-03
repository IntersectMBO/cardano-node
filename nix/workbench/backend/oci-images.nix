{ pkgs
, lib
, containerPkgs
}:

let

  # Why `nix2container` instead of the built-in `dockerTools` ?:
  # - https://lewo.abesis.fr/posts/nix-build-container-image/
  # - https://discourse.nixos.org/t/nix2container-another-dockertools-buildimage-implementation-based-on-skopeo/21688
  n2c = pkgs.nix2container.outputs.packages.x86_64-linux.nix2container;

  clusterNode = n2c.buildImage {
    name = "registry.ci.iog.io/workbench-cluster-node";
    # Adds `/etc/protocols` and ``/etc/services` to the root directory.
    # FIXME: Inside the container still can't resolve `localhost` but can
    # resolve WAN domains using public DNS servers.
    # Running `bash-5.1# /nix/store/*-glibc-#-bin/bin/getent hosts localhost`
    # inside the container returns nothing and python stuff like `supervisord`
    # breaks: "error: <class 'socket.gaierror'>, [Errno -2] Name or service not known: file: /nix/store/hb1lzaisgx2m9n29hqhh6yp6hasplq1v-python3-3.9.10/lib/python3.9/socket.py line: 954"
    # Further reading for hints:
    # https://stackoverflow.com/questions/39965432/docker-container-unable-to-resolve-localhost
    copyToRoot = with pkgs; [ iana-etc ];
    maxLayers = 25;
    # Transform the input binaries sections into layers.
    # These layers are added to the container's /nix/store, nothing in `$PATH`.
    layers = (lib.attrsets.mapAttrsToList
      (name: attr: n2c.buildLayer {deps = [ attr.nix-store-path ];})
      containerPkgs
    );
    # OCI container specification:
    # https://github.com/opencontainers/image-spec/blob/3a7f492d3f1bcada656a7d8c08f3f9bbd05e7406/specs-go/v1/config.go#L24
    config = {
      # Volumes are mounted as user `0:0`, I have no choice here.
      User = "0:0";
      # The stanza `WorkingDir` is not used because the config file of
      # `supervisord` depends on the working directory.
      Entrypoint = []; # `nix2container` includes these derivations inside the image
    };
  };

in

  {
    imageName = clusterNode.imageName;
    imageTag = clusterNode.imageTag;
    # https://github.com/containers/skopeo/blob/main/docs/skopeo-copy.1.md
    copyToPodman = "${clusterNode.copyToPodman}/bin/copy-to-podman";
  }
