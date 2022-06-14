/* Build a Dockerfile with `cardano-node` executable for the workbench

pkgs.dockerTools is a set of functions for creating and manipulating Docker
images according to the Docker Image Specification v1.2.0. Docker itself is not
used to perform any of the operations done by these functions.

External resources:
- [Building and running Docker images](https://nix.dev/tutorials/building-and-running-docker-images)
- [pkgs.dockerTools](https://nixos.org/manual/nixpkgs/stable/#sec-pkgs-dockerTools)
- [Dockerfile reference](https://docs.docker.com/engine/reference/builder/)
- [YouTube: Nix + Docker, a match made in heaven](https://www.youtube.com/watch?v=WP_oAmV6C2U)
*/
{
  # TODO: Change pkgs to just dockerTools:
  # dockerTools ? (import <nixpkgs> {}).dockerTools
    pkgs #? import <nixpkgs> {}
  , cardanoNode #? ((import /home/fmaste/Workspace/GitHub/input-output-hk/cardano-node) {}).outputs.packages.x86_64-linux.cardano-node
}:

# `buildImage` function is analogous to the `docker build` command, in that it
# can be used to build a Docker-compatible repository tarball containing a
# single image with one or multiple layers. As such, the result is suitable for
# being loaded in Docker with `docker load`.
# `buildLayeredImage` creates a Docker image with many of the store paths being
# on their own layer to improve sharing between images. The image is realized
# into the Nix store as a gzipped tarball. Depending on the intended usage,
# many users might prefer to use `streamLayeredImage` instead, which this
# function uses internally.
# `streamLayeredImage` builds a script which, when run, will stream an
# uncompressed tarball of a Docker image to stdout. The arguments to this
# function are as for `buildLayeredImage`. This method of constructing an image
# does not realize the image into the Nix store, so it saves on IO and
# disk/cache space, particularly with large images.
pkgs.dockerTools.buildImage {

  # `name` specifies the name of the resulting image. This is the only required
  # argument for `buildImage` or `buildLayeredImage`.
  name = "cardano-node-workbench";

  # `tag` specifies the tag of the resulting image. By default it’s `null`,
  # which indicates that the nix output hash will be used as tag (the output
  # path’s hash).
  tag = "latest";

  # `fromImage` is the repository tarball containing the base image. It must be
  # a valid Docker image, such as exported by `docker save`. By default it’s
  # `null`, which can be seen as equivalent to `FROM scratch` of a `Dockerfile`.
  # `fromImageName` can be used to further specify the base image within the
  # repository, in case it contains multiple images. By default it’s `null`, in
  # which case `buildImage` will peek the first image available in the
  # repository.
  # `fromImageTag` can be used to further specify the tag of the base image
  # within the repository, in case an image contains multiple tags. By default
  # it’s `null`, in which case `buildImage` will peek the first tag available
  # for the base image.
  fromImageName = "alpine";
  fromImageTag = "latest";

  # `contents` is either a single derivation or a list of derivations that will
  # be copied in the new layer of the resulting image. This can be similarly
  # seen as `ADD contents/ /` in a `Dockerfile`. By default it’s `null`.
  # NOTE: If you see errors similar to "getProtocolByName: does not exist (no
  # such protocol name: tcp)" you may need to add `pkgs.iana-etc` to `contents`.
  # NOTE: If you see errors similar to "Error_Protocol (\"certificate has
  # unknown CA\",True,UnknownCa)" you may need to add `pkgs.cacert` to
  # `contents`.
  contents = [ cardanoNode ];

  # `config` is used to specify the configuration of the containers that will be
  # started off the built image in Docker. The available options are listed in
  # the [Docker Image Specification v1.2.0](https://github.com/moby/moby/blob/master/image/spec/v1.2.md#image-json-field-descriptions).
  config = {
    WorkingDir  = "/opt/workbench";
    Env = [];
    Cmd = ["${cardanoNode}/bin/cardano-node" "run"];
    Volumes = {
      #"/data" = {};
    };
  };

  # Others not used:
  # runAsRoot:
  # created:
  # maxLayers:
  # extraCommands:
  # fakeRootCommands:
  # enableFakechroot:

}
