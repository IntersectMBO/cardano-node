# The Nix side of the workbench bundle: consumer-owned code, NOT part of the
# bundle. Two things, and they share every parameter, which is why they are one
# file:
#
#   runner   what READS the bundle vendored next to this file (workbench.json,
#            profiles/): the runtime `wb`, one materialised profile, and the
#            `workbench-envars` fragment that ties them together
#   shell    the dev shell built on it
#
# ./hydra.nix takes `runner` from here for the CI smoke test and never touches
# `shell`; both members are lazy, so asking for one does not build the other.
#
# Everything with a default has one because it is a WORKBENCH default, not a
# user's: a caller passes only what it varies. That is what makes a wrapper
# around this file (a `workbench-runner` on `pkgs`, say) redundant, and it keeps
# a Hydra job from reading anybody's `customConfig`.
{ pkgs
  # The haskell project: whose executables to run, and what the dev shell is for.
  # The one thing with no sensible default.
, project
, lib              ? pkgs.lib
  ## The dev shell's own, all unused by `runner` and so irrelevant to ./hydra.nix.
, haskellLib       ? null
, setLocale        ? ""
, haveGlibcLocales ? false
, workbenchDevMode ? false
, withHoogle       ? true
  ## The run this is for.
, profileName        ? "default"
, eraName            ? "conway"
, backendName        ? "supervisor"
, stateDir           ? "run/current"
, batchName          ? "undefined"
  ## `runner` does not see this one: the exported data carries absolute paths
  ## either way and wb-run resolves them against whatever PATH it finds, which is
  ## what this flag decides, in `nativeBuildInputs` far below.
, useCabalRun        ? false
, workbenchStartArgs ? []
, profiling          ? {}
  # Which cardano-node version leaf of the profile to run. Not a bundle
  # default: the version comes from cardano-node itself (the checkout's
  # cabal version).
, cardanoNodeVersion ? null
}:

with lib;

let
  nodeVersion =
    if cardanoNodeVersion != null then cardanoNodeVersion
    else project.hsPkgs.cardano-node.identifier.version;

  ## The bundle is authoritative for the base port: ports run exactly as
  ## built (the consumer cannot change them today); the recorded value only
  ## parameterises the runtime (EKG/Prometheus shifts, `wb start`).
  wjMeta =
    if builtins.pathExists ./workbench.json
    then builtins.fromJSON (builtins.readFile ./workbench.json)
    else { };
  basePort = wjMeta.basePort or 30000;

  ## The bundle says what it needs BY NAME; this repo says where to get it.
  ## Resolving by name is not a formality: a path read out of workbench.json is
  ## a plain string with no store context, so carrying one puts nothing in a
  ## derivation closure and the sandbox ends up without it ("bad interpreter").
  ## And the recorded paths come from the nixpkgs the bundle was exported
  ## against, which is not this checkout's -- they would not be here to find
  ## even if they were referenced. Names are the portable part.
  ##
  ## The `pkgs` group's names ARE nixpkgs attribute paths, so they resolve with
  ## no table here to fall out of date. The `cardano` group's are this
  ## checkout's own answers, and the only thing a consumer has to keep in sync.
  ## Every name in the bundle's `cardano` group needs an answer here, including
  ## the ones a bundle RUN never reaches (`runNeeds = false` in the data): this
  ## list is what goes on PATH, and a missing entry throws at eval.
  cardanoTool = {
    cardano-node     = project.exes.cardano-node;
    cardano-cli      = project.hsPkgs.cardano-cli.components.exes.cardano-cli;
    cardano-tracer   = project.exes.cardano-tracer;
    tx-generator     = project.exes.tx-generator;
    locli            = project.exes.locli;
    db-analyser      = project.hsPkgs.ouroboros-consensus.components.exes.db-analyser;
    ## No cardano-profile / cardano-topology: they BUILD profile data, and a
    ## checkout that vendors the bundle has neither the packages nor a reason to
    ## run them, the data here was already materialised by the exporter.
  };

  ## A name the bundle used that this reader has no answer for is a hard stop:
  ## the data would keep a path from another machine, or a command would not be
  ## on PATH for wb-run to find.
  bundleTool = group: name:
    if group == "cardano"
    then cardanoTool.${name} or
           (throw ("workbench bundle needs cardano \"${name}\", which this reader"
                  + " cannot resolve: add it to `cardanoTool` in"
                  + " nix/workbench/shell.nix"))
    else lib.attrByPath
           (lib.splitString "." name)
           (throw ("workbench bundle needs nixpkgs \"${name}\", which this"
                  + " checkout's nixpkgs does not have"))
           pkgs
  ;

  ## Everything the bundle declares, resolved to this checkout's packages.
  ## `wb-run` then finds each COMMAND in the PATH built from these, which is
  ## the whole contract between the two: this side provides, that side resolves.
  ## Empty before the first export.
  workbenchTools =
    lib.concatLists
      (lib.mapAttrsToList
        (group: deps: map (bundleTool group) (builtins.attrNames deps))
        (wjMeta.runtime-deps or { })
      )
  ;

  ## `wb` plus everything the bundle says it needs, ahead of whatever the
  ## caller had.
  wbPath =
    lib.concatStringsSep ":"
      ([ "${wb}" ] ++ map (p: "${p}/bin") workbenchTools ++ [ "$PATH" ])
  ;

  ## The bundle says what shape it is. This reader depends on both halves of
  ## contract 10: profiles/<version>/<era>/<profile>, and `runtime-deps` keyed by
  ## a dependency name this side can resolve (to that dependency's whole record
  ## under profiles/, whose `dir` has no trailing "/"; in workbench.json, to
  ## whether a run needs it).
  ## Checked at eval, so a stale checkout fails at instantiation saying what to
  ## do, rather than three derivations down. A checkout with no bundle yet has
  ## nothing to check.
  contract = 10;
  bundleContract = x:
    if !(wjMeta ? contract) then x
    else if wjMeta.contract != contract
    then throw ("workbench bundle is contract ${toString wjMeta.contract}, this"
                + " reader speaks ${toString contract}: re-export it with"
                + " export/export.sh and copy the result into nix/workbench/")
    ## The contract number cannot catch a payload that merely GREW: a bundle
    ## exported before nix/workbench/start-cluster.sh existed is still shape 8,
    ## and `workbench-envars` below sources that file out of the deployed
    ## runtime. Without this the shell enters, says "No such file or directory",
    ## and leaves `start-cluster` undefined with nothing pointing at why.
    else if !(wjMeta.files ? "start-cluster.sh")
    then throw ("workbench bundle carries no start-cluster.sh (${toString"
                + " (builtins.length (builtins.attrNames wjMeta.files))} payload"
                + " files): it predates nix/workbench/start-cluster.sh, re-export"
                + " it with export/export.sh")
    else x
  ;

  # Both data derivations are built by ./wb-run, the bash reader, so bundle
  # reading is implemented exactly once; this file only parameterises it.

  # The runtime scripts, deployed out of workbench.json into the store:
  # entering the shell forces this, the checkout is never written. Pure text and
  # nothing else -- no shipped file names a store path, so `jq` is the whole
  # dependency. The binaries are earned where they are actually named, by
  # `materialised` below, whose rewritten paths ARE its own inputs.
  wb = bundleContract (pkgs.runCommand "workbench-runtime"
    { nativeBuildInputs = [ pkgs.jq ]; }
    ''
    WB_WORKBENCH_JSON=${./workbench.json} \
      bash ${./wb-run} --deploy "$out"
    '');

  # One profile unpacked into the two data dirs a run exports as
  # WB_SHELL_PROFILE_DATA ($out/profile) and WB_BACKEND_DATA ($out/backend).
  # Era, version, base port and binary resolution are all bound here, not
  # baked into the bundle.
  materialised = bundleContract (pkgs.runCommand "workbench-data-${profileName}-${eraName}"
    { nativeBuildInputs = [ pkgs.jq ] ++ workbenchTools; }
    ''
    export WB_WORKBENCH_JSON=${./workbench.json}
    export WB_PROFILE_CATALOGUE=${./profiles}
    export WB_CARDANO_NODE_VERSION=${nodeVersion}
    ## wb-run resolves every command the data declares out of THIS PATH, so the
    ## rewritten data names this derivation's own inputs. That is also what puts
    ## them in its closure: the paths it writes are references Nix scans for,
    ## not text.
    bash ${./wb-run} --materialise ${profileName} ${eraName} "$out"
    '');

  workbench-envars = ''
    export WB_BUNDLE=${wb}
    export PATH=${wbPath}
    export WB_SHELL_PROFILE_NAME=${profileName}
    export WB_SHELL_PROFILE_DATA=${materialised}/profile
    export WB_BACKEND_DATA=${materialised}/backend
    export WB_PROFILE_CATALOGUE=${./profiles}
    export WB_CARDANO_NODE_VERSION=${nodeVersion}
    export WB_SHELL_ERA_NAME=${eraName}
    export WB_BACKEND_NAME=${backendName}
    export WB_BASE_PORT=${toString basePort}
    export WB_NIX_PLAN=${project.plan-nix}/plan.json
    export WB_CHAP_PATH=${project.args.inputMap."https://chap.intersectmbo.org/"}
    export WB_BATCH_NAME=${batchName}
    export WB_START_ARGS=${lib.escapeShellArg (builtins.concatStringsSep " " workbenchStartArgs)}
    export CARDANO_NODE_SOCKET_PATH=${stateDir}/node-0/node.socket
    ${lib.optionalString (profiling.profiledBuild or false) "export WB_PROFILEDBUILD=yes"}
    ${lib.optionalString (profiling.infoTable or false) "export WB_PROFILINGINFOTABLE=yes"}
    # Shared runtime defaults (WB_DEPLOYMENT_NAME, WB_LOCLI_DB, WB_CACHE_DIR,
    # WB_GITREV, WB_MODULAR_GENESIS, WB_GENESIS_RIPPER) are set by
    # start-cluster.sh itself, once for every reader.
    . ${wb}/start-cluster.sh
  '';

  ## What ./hydra.nix needs off this file. `wb` is the HEAD of `wbPath` and of
  ## anything built from it on purpose: forcing it forces `bundleContract`
  ## first, so a stale bundle says "re-export it" instead of failing on some
  ## tool it could not resolve.
  runner = {
    inherit wb materialised workbench-envars;
    inherit basePort wbPath workbenchTools;
  };

in
{
  inherit runner;

  shell =
  project.shellFor {
    name = "workbench-shell";

    ## `profiling` and `useCabalRun` are this file's own parameters now, the same
    ## ones it handed the runner, so there is nothing to read back off it.
    shellHook =
      ''
      while test $# -gt 0
      do shift; done       ## Flush argv[]

      . ${runner.wb}/lib.sh

      ${runner.workbench-envars}

      progress "profile name"            $WB_SHELL_PROFILE_NAME
      progress "era name"                $WB_SHELL_ERA_NAME
      progress "backend name"            $WB_BACKEND_NAME
      progress "profiling"               'profiledBuild=${if profiling.profiledBuild or false then "yes" else "no"} profilingType=${profiling.profilingType or ""}'
      progress "params"                  'useCabalRun=${toString useCabalRun} workbenchDevMode=${toString workbenchDevMode}'
      progress "deployment name"         $WB_DEPLOYMENT_NAME
      progress "WB_SHELL_PROFILE_DATA="  $WB_SHELL_PROFILE_DATA
      progress "WB_BACKEND_DATA="        $WB_BACKEND_DATA
      progress "WB_LOCLI_DB="            $WB_LOCLI_DB

      function parse_git_branch() {
          git branch 2> /dev/null | sed -n -e 's/^\* \(.*\)/(\1)/p'
      }
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\[\033[01;36m\]\$(parse_git_branch)\[\033[0m\]\$ "
      ''
      + optionalString workbenchDevMode
      ''
      export WB_EXTRA_FLAGS=

      ## Point wb at a workbench checkout to hack the harness live; defaults to
      ## the runtime deployed out of the vendored bundle.
      function wb() {
        "''${WB_DEV_ROOT:-${runner.wb}}"/wb $WB_EXTRA_FLAGS "$@"
      }
      export -f wb
      ''
      + optionalString useCabalRun
      ''
      . ${runner.wb}/lib-cabal.sh
      cabal update
      ''
      +
      ''
      function workbench_atexit() {
          if test -n "$(wb backend is-running run/current)"
          then stop-cluster
          fi
      }
      trap workbench_atexit EXIT
      ${setLocale}
      echo "
        Commands:
          * nix flake lock --update-input <iohkNix|haskellNix> - update nix build input
          * cardano-cli - used for key generation and other operations tasks
          * wb - cluster workbench
          * start-cluster - start a local development cluster
          * stop-cluster - stop a local development cluster
          * restart-cluster - restart the last cluster run (in 'run/current')
                              (WARNING: logs & node DB will be wiped clean)
      "
      ''
    ;

    inherit withHoogle;

    # The workbench shell uses cabalWrapped, which removes the
    # `source-repository-package` stanzas from `cabal.project`. haskell.nix is
    # clever enough to not include `source-repository-package`s in the shell
    # package db (cabal will rebuild them), so select *project* packages instead
    # of the default *local* ones to keep them out entirely.
    packages = ps: builtins.attrValues (haskellLib.selectProjectPackages ps);

    tools = {
    };

    # These programs will be available inside the nix-shell.
    nativeBuildInputs =
       (with pkgs; [
         cairo
         dyff
         git
         graphviz
         hlint
         jq
         moreutils
         nix
         (pkgs.pkg-config or pkgconfig)
         profiteur
         sqlite-interactive
         time
         tmux
         util-linux
       ])
    ++ (with pkgs.haskellPackages; [
         ghc-prof-flamegraph
         graphmod
         profiterole
         weeder
       ])
    # Packages in need of a newer versions compared to flake's nixpkgs.
    # Pinning "nixos-25.11" to avoid cache misses when entering the shell.
    # To update use `curl -L https://channels.nixos.org/nixos-25.11/git-revision`
    ++ (with (builtins.getFlake "github:NixOS/nixpkgs/999ca0e5484922624254294ea1adc2b90081579e").legacyPackages.${pkgs.stdenv.hostPlatform.system}; [
         # Will be removed once nixpkgs is bumped to a suitable version.
         typst
       ])
    ++
    ## Cabal run flag:
    # Include the packages or the tools to build them (see `lib-cabal.sh`).
    (if !useCabalRun
     then
       (with project.exes; [
         # A `notGitRev` version, faster to enter a workbench after a new commit.
         cardano-node
         cardano-tracer
         locli
         # A `notGitRev` version, faster to enter a workbench after a new commit.
         tx-generator
       ])
     else
       (with pkgs; [
         pkgs.cabal-install
         pkgs.ghcid
         pkgs.haskellBuildUtils
         pkgs.cabal-plan
       ])
    )
    ++ (with project.hsPkgs; [
        # A `notGitRev` version, faster to enter a workbench after a new commit.
        cardano-cli.components.exes.cardano-cli
        ouroboros-consensus.components.exes.db-analyser
       ])
    ++ [
        # Publish
        pkgs.bench-data-publish
        # Debugging
        pkgs.postgresql
    ]
    ++ lib.optional haveGlibcLocales pkgs.glibcLocales
    ## Include useful profiling helper programs.
    ++ [
         # For the legacy prog.hp format.
         # Which has been deprecated in favour of eventlog based profiling.
         pkgs.haskellPackages.hp2pretty
         pkgs.haskellPackages.hp2html
         pkgs.haskellPackages.eventlog2html
       ]
    ;
  }
;
}
