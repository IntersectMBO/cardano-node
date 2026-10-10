## =============================================================================
## Workbench Make targets: envars, the `shell` entrypoint, per-profile shell
## aliases (`make <profile>[-VARIANT]`) and utility targets.
## =============================================================================

## Profile name lists (LOCAL_PROFILES / CLOUD_PROFILES).
## Regenerate with `make profiles-regenerate`.
include $(dir $(lastword $(MAKEFILE_LIST)))profiles.mk

###
### Profile discovery
###

## list all workbench profile names (JSON array).
list-profiles:
	nix run .#cardano-profile names

## Show one profile's config, pretty-printed: make show-profile NAME=<profile>.
show-profile:
	@test -n "${NAME}" || { echo 'HELP:  add NAME=<profile>  (make ps lists them)'; exit 1; }
	nix run .#cardano-profile by-name-pretty ${NAME}

## List profile names, one per line.
ps:
	@nix run .#cardano-profile names | tr -d '[]"' | tr ',' '\n'

## Regenerate profiles.mk (the profile name lists).
profiles-regenerate:
	nix run .#cardano-profile lib-make > nix/workbench/profiles.mk

.PHONY: list-profiles show-profile ps profiles-regenerate

###
### Shell entrypoint
###
## `make shell` (top-level Makefile) or `make <profile>[-VARIANT]` enters the
## workbench shell. The subsections below build that: the knobs, the nix-shell
## command, the per-profile aliases, and a couple of extra aliases.

## Envars
## Knobs for `shell` and the aliases. Override any on the command line, e.g.
##   make PROFILE=ci-test ERA=conway BACKEND=nomadexec shell
## The per-profile aliases (`make <profile>[-VARIANT]`, further below) preset
## PROFILE, BACKEND and the nix/profiling flags for you, so you rarely set these
## by hand.

## Profile to enter / run. `make ps` lists the available ones.
PROFILE ?= default

## Cardano era: shelley allegra mary alonzo babbage conway dijkstra.
ERA     ?= conway

## Cluster backend: supervisor (local) | nomadexec (local Nomad) | nomadcloud (remote).
BACKEND ?= supervisor

## -auto* runs: start-cluster --iterations.
ITER    ?=

## -auto* runs: start-cluster --batch-name.
BATCH   ?=

## Extra args appended to the nix-shell invocation.
ARGS    ?=

## Run this command inside the shell, non-interactively (nix-shell --command).
CMD     ?=

## Like CMD, but the shell exits afterwards (nix-shell --run).
RUN     ?=

## WB_ENTER  (the nix-shell command)
## The single nix-shell invocation. The top-level Makefile's shell / -nix /
## -prof / -dev targets run it; every per-profile alias below runs it with
## PROFILE and flags preset.
WB_ENTER = nix-shell -A 'workbench-shell' --max-jobs 8 --cores 0 --show-trace \
  --argstr profileName $(PROFILE) --argstr eraName $(ERA) --argstr backendName $(BACKEND) \
  $(ARGS) $(if $(CMD),--command "$(CMD)") $(if $(RUN),--run "$(RUN)")

## Per-profile aliases  (make <profile>[-VARIANT])
## `make <profile>` (and `<profile>-VARIANT`) is shorthand for `make shell` with
## PROFILE and the right flags preset. Each variant is ONE literal, greppable
## stanzas, built with $(addsuffix ...) and no eval/call/foreach.
##
## Suffix axes (profilingType is always forwarded from $(WB_PROFILING)):
##   nix      -> ARG_NIX_BINARY_YES/NO  yes = Nix-store binary (useCabalRun false).
##                                      no  = `cabal build` on-demand.
##   prof     -> ARG_PROFILED_YES/NO    yes = build everything profiled (-prof).
##                                      no  = not profiled.
##   auto     -> autostart cluster (--run), shell exits when the run finishes.
##   autostay -> autostart cluster, then stay in the shell (--command "...; return").
##   dev      -> ARG_DEV_MODE_YES/NO    yes = editable workbench scripts; ALWAYS yes here (see NOTE).
##                                      no  = workbench from the Nix store (unused; see NOTE).
##   backend  -> supervisor (default)
##             | nomadexec  (local Nomad)
##             | nomadcloud (remote Nomad)
##
## Variant -> flags.
## Every nix/dev/prof toggle is emitted explicitly, no nix-shell default is relied on).
##
## NOTE: dev is 'x' for every variant: workbenchDevMode defaults to true and
##       nothing here passes false, so dev mode is always on. The old proftgt
##       meant to turn it off for -nix / -nomadexec / cloud, but it only ever
##       emitted 'true' (never 'false'), so with the default already true (from
##       nix/custom-config.nix) that off-setting was a no-op bug. We keep the
##       same effective behavior; ARG_DEV_MODE_NO exists to wire it up.
##   suffix            nix prof auto autostay   dev backend      purpose
##   (base)            .   .    .    .          x   supervisor   dev shell; cabal builds/runs on demand
##   -nix              x   .    .    .          x   supervisor   run the Nix-store binary (no cabal)
##   -prof             .   x    .    .          x   supervisor   base + profiled build (for -p, -hc, ... modes)
##   -profnix          x   x    .    .          x   supervisor   profiled, run the Nix-store binary
##   -auto             .   .    x    .          x   supervisor   base + autostart cluster, exit when done
##   -autonix          x   .    x    .          x   supervisor   -nix + autostart cluster, exit when done
##   -autoprof         .   x    x    .          x   supervisor   -prof + autostart cluster, exit when done
##   -autoprofnix      x   x    x    .          x   supervisor   -profnix + autostart cluster, exit when done
##   -autostay         .   .    .    x          x   supervisor   base + autostart, then stay in the shell
##   -nomadexec        x   .    .    .          x   nomadexec    Nix-store binary on Nomad exec (local) backend
##   -nomadexec-auto   x   .    x    .          x   nomadexec    -nomadexec + autostart cluster
##   (cloud base)      x   .    .    .          x   nomadcloud   cloud profiles on the Nomad cloud backend
##   (cloud -auto)     x   .    x    .          x   nomadcloud   cloud base + autostart cluster

## Reusable nix-shell --arg fragments: every boolean toggle has an explicit
## YES/NO so no nix-shell default is ever relied on.
## ARG_PROFILING_TYPE is recursive so WB_PROFILING resolves at run time.
## ARG_DEV_MODE_NO is currently unused (dev mode is kept on for every variant,
## see the NOTE above the matrix).
ARG_NIX_BINARY_YES := --arg useCabalRun false
ARG_NIX_BINARY_NO  := --arg useCabalRun true
ARG_DEV_MODE_YES   := --arg workbenchDevMode true
ARG_DEV_MODE_NO    := --arg workbenchDevMode false
ARG_PROFILED_YES   := --arg profiledBuild true
ARG_PROFILED_NO    := --arg profiledBuild false
ARG_PROFILING_TYPE  = --arg profilingType '"$(WB_PROFILING)"'

## Cluster autostart command, shared by all -auto* / -autostay variants.
ID ?= $(shell git symbolic-ref HEAD | sed 's_/_\n_g' | tail -n1)
AUTOSTART = start-cluster $(if $(ITER),--iterations $(ITER)) $(if $(ID),--ident $(ID)) $(if $(BATCH),--batch-name $(BATCH))

## One explicit target list per variant, derived from the profile lists.
TARGETS_LOCAL_SUPERVISOR_BASE        := $(LOCAL_PROFILES)
TARGETS_LOCAL_SUPERVISOR_NIX         := $(addsuffix -nix,$(LOCAL_PROFILES))
TARGETS_LOCAL_SUPERVISOR_PROF        := $(addsuffix -prof,$(LOCAL_PROFILES))
TARGETS_LOCAL_SUPERVISOR_PROFNIX     := $(addsuffix -profnix,$(LOCAL_PROFILES))
TARGETS_LOCAL_SUPERVISOR_AUTO        := $(addsuffix -auto,$(LOCAL_PROFILES))
TARGETS_LOCAL_SUPERVISOR_AUTONIX     := $(addsuffix -autonix,$(LOCAL_PROFILES))
TARGETS_LOCAL_SUPERVISOR_AUTOPROF    := $(addsuffix -autoprof,$(LOCAL_PROFILES))
TARGETS_LOCAL_SUPERVISOR_AUTOPROFNIX := $(addsuffix -autoprofnix,$(LOCAL_PROFILES))
TARGETS_LOCAL_SUPERVISOR_AUTOSTAY    := $(addsuffix -autostay,$(LOCAL_PROFILES))
TARGETS_LOCAL_NOMADEXEC_BASE         := $(addsuffix -nomadexec,$(LOCAL_PROFILES))
TARGETS_LOCAL_NOMADEXEC_AUTO         := $(addsuffix -nomadexec-auto,$(LOCAL_PROFILES))
TARGETS_CLOUD_BASE                   := $(CLOUD_PROFILES)
TARGETS_CLOUD_AUTO                   := $(addsuffix -auto,$(CLOUD_PROFILES))

.PHONY: $(TARGETS_LOCAL_SUPERVISOR_BASE) $(TARGETS_LOCAL_SUPERVISOR_NIX) $(TARGETS_LOCAL_SUPERVISOR_PROF) $(TARGETS_LOCAL_SUPERVISOR_PROFNIX) $(TARGETS_LOCAL_SUPERVISOR_AUTO) $(TARGETS_LOCAL_SUPERVISOR_AUTONIX) $(TARGETS_LOCAL_SUPERVISOR_AUTOPROF) $(TARGETS_LOCAL_SUPERVISOR_AUTOPROFNIX) $(TARGETS_LOCAL_SUPERVISOR_AUTOSTAY) $(TARGETS_LOCAL_NOMADEXEC_BASE) $(TARGETS_LOCAL_NOMADEXEC_AUTO) $(TARGETS_CLOUD_BASE) $(TARGETS_CLOUD_AUTO)

## Base targets have no stem, so set PROFILE from $@; suffixed targets are static
## pattern rules, so set PROFILE from $* (the stem before the suffix).

## base: dev shell, cabal, supervisor.
$(TARGETS_LOCAL_SUPERVISOR_BASE):        PROFILE = $@
$(TARGETS_LOCAL_SUPERVISOR_BASE):        BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_BASE):        ARGS += $(ARG_NIX_BINARY_NO) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_BASE):        ; $(WB_ENTER)

## -nix: Nix-store binary.
$(TARGETS_LOCAL_SUPERVISOR_NIX):         PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_NIX):         BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_NIX):         ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_NIX):         %-nix: ; $(WB_ENTER)

## -prof: + profiled build.
$(TARGETS_LOCAL_SUPERVISOR_PROF):        PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_PROF):        BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_PROF):        ARGS += $(ARG_NIX_BINARY_NO) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_YES) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_PROF):        %-prof: ; $(WB_ENTER)

## -profnix: profiled + Nix-store binary.
$(TARGETS_LOCAL_SUPERVISOR_PROFNIX):     PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_PROFNIX):     BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_PROFNIX):     ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_YES) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_PROFNIX):     %-profnix: ; $(WB_ENTER)

## -auto: dev shell + autostart, exit on finish.
$(TARGETS_LOCAL_SUPERVISOR_AUTO):        PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_AUTO):        BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_AUTO):        RUN  = $(AUTOSTART)
$(TARGETS_LOCAL_SUPERVISOR_AUTO):        ARGS += $(ARG_NIX_BINARY_NO) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_AUTO):        %-auto: ; $(WB_ENTER)

## -autonix: Nix-store binary + autostart.
$(TARGETS_LOCAL_SUPERVISOR_AUTONIX):     PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_AUTONIX):     BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_AUTONIX):     RUN  = $(AUTOSTART)
$(TARGETS_LOCAL_SUPERVISOR_AUTONIX):     ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_AUTONIX):     %-autonix: ; $(WB_ENTER)

## -autoprof: profiled + autostart.
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROF):    PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROF):    BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROF):    RUN  = $(AUTOSTART)
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROF):    ARGS += $(ARG_NIX_BINARY_NO) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_YES) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROF):    %-autoprof: ; $(WB_ENTER)

## -autoprofnix: profiled + Nix-store binary + autostart.
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROFNIX): PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROFNIX): BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROFNIX): RUN  = $(AUTOSTART)
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROFNIX): ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_YES) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_AUTOPROFNIX): %-autoprofnix: ; $(WB_ENTER)

## -autostay: dev shell + autostart, stay in the shell afterwards.
$(TARGETS_LOCAL_SUPERVISOR_AUTOSTAY):    PROFILE = $*
$(TARGETS_LOCAL_SUPERVISOR_AUTOSTAY):    BACKEND = supervisor
$(TARGETS_LOCAL_SUPERVISOR_AUTOSTAY):    CMD  = $(AUTOSTART); return
$(TARGETS_LOCAL_SUPERVISOR_AUTOSTAY):    ARGS += $(ARG_NIX_BINARY_NO) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_SUPERVISOR_AUTOSTAY):    %-autostay: ; $(WB_ENTER)

## -nomadexec: Nomad exec (local) backend.
$(TARGETS_LOCAL_NOMADEXEC_BASE):         PROFILE = $*
$(TARGETS_LOCAL_NOMADEXEC_BASE):         BACKEND = nomadexec
$(TARGETS_LOCAL_NOMADEXEC_BASE):         ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_NOMADEXEC_BASE):         %-nomadexec: ; $(WB_ENTER)

## -nomadexec-auto: Nomad exec (local) + autostart.
$(TARGETS_LOCAL_NOMADEXEC_AUTO):         PROFILE = $*
$(TARGETS_LOCAL_NOMADEXEC_AUTO):         BACKEND = nomadexec
$(TARGETS_LOCAL_NOMADEXEC_AUTO):         RUN  = $(AUTOSTART)
$(TARGETS_LOCAL_NOMADEXEC_AUTO):         ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_LOCAL_NOMADEXEC_AUTO):         %-nomadexec-auto: ; $(WB_ENTER)

## cloud base: Nomad cloud backend.
$(TARGETS_CLOUD_BASE):                   PROFILE = $@
$(TARGETS_CLOUD_BASE):                   BACKEND = nomadcloud
$(TARGETS_CLOUD_BASE):                   ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_CLOUD_BASE):                   ; $(WB_ENTER)

## cloud -auto: Nomad cloud + autostart.
$(TARGETS_CLOUD_AUTO):                   PROFILE = $*
$(TARGETS_CLOUD_AUTO):                   BACKEND = nomadcloud
$(TARGETS_CLOUD_AUTO):                   RUN  = $(AUTOSTART)
$(TARGETS_CLOUD_AUTO):                   ARGS += $(ARG_NIX_BINARY_YES) $(ARG_DEV_MODE_YES) $(ARG_PROFILED_NO) $(ARG_PROFILING_TYPE)
$(TARGETS_CLOUD_AUTO):                   %-auto: ; $(WB_ENTER)

## Other aliases: 6-dense-* (RTS profiling), playground (arbitrary profile).
## Aliases for removed profiles: inject RTS -hT (space-heap) + -l (eventlog) as
## shell params on a NON-profiled build (neither needs a profiled runtime), via
## the base target.

6-dense-rtsprof:
	WB_PROFILING="space-heap+eventlog" $(MAKE) 6-dense
6-dense-1h-rtsprof:
	WB_PROFILING="space-heap+eventlog" $(MAKE) 6-dense-1h
6-dense-4h-rtsprof:
	WB_PROFILING="space-heap+eventlog" $(MAKE) 6-dense-4h

## make playground-<name>  ->  supervisor dev shell for an arbitrary profile name.
playground-%:
	$(MAKE) shell PROFILE=$* BACKEND=supervisor

.PHONY: 6-dense-rtsprof 6-dense-1h-rtsprof 6-dense-4h-rtsprof

###
### Run utils  (analyse, clean)
###

analyse: RUN := wb analyse std ${TAG}
analyse: shell

.PHONY: analyse

## Workbench artifacts left in the working tree; top-level `make clean` calls this.
clean-profile proclean:
	rm -f  *.html *.prof *.hp *.stats *.eventlog
	rm -rf logs/ socket/ cluster.*

.PHONY: clean-profile proclean

###
### Docs
###

## Open the workbench Nix internals walkthrough (doc.org).
workbench-internals-walkthrough:
	emn nix/workbench/doc.org

.PHONY: workbench-internals-walkthrough

