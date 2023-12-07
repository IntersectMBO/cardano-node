
## proftgt :: target -> profile -> fullnixmode -> devmode -> autostart -> autostay -> profiled -> backend -> IO ()
define proftgt
$(1): shell
$(1): PROFILE = $(2)-${ERA}
$(1): BACKEND = $(8)
ifeq ($(strip $(3)),true)
$(1): ARGS += --arg 'useCabalRun' false
endif
ifeq ($(strip $(4)),true)
$(1): ARGS += --arg 'workbenchDevMode' true
endif
ifeq ($(strip $(7)),true)
$(1): ARGS += --arg 'profiling' '"$(WB_PROFILING)"'
else
$(1): ARGS += --arg 'profiling' '"none"'
endif
ifeq ($(strip $(5))$(strip $(6)),truetrue)
$(1): CMD := start-cluster $(if ${ITER},--iterations ${ITER}) $(if ${ID},--ident ${ID}) $(if ${BATCH},--batch-name ${BATCH}); return
endif
ifeq ($(strip $(5))$(strip $(6)),truefalse)
$(1): RUN := start-cluster $(if ${ITER},--iterations ${ITER}) $(if ${ID},--ident ${ID}) $(if ${BATCH},--batch-name ${BATCH})
endif
ifeq ($(strip $(3))$(strip $(4))$(strip $(5))$(strip $(6)),falsetruefalsefalse)
define EXTRA_HELP +=
$(1): ## Shell for profile \033[34m$(2)\033[0m  (also: \033[34m-auto -autostay -nix -autonix -prof\033[0m)\n
endef
endif
endef

define define_profile_targets
ID ?= $(shell git symbolic-ref HEAD | sed 's_/_\n_g' | tail -n1)
##                                           defining this target       profname  nix   dev   auto  stay profiled  backend
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof),                  $$(prof),false, true,false,false, false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-prof,             $$(prof),false, true,false,false,  true, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autoprof,         $$(prof),false, true, true,false,  true, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-profnix,          $$(prof), true, true,false,false,  true, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autoprofnix,      $$(prof), true, true, true,false,  true, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-auto,             $$(prof),false, true, true,false, false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autostay,         $$(prof),false, true, true, true, false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nix,              $$(prof), true,false,false,false, false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autonix,          $$(prof), true,false, true,false, false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadexec,        $$(prof), true,false,false,false, false, nomadexec)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadexec-auto,   $$(prof), true,false, true,false, false, nomadexec)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadpodman,      $$(prof), true,false,false,false, false, nomadpodman)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadpodman-auto, $$(prof), true,false, true,false, false, nomadpodman)))
endef

define define_profile_targets_nomadcloud
##                                           defining this target       profname  nix   dev   auto  stay profiled  backend
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof),                  $$(prof), true,false,false,false, false, nomadcloud)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-auto,             $$(prof), true,false, true,false, false, nomadcloud)))
endef
