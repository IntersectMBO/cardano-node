
## proftgt :: target -> profile -> pure -> useCabalRun -> profiled -> devmode -> autostart -> autostay -> backend -> IO ()
define proftgt
# Target
$(1): shell
# Profile name
$(1): PROFILE = $(2)-${ERA}
# Pure Nix shell
ifeq ($(strip $(3)),true)
$(1): ARGS += --pure
endif
# Use Cabal instead of full Nix mode
ifeq ($(strip $(4)),true)
$(1): ARGS += --arg 'useCabalRun' true
else
$(1): ARGS += --arg 'useCabalRun' false
endif
# Profiling
ifeq ($(strip $(5)),true)
$(1): ARGS += --arg 'profiling' '"$(WB_PROFILING)"'
else
$(1): ARGS += --arg 'profiling' '"none"'
endif
# DevMode
ifeq ($(strip $(6)),true)
$(1): ARGS += --arg 'workbenchDevMode' true
else
$(1): ARGS += --arg 'workbenchDevMode' false
endif
# autostart and autostay
ifeq ($(strip $(7))$(strip $(8)),truetrue)
$(1): CMD := start-cluster $(if ${ITER},--iterations ${ITER}) $(if ${ID},--ident ${ID}); return
endif
# autostart and no autostay
ifeq ($(strip $(7))$(strip $(8)),truefalse)
$(1): RUN := start-cluster $(if ${ITER},--iterations ${ITER}) $(if ${ID},--ident ${ID})
endif
# Backend name
$(1): BACKEND = $(9)
# Others
ifeq ($(strip $(4))$(strip $(6))$(strip $(7))$(strip $(8)),truetruefalsefalse)
define EXTRA_HELP +=
$(1): ## Shell for profile \033[34m$(2)\033[0m  (also: \033[34m-auto -autostay -nix -autonix -prof\033[0m)\n
endef
endif
endef

define define_profile_targets
ID ?= $(shell git symbolic-ref HEAD | sed 's_/_\n_g' | tail -n1)
##                                           defining this target      profname pure cabal profiled dev auto  stay  backend
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof),                 $$(prof), true,false,false,false,false,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-auto,            $$(prof), true,false,false,false, true,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-prof,            $$(prof), true,false, true, true,false,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autoprof,        $$(prof), true,false, true, true, true,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-cabal,           $$(prof), true, true,false, true,false,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autocabal,       $$(prof), true, true,false, true, true,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-profcabal,       $$(prof), true, true, true, true,false,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autoprofcabal,   $$(prof), true, true, true, true, true,false,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autostaycabal,   $$(prof), true, true,false, true, true, true,supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadcloud,      $$(prof), true,false,false,false,false,false,nomadcloud)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadcloud-auto, $$(prof), true,false,false,false, true,false,nomadcloud)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadexec,       $$(prof),false,false,false,false,false,false,nomadexec)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadexec-auto,  $$(prof),false,false,false,false, true,false,nomadexec)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadpodman,     $$(prof),false,false,false,false,false,false,nomadpodman)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomadpodman-auto,$$(prof),false,false,false,false, true,false,nomadpodman)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nixops,          $$(prof), true,false,false, true,false,false,nixops)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autonixops,      $$(prof), true,false,false, true, true, true,nixops)))
endef
