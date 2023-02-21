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
$(1): ARGS += --arg 'profiled' true
endif
ifeq ($(strip $(5))$(strip $(6)),truetrue)
$(1): CMD := start-cluster $(if ${ITER},--iterations ${ITER}); return
endif
ifeq ($(strip $(5))$(strip $(6)),truefalse)
$(1): RUN := start-cluster $(if ${ITER},--iterations ${ITER})
endif
ifeq ($(strip $(3))$(strip $(4))$(strip $(5))$(strip $(6)),falsetruefalsefalse)
define EXTRA_HELP +=
$(1): ## Shell for profile \033[34m$(2)\033[0m  (also: \033[34m-auto -autostay -nix -autonix -prof\033[0m)\n
endef
endif

endef

define define_profile_targets
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof),            $$(prof),false, true,false,false,false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-prof,       $$(prof),false, true,false,false, true, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-auto,       $$(prof),false, true, true,false,false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autostay,   $$(prof),false, true, true, true,false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nix,        $$(prof), true,false,false,false,false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autonix,    $$(prof), true,false, true,false,false, supervisor)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nomad,      $$(prof), true,false,false,false,false, nomad)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autonomad,  $$(prof), true,false, true,false,false, nomad)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-nixops,     $$(prof), true, true,false,false,false, nixops)))
$$(foreach prof,$(1),$$(eval $$(call proftgt,$$(prof)-autonixops, $$(prof), true, true, true, true,false, nixops)))
endef
