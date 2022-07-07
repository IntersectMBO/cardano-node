## define_proftarget :: target -> profile -> fullnixmode -> devmode -> autostart -> autostay -> profiled -> IO ()
define define_profile_target
$(1): shell
$(1): PROFILE = $(2)-${ERA}
ifeq ($(3),true)
$(1): ARGS += --arg 'useCabalRun' false
endif
ifeq ($(4),true)
$(1): ARGS += --arg 'workbenchDevMode' true
endif
ifeq ($(7),true)
$(1): ARGS += --arg 'profiled' true
endif
ifeq ($(5)$(6),truetrue)
$(1): CMD := start-cluster $(if ${ITER},--iterations ${ITER}); return
endif
ifeq ($(5)$(6),truefalse)
$(1): RUN := start-cluster $(if ${ITER},--iterations ${ITER})
endif
ifeq ($(3)$(4)$(5)$(6),falsetruefalsefalse)
define EXTRA_HELP +=
$(1): ## Shell for profile \033[34m$(2)\033[0m  (also: \033[34m-auto -autostay -nix -autonix -prof\033[0m)\n
endef
endif

endef

define define_profile_targets
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof),$$(prof),false,true,false,false,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-prof,$$(prof),false,true,false,false,true)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-auto,$$(prof),false,true,true,false,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-autostay,$$(prof),false,true,true,true,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-nix,$$(prof),true,false,false,false,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-autonix,$$(prof),true,false,true,false,false)))
endef
