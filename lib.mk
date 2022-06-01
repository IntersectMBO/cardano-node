## define_proftarget :: target -> profile -> fullnixmode -> devmode -> autostart -> autostay -> IO ()
define define_profile_target
$(1): shell
$(1): PROFILE = $(2)-${ERA}
ifeq ($(3),true)
$(1): ARGS += --arg 'useCabalRun' false
endif
ifeq ($(4),true)
$(1): ARGS += --arg 'workbenchDevMode' true
endif
ifeq ($(5)$(6),truetrue)
$(1): CMD := start-cluster
endif
ifeq ($(5)$(6),truefalse)
$(1): CMD := start-cluster && exit || exit 1
endif
ifeq ($(3)$(4)$(5)$(6),falsetruefalsefalse)
define EXTRA_HELP +=
$(1): ## Shell for profile \033[34m$(2)\033[0m  (also: \033[34m-auto -autostay -nix -autonix\033[0m)\n
endef
endif

endef

define define_profile_targets
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof),$$(prof),false,true,false,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-auto,$$(prof),false,true,true,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-autostay,$$(prof),false,true,true,true)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-nix,$$(prof),true,false,false,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-autonix,$$(prof),true,false,true,false)))
endef
