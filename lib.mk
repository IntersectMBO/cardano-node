## define_proftarget :: target -> profile -> devmode -> autostart -> IO ()
define define_profile_target
$(1): shell
$(1): PROFILE = $(2)-${ERA}
ifeq ($(3),true)
$(1): ARGS += --arg 'workbenchDevMode' true
endif
ifeq ($(4),true)
$(1): CMD := start-cluster; exit
endif
ifeq ($(3)$(4),truefalse)
define EXTRA_HELP +=
$(1): ## Shell for profile \033[34m$(2)\033[0m  (also: \033[34m-auto -nix -autonix\033[0m)\n
endef
endif

endef

define define_profile_targets
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof),$$(prof),true,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-auto,$$(prof),true,true)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-nix,$$(prof),false,false)))
$$(foreach prof,$(1),$$(eval $$(call define_profile_target,$$(prof)-autonix,$$(prof),false,true)))
endef
