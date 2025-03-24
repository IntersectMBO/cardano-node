PROFILES_EMPTY := fast-solo fast fast-p2p fast-oldtracing fast-notracer fast-plutus ci-test ci-test-rtview ci-test-notracer ci-test-p2p ci-test-plutus ci-test-hydra trace-bench trace-bench-rtview trace-bench-oldtracing trace-bench-notracer trace-full trace-full-rtview default default-p2p oldtracing plutus plutus-secp-ecdsa plutus-secp-schnorr epoch-transition
PROFILES_MINIATURE := ci-bench ci-bench-lmdb ci-bench-rtview ci-bench-p2p ci-bench-notracer ci-bench-drep ci-bench-plutus ci-bench-plutus24 ci-bench-plutus-secp-ecdsa ci-bench-plutus-secp-schnorr ci-bench-plutusv3-blst ci-bench-plutusv3-ripemd ci-bench-plutusv3-ripemd-step2x 10 10-p2p 10-notracer 10-plutus 6-dense 6-dense-rtsprof 6-dense-1h 6-dense-1h-rtsprof 6-dense-4h 6-dense-4h-rtsprof
PROFILES_FORGE_STRESS := forge-stress-solo-xs forge-stress-solo forge-stress-plutus-solo forge-stress-pre-solo-xs forge-stress-pre-solo forge-stress-pre-solo-xl forge-stress forge-stress-notracer forge-stress-p2p forge-stress-plutus forge-stress-large forge-stress-pre forge-stress-pre-rtsA4m forge-stress-pre-rtsA64m forge-stress-pre-rtsN3 forge-stress-pre-rtsA4mN3 forge-stress-pre-rtsA64mN3 forge-stress-pre-rtsxn forge-stress-pre-notracer forge-stress-pre-plutus forge-stress-pre-large forge-stress-pre-large-rtsqg1 forge-stress-pre-large-rtsN3 forge-stress-pre-large-rtsN4 forge-stress-pre-large-rtsqg1N4
PROFILES_PLUTUSCALL := plutuscall-loop plutuscall-loop-memx2 plutuscall-secp-ecdsa plutuscall-secp-ecdsa-stepx2 plutuscall-secp-schnorr plutuscall-secp-schnorr-stepx2 plutuscall-volt-loop plutuscall-volt-blst plutuscall-volt-ripemd
PROFILES_MODEL := model-secp-ecdsa-stepx2 model-secp-ecdsa model-value model-value-test
PROFILES_K3 := k3-3ep-5kTx-10000kU-1300kD-64kbs-fixed-loaded k3-3ep-9kTx-10000kU-1300kD-64kbs-5tps-fixed-loaded k3-3ep-18kTx-10000kU-1300kD-64kbs-10tps-fixed-loaded
PROFILES_SCENARIOS := chainsync-early-byron chainsync-early-byron-notracer chainsync-early-byron-oldtracing chainsync-early-alonzo chainsync-early-alonzo-notracer chainsync-early-alonzo-p2p chainsync-early-alonzo-oldtracing devops idle tracer-only
PROFILES_LEGACY := ci-test-dense10 dish dish-10M dish-plutus dish-10M-plutus
PROFILES_SCALING := faststartup-24M
PROFILES_NOMAD_PERF := value-nomadperf value-nomadperf-nop2p value-drep1k-nomadperf value-drep10k-nomadperf value-drep100k-nomadperf value-oldtracing-nomadperf value-oldtracing-nomadperf-nop2p value-volt-nomadperf value-volt-rtsqg1-nomadperf plutus-nomadperf plutus-nomadperf-nop2p plutus-drep1k-nomadperf plutus-drep10k-nomadperf plutus-drep100k-nomadperf plutus24-nomadperf plutus-secp-ecdsa-nomadperf plutus-secp-schnorr-nomadperf plutus-volt-nomadperf plutus-volt-memx15-nomadperf plutus-volt-memx2-nomadperf plutus-volt-rtsqg1-nomadperf plutusv3-blst-nomadperf plutusv3-blst-stepx15-nomadperf plutusv3-blst-stepx2-nomadperf plutusv3-ripemd-nomadperf plutusv3-ripemd-stepx15-nomadperf plutusv3-ripemd-stepx2-nomadperf value-voting-utxo-volt-nomadperf value-voting-volt-nomadperf value-voting-double-volt-nomadperf plutus-voting-utxo-volt-nomadperf plutus-voting-volt-nomadperf plutus-voting-double-volt-nomadperf latency-nomadperf fast-nomadperf fast-nomadperf-nop2p ci-test-nomadperf ci-test-nomadperf-nop2p ci-test-oldtracing-nomadperf default-nomadperf-nop2p default-nomadperf oldtracing-nomadperf oldtracing-nomadperf-nop2p ci-bench-nomadperf ci-bench-nomadperf-nop2p ci-bench-oldtracing-nomadperf
PROFILES_NOMAD_PERFSSD := utxoscale-solo-12M16G-nomadperfssd utxoscale-solo-12M64G-nomadperfssd utxoscale-solo-24M64G-nomadperfssd fast-nomadperfssd value-nomadperfssd latency-nomadperfssd

LOCAL_PROFILES += $(PROFILES_EMPTY)
LOCAL_PROFILES += $(PROFILES_MINIATURE)
LOCAL_PROFILES += $(PROFILES_FORGE_STRESS)
LOCAL_PROFILES += $(PROFILES_PLUTUSCALL)
LOCAL_PROFILES += $(PROFILES_MODEL)
LOCAL_PROFILES += $(PROFILES_K3)
LOCAL_PROFILES += $(PROFILES_SCENARIOS)
LOCAL_PROFILES += $(PROFILES_LEGACY)
LOCAL_PROFILES += $(PROFILES_SCALING)
CLOUD_PROFILES += $(PROFILES_NOMAD_PERF)
CLOUD_PROFILES += $(PROFILES_NOMAD_PERFSSD)
