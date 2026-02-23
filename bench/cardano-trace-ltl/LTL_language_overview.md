Cardano Trace LTL Overview
==========================

Explain a version of linear temporal logic that is suitable for Cardano trace analysis.

1. Temporal Vocabulary
----------------------
- The logic offers implication, conjunction/disjunction, and negation, plus a small family of temporal constructs: universal and existential quantifiers over trace suffixes, strict and weak “next” operators, bounded repetitions of those next operators, and the classical “until”.
- Weak next behaves like strict next whenever another event exists but is considered satisfied automatically once the trace ends. The bounded repetition variant simply chains that weak obligation across a fixed number of steps, enabling “in the reasonably proximate future” requirements without referencing wall-clock time.
- Atomic formulas represent predicates over properties of individual trace records.

2. Atomic Predicates & Trace Records
------------------------------------
- Each trace record should be normalised into a canonical structure that exposes the metadata the Cardano node already emits: namespace (e.g., `["Forge","Loop","ForgedBlock"]`), severity, host, thread id, slot number, block number, plus an extensible map of additional fields (block hash, failure reason, ledger point, etc.).
- Atomic predicates state facts about that structured record: “namespace equals Forge.ForgedBlock”, “slot equals 423115”, “reason belongs to {BlockFromFuture, SlotIsImmutable}”, “severity is at least Warning”, “field `prev` matches a given block hash pattern”.

3. Trace Model
--------------
- A trace is a finite sequence of the normalised records, ordered by timestamp. Timestamps are not exposed to atomic formulas but instead are handled by the temporal fragment of the logic.

4. Derivative Perspective
-------------------------
- The logic is interpreted via derivatives: consuming an event rewrites the current obligation into a new formula that describes what must hold for the remaining suffix. This mirrors Brzozowski derivatives for regular expressions, extended to the temporal combinators.
- Brzozowski derivatives originate in regular-expression theory: for a regex `R` and an input symbol `a`, the derivative `∂ₐ R` recognises exactly those suffixes `w` for which the concatenation `a·w` belongs to `R`. Iteratively taking derivatives as each symbol arrives lets one perform deterministic matching without constructing an automaton up front. Our temporal setting follows the same idea—treat each log record as the “symbol”, rewrite the obligation, and inspect the residual formula instead of building a separate monitor.

5. Simplification & Verdicts
----------------------------
- After each derivative step we simplify: flatten nested conjunctions/disjunctions, remove redundant `True`/`False`, collapse duplicate subformulas, and optionally drop weak-next obligations just before the last two events of a finite trace.
- To produce a final Boolean verdict on a finite trace, evaluate the simplified residual formula assuming the end of timeline: universal obligations become `True`, atomic formulas evaluate to `False`.

6. Automata & Offline Monitoring
--------------------------------
- Given the alphabet of atomic predicates we care about is finite (e.g., a bounded collection of namespaces and slot-labelled events), the derivative process can only produce finitely many distinct residual formulas. Each residual formula can therefore be treated as a “state” in a deterministic monitor: feed an event, take the derivative, simplify, land in another state. By enumerating all such states and recording which state each event causes you to enter, you effectively synthesise a deterministic automaton or lookup table that implements the original LTL property.


7. Worked Examples (concrete notation)
--------------------------------------
Let `☐ φ` abbreviate “φ must always hold".
Let `◯(k) φ` abbreviate “φ must hold within the next `k` units of time, tolerating early termination” and treat each trace namespace as an atom. The following invariants operate on the standard Cardano forging messages:

1. **Leadership outcome within a window**
   `☐ (StartLeadershipCheck ⇒ ◯(k) (NodeIsLeader ∨ NodeNotLeader))`
   Use a `k` large enough to cover expected intermediate bookkeeping events so overlapping leadership checks don’t interfere.

2. **Forge adoption chain**
   `☐ (ForgedBlock ⇒ ◯(k) (AdoptedBlock ∨ DidntAdoptBlock ∨ ForgedInvalidBlock))`
   Guarantees every forged block is followed swiftly by an adoption verdict; weak next ensures the rule is vacuously true if the node shuts down immediately after forging.

3. **Failure diagnostics lead to cannot-forge**
   `☐ ((SlotIsImmutable ∨ BlockFromFuture ∨ NoLedgerState ∨ NoLedgerView ∨ ForgeStateUpdateError) ⇒ ◯(k) NodeCannotForge)`
   Ensures that every recorded failure reason triggers the explicit `NodeCannotForge` event, preventing silent drops.

4. **Ledger ticking and mempool snapshot sequence**
   `∀ slot. ☐ (NodeIsLeader(slot) ⇒ ◯(k) ForgeTickedLedgerState(slot))`
   `∀ slot. ☐ (ForgeTickedLedgerState(slot) ⇒ ◯(k) ForgingMempoolSnapshot(slot))`
   `∀ slot. ☐ (ForgingMempoolSnapshot(slot) ⇒ ◯(k) ForgedBlock(slot))`
   These bindings force the per-slot forging pipeline to respect the documented ordering, matching the flowchart published in the consensus repository.

5. **Consensus flow edges**
   `☐ (BlockContext ⇒ ◯(k) (LedgerState ∨ NoLedgerState))`
   `☐ (LedgerState ⇒ ◯(k) (LedgerView ∨ NoLedgerView))`
   `☐ (LedgerView ⇒ ◯(k) (ForgeStateUpdateError ∨ NodeCannotForge ∨ NodeNotLeader ∨ NodeIsLeader))`
   Encodes each edge in the forging diagram so that missing or reordered messages are caught immediately.

6. **Immutable or future tip aborts the slot**
   `☐ ((SlotIsImmutable ∨ BlockFromFuture) ⇒ ◯(k) NodeNotLeader)`
   Mirroring the early branches in `Ouroboros/Consensus/Node/Tracers.hs`, the node must record that it will not lead the slot whenever the tip inhabits the same/future slot, preventing it from silently forging on stale ancestors.

7. **Adoption thread crashes bubble up**
   `☐ (AdoptionThreadDied ⇒ ◯(k) NodeCannotForge)`
   If the adoption worker dies (e.g., due to an async exception) the forger cannot safely continue; this invariant forces an explicit `NodeCannotForge` so operators see the failure instead of a quiet stall.

8. **Invalid forge triggers a fresh leadership cycle**
   `∀ slot. ∀ block. ☐ (ForgedInvalidBlock(slot, block) ⇒ ◯(k) (NodeCannotForge(slot) ∧ ◯(m) StartLeadershipCheck(slot+1)))`
   Use the slot numbers embedded in the trace payload to bind the next leadership check to the successor slot. This captures the expectation that forging an invalid block should immediately halt forging for that slot and restart the leadership pipeline for the next slot.

9. **Ledger anchor consistency**
   `∀ slot. ∀ point. LedgerState(slot, point) ⇒ ◯(k) ForgeTickedLedgerState(slot, point)`
   Both events expose the anchor point; comparing the `point` fields ensures the ticker processes exactly the ledger snapshot that was previously fetched, matching the code path between `TraceLedgerState` and `TraceForgeTickedLedgerState`.
