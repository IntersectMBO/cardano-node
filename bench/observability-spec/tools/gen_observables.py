import json, sys
T = lambda n: {"$ref": "types.schema.json#/$defs/" + n}
def obj(req, opt=None):
    props = dict(req); props.update(opt or {})
    return {"type": "object", "required": list(req.keys()), "properties": props}
slot = {"slot": T("SlotNo")}
peer = {"peer": T("Peer")}

payloads = {
  "Resources": obj(
    {k: T("Count") for k in ["CentiCpu", "Threads"]} | {k: T("Size") for k in ["RSS", "FsRd", "FsWr"]},
    {k: T("Size") for k in ["NetRd", "NetWr", "Alloc", "Live", "Heap"]}
    | {k: T("Count") for k in ["CentiBlkIO", "CentiMut", "CentiGC", "GcsMajor", "GcsMinor"]}),
  "Forge.Loop.StartLeadershipCheck": obj(slot),
  "Forge.Loop.BlockContext": obj(slot | {"blockNo": T("BlockNo")}, {"prev": T("Point")}),
  "Forge.Loop.LedgerState": obj(slot),
  "Forge.Loop.LedgerView": obj(slot),
  "Forge.Loop.NodeIsLeader": obj(slot),
  "Forge.Loop.NodeNotLeader": obj(slot),
  "Forge.Loop.ForgeTickedLedgerState": obj(slot, {"prev": T("Point")}),
  "Forge.Loop.ForgingMempoolSnapshot": obj(slot, {"prev": T("Point"), "mempoolHash": T("ChainHash"), "mempoolSlot": T("SlotNo")}),
  "Forge.Loop.ForgedBlock": obj(slot | {"block": T("BlockHash"), "blockNo": T("BlockNo"), "blockPrev": T("ChainHash")}),
  "Forge.Loop.AdoptedBlock": obj(slot | {"blockHash": T("BlockHash"), "blockSize": T("Size")}),
  "ChainSync.Client.DownloadedHeader": obj({"block": T("BlockHash"), "blockNo": T("BlockNo"), "slot": T("SlotNo")}, peer),
  "BlockFetch.Client.SendFetchRequest": obj({"head": T("ChainHash"), "length": {"type": "integer", "minimum": 1}}, peer),
  "BlockFetch.Client.CompletedBlockFetch": obj({"block": T("BlockHash")}, {"size": T("Size"), "delay": T("Duration")} | peer),
  "ChainSync.ServerHeader.Update": {
    "type": "object",
    "required": ["blockingRead"],
    "properties": {"addBlock": T("Point"), "rollBackTo": T("Point"), "blockingRead": {"type": "boolean"},
                   "risingEdge": {"const": True}} | peer,
    "oneOf": [{"required": ["addBlock"], "not": {"required": ["rollBackTo"]}},
              {"required": ["rollBackTo"], "not": {"required": ["addBlock"]}}]},
  "BlockFetch.Server.SendBlock": obj({"block": T("ChainHash")}, peer),
  "ChainDB.AddBlockEvent.AddedToCurrentChain": obj({"newtip": T("Point")}),
  "ChainDB.AddBlockEvent.SwitchedToAFork": obj({"newtip": T("Point")}),
  "ChainDB.LedgerEvent.Snapshot.TookSnapshot": obj({"enclosedTime": T("EdgeTiming")}, {"tip": T("Point")}),
  "LedgerMetrics": obj(slot | {"utxoSize": T("Count"), "chainDensity": {"type": "number", "minimum": 0}}, {"delegMapSize": T("Count")}),
  "TxSubmission.TxInbound.Collected": obj({"count": T("Count")} | peer),
  "TxSubmission.TxInbound.Processed": obj({"accepted": T("Count")} | peer, {"rejected": T("Count")}),
  "Mempool.AddedTx": obj({"mempoolSize": T("MempoolSize")}, {"tx": T("TxRef")}),
  "Mempool.RemoveTxs": obj({"mempoolSize": T("MempoolSize")},
                           {"txs": {"type": "array", "items": {"type": "object", "required": ["tx"], "properties": {"tx": T("TxRef")}}}}),
  "Mempool.RejectedTx": obj({}, {"mempoolSize": T("MempoolSize"), "tx": T("TxRef")}),
}
for p in payloads.values():
    if not p.get("required"):
        p.pop("required", None)

schema = {
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "observables.schema.json",
  "title": "Performance observable event",
  "description": "One line of a log stream: the envelope, and for namespaces in the catalogue (specification section 5) the payload's required fields with canonical syntax. Events of other namespaces are only checked against the envelope. Generated; edit gen_observables.py instead.",
  "allOf": [{"$ref": "envelope.schema.json"}] + [
      {"if": {"required": ["ns"], "properties": {"ns": {"const": ns}}},
       "then": {"properties": {"data": {"$ref": "#/$defs/" + ns}}}}
      for ns in payloads],
  "$defs": payloads,
}
json.dump(schema, sys.stdout, indent=2)
print()
