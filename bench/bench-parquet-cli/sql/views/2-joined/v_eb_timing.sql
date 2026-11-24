-- "v_eb_timing": the WHOLE LIFE of every forged endorser block in
-- one row per (EB, peer) — the forging on the left, and what each other node
-- did with it on the right: announced, delivered, written, adopted.
--
-- Tier 2, Leios family: EXPERIMENTAL until the Leios observables are final.
--
-- IT IS THE TWIN OF v_block_timing, column for column, and the one
-- place the two differ is what ADOPTION means; see below.
-- EVERY FORGED EB HAS A ROW, even one no other node ever touched; its peer
-- columns are then NULL. So this view's distinct hashes are v_eb_forged's
-- distinct hashes exactly.
-- A PEER IS ANY NODE BUT THE FORGER that did at least ONE of the four things.
-- The stages are independent: a node can announce an EB it never wrote, and
-- each column is NULL on its own evidence, never because another is.
-- NOTHING IS FILTERED. No minimum adoption count, no fork resolved, no
-- winner picked, no EB dropped for being announced by a block that lost a
-- height battle. Every forged EB gets a row, every node that touched it gets
-- a row, and each event on the path is COUNTED where it happened and TIMED
-- at its first. What those numbers mean is the reader's to decide.
-- WRITING THE EB IS NOT HOLDING ITS TRANSACTIONS. An endorser block is a
-- list of (tx hash, size) references: writing it stores THAT LIST, and the
-- transactions it names are typically still outstanding. The CLOSURE is
-- complete when the node holds every one of them, and the kernel says so in
-- its own trace, "Consensus.LeiosKernel.BlockTxsAcquired" -- the one raw
-- trace this view reads, there being no axiom for it yet. It lands well
-- after the write, so peer_closure_obtained_at is the moment the peer could
-- actually USE the EB; peer_written_at is only when it held the list.
-- AN ENDORSER BLOCK IS ADOPTED WHEN A BLOCK CERTIFYING IT IS ADOPTED. An EB
-- is never put on a chain itself; it reaches one inside the certificate of a
-- ranking block, so a peer holds the EB on its chain exactly when it has
-- adopted some block whose body certifies it -- v_block_forged's
-- certified_eb_hash names which.
-- AN EB CAN BE CERTIFIED BY MORE THAN ONE RANKING BLOCK, rivals at a
-- contested height each carrying the certificate. Nothing here chooses
-- between them: every certifying block's adoption is an event on the path,
-- and peer_adopted_at is the MIN over all of them -- the first moment that
-- peer held the EB by any route. That is an aggregate, not a selection, so
-- no fork is resolved and no certifier is called the real one.
-- peer_adopted_at is NULL for an EB no block ever certified, which is a
-- large share of them, and for a peer that adopted no certifying block.
-- MIN AND COUNT TOGETHER, because the underlying events repeat. The time is
-- the FIRST of its kind and the count says how many there were. Both are
-- taken here, in the pivot; the axioms below collapse nothing.
-- COUNTS ARE OF EVENTS AS TRACED. Unlike the ranking twin, none of the three
-- endorser traces is bracketed, so there is no rising row to filter: every
-- row is its own event and counts as one.
-- Axiomatic means a property of the event as the protocol defines it; the
-- traces are only the evidence, and where no trace evidences a value the cell
-- is NULL, never inferred.
--
--
-- Fields:
-- --------
-- * ts                    The forging (v_eb_forged), and
--   (NOT NULL)
-- * process               the node that forged it, and
--   (NOT NULL)
-- * hash                  the endorser block, and
--   (NOT NULL)
-- * slot_no               its slot: all four repeated on every one of the
--   (NOT NULL)            forging's rows. A hash forged twice has two
--                         forgings, so two sets of rows.
--
-- * slot_start            When that slot began, per genesis.
--   (NOT NULL)
--
-- * bytes                 The endorser block's serialized size.
--   (NOT NULL)
--
-- * forger_announced_at   When the FORGER first announced it to anyone, and
-- * forger_announcements  how many announcements it made. NULL and NULL when
--   (NULL)                it never put the EB on the wire at all.
--
-- * peer_process          The other node this row is about. NULL only on the
--   (NULL)                single row of an EB no other node touched.
--
-- * peer_announced_at     When that peer first announced it, and
-- * peer_announcements    how many announcements it made;
-- * peer_delivered_at     when the body first arrived at that peer, and
-- * peer_deliveries       how many times it was delivered to it;
-- * peer_written_at       when it first wrote the body down, and
-- * peer_writes           how many writes it made;
--   (NULL)
--
-- * peer_closure_obtained_at  When that peer first held every TRANSACTION
-- * peer_closures_obtained    BODY the EB references
--   (NULL)                    (Consensus.LeiosKernel.BlockTxsAcquired), and how
--                             many times the kernel reported it.
--
-- * peer_adopted_at       When it first adopted a ranking block certifying
--   (NULL)                this EB (v_block_forged.certified_eb_hash, then
--                         v_block_adoptions); several certifiers: the earliest.
--
-- The counts are 0, not NULL, for a stage a peer never reached (NULL times).
--
-- Example (in chronological order):
-- --------
-- 
-- at    2026-10-18T18:41:11.274194691Z
-- ns    Consensus.LeiosKernel.BlockForged
-- data  { "closureSize":25718
--       , "ebSize":25718
--       , "hash":"0b302b.."
--       , "mempoolRestSize":0
--       , "numTxs":695
--       , "slot":592
--       }
-- (ts, process, hash, slot_no, bytes -- via v_eb_forged.)
--
-- at    2026-10-18T18:41:11.784593689Z
-- ns    LeiosFetch.Remote.Receive.Block
-- data  { "msg":{ "ebBytesSize":25718
--               , "ebHash":"0b302b.."
--               , "kind":"MsgLeiosBlock"
--               }
--       , "peer":{"connectionId":"172.31.14.36:30005 13.238.107.224:30008"}
--       }
-- (node-5: peer_delivered_at / peer_deliveries)
--
-- at    2026-10-18T18:41:11.791392668Z
-- ns    Consensus.LeiosKernel.BlockAcquired
-- data  { "bodyAgeSeconds":0.784606042
--       , "ebHash":"0b302b.."
--       , "ebSlot":592
--       }
-- (peer_written_at / peer_writes)
--
-- at    2026-10-18T18:41:12.279754418Z
-- ns    Consensus.LeiosKernel.BlockTxsAcquired
-- data  { "closureAgeSeconds":1.272967795
--       , "ebHash":"0b302b.."
--       , "ebSlot":592
--       }
-- (peer_closure_obtained_at / peer_closures_obtained -- every transaction
-- the EB references is now held.)
--
-- at    2026-10-18T18:41:11.792106129Z
-- ns    LeiosNotify.Remote.Send.BlockAnnouncement
-- data  { "msg":{ "ebBodySize":25718
--               , "ebHash":"0b302b.."
--               , "ebSlot":592
--               , "kind":"MsgLeiosBlockAnnouncement"
--               }
--       , "peer":{"connectionId":"172.31.14.36:30005 3.26.59.49:30032"}
--       }
-- (peer_announced_at / peer_announcements -- the peer relaying it onward.)
--
-- at    2026-10-18T18:41:12.371014025Z
-- ns    ChainDB.AddBlockEvent.AddedToCurrentChain
-- data  { "newtip":"a2ab6e..@593"
--       }
-- (peer_adopted_at, and the ONE indirection in this view: the block adopted
-- here is not the EB, it is a ranking block whose certificate certifies it.)
--
CREATE OR REPLACE VIEW v_eb_timing AS
WITH
  certifying AS MATERIALIZED (
    -- Which ranking blocks certify which EB. More than one may, and all of
    -- them count; nothing here picks between them.
    SELECT certified_eb_hash AS eb, hash AS rb
    FROM v_block_forged
    WHERE certified_eb_hash IS NOT NULL),
  acts AS MATERIALIZED (
    -- Every event on the path, one row each, tagged by stage. None of the
    -- endorser traces is bracketed, so no rising-row filter is needed.
          SELECT hash, process, ts, 1 AS k FROM v_eb_announcements
    UNION ALL SELECT hash, process, ts, 2      FROM v_eb_deliveries
    UNION ALL SELECT hash, process, ts, 3      FROM v_eb_writes
    UNION ALL
    -- The closure. No tier-1 view reads this trace, so it is read here.
          SELECT data->>'$.ebHash', process, ts, 4
          FROM raw WHERE ns = 'Consensus.LeiosKernel.BlockTxsAcquired'
    UNION ALL
          SELECT c.eb, a.process, a.ts, 5
          FROM certifying c JOIN v_block_adoptions a ON a.hash = c.rb),
  agg AS MATERIALIZED (
    -- One row per (EB, node), the forger included for its own announcement
    -- columns, with the first time and the count of each stage.
    SELECT hash, process
         , min(ts) FILTER (WHERE k = 1) AS ann_at, count_if(k = 1) AS ann_n
         , min(ts) FILTER (WHERE k = 2) AS del_at, count_if(k = 2) AS del_n
         , min(ts) FILTER (WHERE k = 3) AS wri_at, count_if(k = 3) AS wri_n
         , min(ts) FILTER (WHERE k = 4) AS clo_at, count_if(k = 4) AS clo_n
         , min(ts) FILTER (WHERE k = 5) AS ado_at
    FROM acts
    GROUP BY hash, process)
SELECT f.ts, f.process, f.hash, f.slot_no, f.slot_start, f.bytes
     , g.ann_at AS forger_announced_at, g.ann_n AS forger_announcements
     , p.process AS peer_process
     , p.ann_at AS peer_announced_at,        p.ann_n AS peer_announcements
     , p.del_at AS peer_delivered_at,        p.del_n AS peer_deliveries
     , p.wri_at AS peer_written_at,          p.wri_n AS peer_writes
     , p.clo_at AS peer_closure_obtained_at, p.clo_n AS peer_closures_obtained
     , p.ado_at AS peer_adopted_at
FROM v_eb_forged f
JOIN      agg p ON p.hash = f.hash AND p.process <> f.process
LEFT JOIN agg g ON g.hash = f.hash AND g.process =  f.process
UNION ALL
-- EBs no other node touched: one row, peer columns NULL.
SELECT f.ts, f.process, f.hash, f.slot_no, f.slot_start, f.bytes
     , g.ann_at, g.ann_n
     , NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
FROM v_eb_forged f
LEFT JOIN agg g ON g.hash = f.hash AND g.process = f.process
WHERE NOT EXISTS (SELECT 1 FROM agg p WHERE p.hash = f.hash AND p.process <> f.process);

COMMENT ON VIEW v_eb_timing IS 'tier 2, Leios (experimental): an endorser block''s whole life per peer (announced, delivered, written, closure obtained, certifying block adopted: first time and count), one row per (forging, peer) (previously: block_endorser_timing)';
