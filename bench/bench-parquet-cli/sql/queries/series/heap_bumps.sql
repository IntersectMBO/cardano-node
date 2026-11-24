-- "heap_bumps": the slots in which a node's heap GREW, with the new size. A
-- series (x = slot, y = heap_gib, one series per process), for a lines plot of
-- the heap's growth steps.
--
-- Built on v_heap_per_slot: a row whenever a node's heap is larger than at its
-- previous sample. Shrinking and unchanged samples are left out.
--
-- Fields:
-- --------
-- * process    The node (series key).
-- * slot       The slot of the sample (x).
-- * heap_gib   The heap after the bump, GiB (y).
SELECT process, slot, heap_bytes / 2^30 AS heap_gib
FROM (SELECT process, slot, heap_bytes,
             lag(heap_bytes) OVER (PARTITION BY process ORDER BY slot) AS prev
      FROM v_heap_per_slot)
WHERE heap_bytes > prev
ORDER BY process, slot;
