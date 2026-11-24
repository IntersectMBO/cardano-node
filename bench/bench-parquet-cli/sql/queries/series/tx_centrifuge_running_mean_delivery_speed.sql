-- "tx_centrifuge_running_mean_delivery_speed": the total raw CBOR bytes sent to
-- each target SO FAR, divided by the time elapsed since that target's first
-- send. A running mean, not a rate. A series: x = minute, key = target,
-- y = bytes_per_s. Generator family: experimental.
--
-- NO WINDOW, NO WEIGHTING, NO SMOOTHING. Every point is exact arithmetic on two
-- exact numbers: the cumulative bytes through the end of the minute, and the
-- span from that target's first delivery to that same instant, so two runs can
-- be compared point for point.
-- IT IS NOT tx_centrifuge_delivered_bytes DIFFERENCED. That would be a
-- per-minute rate, which swings with every idle minute; this is the average
-- achieved since the target's own start, so it converges rather than oscillates.
-- THE DENOMINATOR STARTS AT THAT TARGET'S FIRST SEND, not at the anchor: a target
-- that began late is not charged for the minutes before it existed. The minute
-- is still anchored, so minute numbers align across the tx_centrifuge_* series.
-- ONE ROW PER (target, minute) IN WHICH THAT TARGET WAS SENT ANYTHING.
--
-- Fields:
-- --------
-- * target       The node the transactions were sent to (series key).
-- * minute       Whole minutes since the generator's first TxSubmission event (x).
-- * bytes_per_s  Cumulative bytes to that target divided by the seconds from its
--                first send to the end of this minute (y).
WITH
  t0 AS (SELECT min(ts) AS t0 FROM raw_generator
         WHERE ns IN ('TxCentrifuge.TxSubmission.RequestTxIds', 'TxCentrifuge.TxSubmission.ReplyTxIds',
                      'TxCentrifuge.TxSubmission.ReplyTxs')),
  started AS (SELECT target, min(ts) AS first_ts FROM v_gen_tx_sent GROUP BY target),
  per_minute AS (
    SELECT target, (epoch_ns(ts) - epoch_ns((SELECT t0 FROM t0))) // 60000000000 AS minute, sum(bytes) AS bytes
    FROM v_gen_tx_sent GROUP BY ALL),
  cumulative AS (
    SELECT target, minute,
           sum(bytes) OVER (PARTITION BY target ORDER BY minute) AS bytes_total
    FROM per_minute)
SELECT c.target, c.minute,
       round(c.bytes_total
             / (((c.minute + 1) * 60000000000.0
                 - (epoch_ns(s.first_ts) - epoch_ns((SELECT t0 FROM t0)))) / 1e9), 2) AS bytes_per_s
FROM cumulative c JOIN started s USING (target)
ORDER BY minute, target;
