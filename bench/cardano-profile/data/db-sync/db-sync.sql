SELECT json_agg(epoch_param) FROM (SELECT * FROM epoch_param ORDER BY epoch_no) AS epoch_param;

SELECT json_agg(cost_model) FROM (SELECT * FROM cost_model ORDER BY id) AS cost_model;

SELECT *
FROM epoch_param
WHERE
  id IN (
    SELECT MIN(id) AS id
    FROM epoch_param
    GROUP BY
        coins_per_utxo_size
      , collateral_percent
      , committee_max_term_length
      , committee_min_size
      , cost_model_id
      , decentralisation
      , drep_activity
      , drep_deposit
      , dvt_committee_no_confidence
      , dvt_committee_normal
      , dvt_hard_fork_initiation
      , dvt_motion_no_confidence
      , dvt_p_p_economic_group
      , dvt_p_p_gov_group
      , dvt_p_p_network_group
      , dvt_p_p_technical_group
      , dvt_treasury_withdrawal
      , dvt_update_to_constitution
      , extra_entropy
      , gov_action_deposit
      , gov_action_lifetime
      , influence
      , key_deposit
      , max_bh_size
      , max_block_ex_mem
      , max_block_ex_steps
      , max_block_size
      , max_collateral_inputs
      , max_epoch
      , max_tx_ex_mem
      , max_tx_ex_steps
      , max_tx_size
      , max_val_size
      , min_fee_a
      , min_fee_b
      , min_fee_ref_script_cost_per_byte
      , min_pool_cost
      , min_utxo_value
      , monetary_expand_rate
      , optimal_pool_count
      , pool_deposit
      , price_mem
      , price_step
      , protocol_major
      , protocol_minor
      , pvt_committee_no_confidence
      , pvt_committee_normal
      , pvt_hard_fork_initiation
      , pvt_motion_no_confidence
      , pvtpp_security_group
      , treasury_growth_rate
  )
;
