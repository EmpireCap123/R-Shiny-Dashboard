# R-Shiny-Dashboard
WITH ctePolicies AS (
  SELECT
    dp.dim_policy_key,
    dp.policy_num,
    dps.policy_status_code,
    dps.policy_reason_code,
    dp.submission_num,
    dp.bound_dttm,
    dp.dim_eff_dttm,
    dps.dim_eff_dttm,
    ROW_NUMBER() OVER(
      PARTITION BY dp.policy_num
      ORDER BY
        dp.dim_eff_dttm,
        dp.dim_policy_key
    ) AS sale_event,
    ROW_NUMBER() OVER(
      PARTITION BY dp.policy_num
      ORDER BY
        dp.dim_eff_dttm DESC,
        dps.dim_eff_dttm DESC,
        dp.dim_policy_key DESC
    ) AS most_recent_submission
  FROM dim_policy dp
  JOIN dim_policy_status dps
    ON dps.hub_policy_mod_key = dp.hub_policy_mod_key
   AND dps.dim_eff_dttm = dp.dim_eff_dttm
  WHERE dp.line_of_busn_code IN ('BP7BusinessOwners', 'CA7CommAuto')
    AND dp.policy_num != dp.submission_num
),

cteTerritory AS (
  SELECT
    dp.dim_policy_key,
    dp.policy_num,
    dp.submission_num,
    dpl.territory_code,
    dpl.loc_zip_code AS primary_risk_zip_code,
    ROW_NUMBER() OVER(
      PARTITION BY dp.submission_num
      ORDER BY
        dpl.dim_eff_dttm,
        dp.dim_policy_key
    )                AS record_order
  FROM dim_policy dp
  JOIN bridge_policy_loc_addr bpla
    ON bpla.dim_policy_key = dp.dim_policy_key
  JOIN dim_policy_loc dpl
    ON dpl.dim_policy_loc_key = bpla.dim_policy_loc_key
  WHERE dp.line_of_busn_code IN ('BP7BusinessOwners', 'CA7CommAuto')
    AND dpl.loc_num = 1
    AND dpl.delete_flag = 'N'
),

cteSuperAgency AS (
  SELECT
    agency_code,
    agency_name,
    ROW_NUMBER() OVER(
      PARTITION BY agency_code
      ORDER BY dim_eff_dttm DESC
    ) AS currentAgencyRecord
  FROM dim_agency
),

ctePremium AS (
  SELECT
    dim_policy_key,
    'Sale'               AS policy_status,
    'CA7CommAuto'        AS line_of_business,
    SUM(wrtn_prem_amt)   AS wrtn_prem_amt
  FROM fact_ca_prem_trans fcpt
  GROUP BY dim_policy_key
  UNION ALL
  SELECT
    dim_policy_key,
    'Sale'               AS policy_status,
    'BP7BusinessOwners'  AS line_of_business,
    SUM(wrtn_prem_amt)   AS wrtn_prem_amt
  FROM fact_bop_prem_trans
  GROUP BY dim_policy_key
  UNION ALL
  SELECT
    -- This is the same as dim_policy_key.
    dim_quote_policy_key AS dim_policy_key,
    'Quote'              AS policy_status,
    'CA7CommAuto'        AS line_of_business,
    SUM(wrtn_prem_amt)   AS wrtn_prem_amt
  FROM fact_ca_quote_prem_trans fcqpt
  GROUP BY dim_quote_policy_key
  UNION ALL
  SELECT
    -- This is the same as dim_policy_key.
    dim_quote_policy_key AS dim_policy_key,
    'Quote'              AS policy_status,
    'BP7BusinessOwners'  AS line_of_business,
    SUM(wrtn_prem_amt)   AS wrtn_prem_amt
  FROM fact_bop_quote_prem_trans fbqpt
  GROUP BY dim_quote_policy_key
),

ctePolicyAgencyBridge AS (
  SELECT
    bpa.dim_policy_key,
    bpa.dim_agency_key,
    bpa.etl_load_cycle_id,
    ROW_NUMBER() OVER(
      PARTITION BY bpa.dim_policy_key
      ORDER BY bpa.etl_load_cycle_id DESC
    ) AS active_bridge
  FROM bridge_policy_agency bpa
),

ctePolicyLineBridge AS (
  SELECT
    bpl.dim_policy_key,
    bpl.dim_policy_line_key,
    bpl.etl_load_cycle_id,
    ROW_NUMBER() OVER(
      PARTITION BY bpl.dim_policy_key
      ORDER BY bpl.etl_load_cycle_id DESC
    ) AS active_bridge
  FROM bridge_policy_line bpl
),


ctePolicyCustAcctBridge AS (
  SELECT
    bpca.dim_policy_key,
    bpca.dim_cust_acct_key,
    bpca.etl_load_cycle_id,
    ROW_NUMBER() OVER(
      PARTITION BY bpca.dim_policy_key
      ORDER BY bpca.etl_load_cycle_id DESC
    ) AS active_bridge
  FROM bridge_policy_cust_acct bpca
),

cteQuotes AS (
  SELECT
    dp.dim_policy_key,
    dp.submission_num,
    dp.submission_dttm,
    dps.hub_policy_mod_key,
    dps.policy_status_code,
    dps.policy_reason_code,
    dp.primry_state_key,
    dp.line_of_busn_code,
    dp.affinity_group_name,
    dp.policy_eff_date,
    dp.policy_exp_date,
    dp.uwtr_name,
    dp.rate_as_of_date,
    dp.quoted_cnt_num,
    dp.participant_name,
    dp.first_quoted_date,
    dp.dim_eff_dttm,
    dps.dim_eff_dttm,
    ROW_NUMBER() OVER(
      PARTITION BY dp.submission_num
      ORDER BY
        dp.dim_eff_dttm DESC,
        dps.dim_eff_dttm DESC,
        dp.dim_policy_key DESC
    ) AS record_order
  FROM dim_policy dp
  JOIN dim_policy_status dps
    ON dps.hub_policy_mod_key = dp.hub_policy_mod_key
   --AND dps.dim_eff_dttm = dp.dim_eff_dttm
  WHERE dp.line_of_busn_code IN ('BP7BusinessOwners', 'CA7CommAuto')
    AND dps.policy_status_code != 'CANCELLED'
    AND dp.submission_num = dp.policy_num
)

SELECT
  cq.submission_num            AS submission_number,
  CASE
    WHEN cq.policy_status_code = 'ACTIVE'
    THEN 'Quoted'
    ELSE cq.policy_status_code
  END                          AS submission_status,
  cq.submission_dttm :: DATE   AS submission_date,
  COALESCE(
    cq.first_quoted_date :: DATE :: TEXT,
    'Draft Only'
  )                            AS first_quoted_date,
  cq.quoted_cnt_num            AS number_of_quote_iterations,
  COALESCE(
    cq.rate_as_of_date :: DATE :: TEXT,
    'Draft Only'
  )                            AS rate_as_of_date,
  cq.line_of_busn_code         AS line_of_business,
  cq.policy_eff_date :: DATE   AS quoted_policy_effective_date,
  cq.policy_exp_date :: DATE   AS quoted_policy_expiration_date,
  --cqp.wrtn_prem_amt AS quoted_premium_amount,
  --csp.wrtn_prem_amt AS sold_premium_amount,
  arcsPS.ref_code_desc         AS primary_state,
  arcsREG.ref_code_desc        AS region,
  dpl.lib_centile              AS liability_cetile,
  dpl.prop_centile             AS property_centile,
  dca.cust_acct_name,
  dca.cust_acct_num,
  cq.uwtr_name                 AS underwriter_name,
  cq.participant_name,
  da.agency_name,
  da.agency_code,
  da.super_primry_agency_code  AS super_primary_agency_code,
  csa.agency_name              AS super_primary_agency_name,
  arcsAS.ref_code_desc         AS agency_state,
  CASE
    WHEN arcsPD.ref_code_desc = 'Default Value :NULLVALUE'
    THEN 'No Program Description'
    ELSE arcsPD.ref_code_desc
  END                          AS program_description,
  cq.affinity_group_name,
  dca.naics_description,
  dca.naics_code,
  iencSec.naics_description    AS naics_sector_description,
  iencSubSec.naics_description AS naics_sub_sector_description,
  ct.territory_code,
  ct.primary_risk_zip_code,
  CASE
    WHEN cps.policy_num IS NULL
    THEN 'N'
    ELSE 'Y'
  END                          AS bound_policy_indicator,
  COALESCE(
    cps.bound_dttm :: DATE :: TEXT,
    'Quote Only'
  )                            AS bound_date,
  COALESCE(
    cps.policy_num,
    'Quote Only'
  )                            AS policy_number,
  COALESCE(
    INITCAP(cprs.policy_status_code),
    'Quote Only'
  )                            AS current_policy_status,
  COALESCE(
    INITCAP(cprs.policy_reason_code),
    'Quote Only'
  )                            AS reason_for_current_status,
  COALESCE(
    cprs.submission_num,
    'Quote Only'
  )                            AS submission_that_put_it_in_that_status
FROM cteQuotes cq
LEFT JOIN ctePolicies cps
  ON cps.submission_num = cq.submission_num
 AND cps.sale_event = 1
LEFT JOIN ctePolicies cprs
  ON cprs.policy_num = cps.policy_num
 AND cprs.most_recent_submission = 1
JOIN abc_ref_code_set arcsPS
  ON arcsPS.code_val_master_key = cq.primry_state_key
JOIN ctePolicyAgencyBridge bpa
  ON bpa.dim_policy_key = cq.dim_policy_key
 AND active_bridge = 1
JOIN dim_agency da
  ON da.dim_agency_key = bpa.dim_agency_key
LEFT JOIN cteSuperAgency csa
  ON csa.agency_code = da.super_primry_agency_code
 AND csa.currentAgencyRecord = 1
JOIN abc_ref_code_set arcsAS
  ON arcsAS.code_val_master_key = da.agency_state_key
JOIN ctePolicyLineBridge bpl
  ON bpl.dim_policy_key = cq.dim_policy_key
 AND bpl.active_bridge = 1
JOIN dim_policy_line dpl
  ON dpl.dim_policy_line_key = bpl.dim_policy_line_key
JOIN abc_ref_code_set arcsPD
  ON arcsPD.code_val_master_key = dpl.program_key
JOIN ctePolicyCustAcctBridge bpca
  ON bpca.dim_policy_key = cq.dim_policy_key
 AND bpca.active_bridge = 1
JOIN dim_cust_acct dca
  ON dca.dim_cust_acct_key = bpca.dim_cust_acct_key
-- LEFT JOIN ctePremium cqp
--   ON cqp.dim_policy_key = cq.dim_policy_key
-- LEFT JOIN ctePremium csp
--   ON csp.dim_policy_key = cps.dim_policy_key
JOIN abc_ref_code_set arcsREG
  ON arcsREG.code_val_master_key = da.agency_uwtr_region_key
LEFT JOIN cteTerritory ct
  ON ct.submission_num = cq.submission_num
 AND ct.record_order = 1
LEFT JOIN int_extrnl_naics_codes iencSec
  ON iencSec.naics_code = SUBSTRING(dca.naics_code :: TEXT, 1, 2)
 AND LENGTH(iencSec.naics_code :: TEXT) = 2
LEFT JOIN int_extrnl_naics_codes iencSubSec
  ON iencSubSec.naics_code = SUBSTRING(dca.naics_code :: TEXT, 1, 4)
 AND LENGTH(iencSubSec.naics_code :: TEXT) = 4
WHERE cq.record_order = 1
ORDER BY submission_number;
