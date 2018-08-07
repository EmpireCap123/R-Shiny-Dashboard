# R-Shiny-Dashboard
WITH ctePolicyBinds AS (
  SELECT
    PolicyID,
    MAX(PolicyNumber) AS policyExists
  FROM pc_policyperiod
  GROUP BY PolicyID
),

cteJobUserRoles AS (
  SELECT
    jura.JobID,
    jura.AssignedUserID,
    ur.NAME AS userRole
  FROM pc_jobuserroleassign jura
  JOIN pctl_userrole ur
    ON ur.ID = jura.Role
  WHERE jura.Retired = 0
    AND ur.RETIRED = 0
),

cteVehicles AS (
  SELECT
    capp.BranchID AS PolicyPeriodID,
    capp.ExpirationDate,
    'Private Passenger' AS vehicleType
  FROM pcx_ca7privatepassenger capp
  UNION ALL
  SELECT
    cat.BranchID AS PolicyPeriodID,
    cat.ExpirationDate,
    'Truck' AS vehicleType
  FROM pcx_ca7truck cat
  UNION ALL
  SELECT
    capt.BranchID AS PolicyPeriodID,
    capt.ExpirationDate,
    'Public Transport' AS vehicleType
  FROM pcx_ca7publictransport capt
  UNION ALL
  SELECT
    caspt.BranchID AS PolicyPeriodID,
    caspt.ExpirationDate,
    'Special Type' AS vehicleType
  FROM pcx_ca7specialtype caspt
),

cteVehicleCount AS (
  SELECT
    PolicyPeriodID,
    COUNT(*) AS vehicleCount
  FROM cteVehicles
  WHERE ExpirationDate IS NULL
  GROUP BY PolicyPeriodID
)

SELECT
  j.JobNumber                               AS submissionNumber,
  ptlj.NAME                                 AS submissionType,
  qt.DESCRIPTION                            AS quoteType,
  pps.NAME                                  AS submissionStatus,
  CASE
    WHEN pp.PolicyNumber IS NULL THEN 'N'
    ELSE 'Y'
  END                                       AS boundPolicyIndicator,
  CAST(j.SubmissionDate AS DATE)            AS submissionDate,
  DATENAME(weekday, j.SubmissionDate)       AS submissionDayOfWeek,
  CAST(pp.dateFirstQuoted AS DATE)          AS firstQuotedDate,
  CAST(pp.RateAsOfDate AS DATE)             AS rateAsOfDate,
  p.ProductCode                             AS lineOfBusiness,
  CAST(pp.PeriodStart AS DATE)              AS quotedPolicyEffectiveDate,
  CAST(pp.PeriodEnd AS DATE)                AS quotedPolicyExpirationDate,
  pp.TotalPremiumRPT                        AS premiumAmount,
  jur.DESCRIPTION                           AS baseState,
  tc.Code                                   AS primaryLocationTerritoryCode,
  propcen.Centile                           AS propertyCentile,
  libcen.Centile                            AS liabilityCentile,
  COALESCE(camt.NAME, 'Not in CA7 Program') AS programDescription,
  pag.Name                                  AS affinityGroup,
  a.AccountNumber                           AS accountNumber,
  uwc.FirstName + ' ' + uwc.LastName        AS underwriterName,
  crc.FirstName + ' ' + crc.LastName        AS createUserName,
  agc.FirstName + ' ' + agc.LastName        AS agentName,
  pc.Description                            AS agencyName,
  ags.NAME                                  AS agencyState,
  pc.Code                                   AS agencyCode,
  ic.Code                                   AS naicsCode,
  ic.Classification                         AS naicsDescription,
  cv.vehicleCount,
  pp.PolicyNumber                           AS policyNumber,
  pp.PrevInstallmentPlan                    AS billingPlan,
  CASE
    WHEN pp.PolicyNumber IS NOT NULL
     AND ptlj.NAME = 'Submission'
     AND pps.NAME = 'Bound'
    THEN CAST(j.UpdateTime AS DATE)
    ELSE NULL
  END                                       AS boundDate
FROM pc_job j
JOIN cteJobUserRoles uwcjur
  ON uwcjur.JobID = j.ID
 AND uwcjur.userRole = 'Underwriter'
JOIN pc_user uwu
  ON uwu.ID = uwcjur.AssignedUserID
JOIN pc_contact uwc
  ON uwc.ID = uwu.ContactID
JOIN cteJobUserRoles crcjur
  ON crcjur.JobID = j.ID
 AND crcjur.userRole = 'Creator'
JOIN pc_user cru
  ON cru.ID = crcjur.AssignedUserID
JOIN pc_contact crc
  ON crc.ID = cru.ContactID
JOIN cteJobUserRoles agcjur
  ON agcjur.JobID = j.ID
 AND agcjur.userRole = 'Producer' -- aka Agent
JOIN pc_user agu
  ON agu.ID = agcjur.AssignedUserID
JOIN pc_contact agc
  ON agc.ID = agu.ContactID
JOIN pc_policyperiod pp
  ON pp.JobID = j.ID
JOIN pc_policy p
  ON p.ID = pp.PolicyID
JOIN pc_policyterm pt
  ON pt.ID = pp.PolicyTermID
JOIN pc_policyline pl
  ON pl.BranchID = pp.ID
 AND pl.ExpirationDate IS NULL
LEFT JOIN pctl_ca7modeltype_ext camt
  ON camt.ID = pl.CA7Model_Ext
JOIN pc_account a
  ON p.AccountID = a.ID
JOIN pc_producercode pc
  ON pc.ID = pp.ProducerCodeOfRecordID
JOIN pc_address aga
  ON aga.ID = pc.AddressID
LEFT JOIN pc_affinitygroup pag
  ON pag.ID = pt.AffinityGroupID
JOIN pctl_state ags
  ON ags.ID = aga.State
JOIN pc_industrycode ic
  ON ic.ID = pp.IndustryCodeID
JOIN pctl_industrycodetype ict
  ON ict.ID = ic.Domain
 AND ict.NAME = 'NAICS'
JOIN pctl_policyperiodstatus pps
  ON pps.ID = pp.Status
JOIN pctl_job ptlj
  ON ptlj.ID = j.Subtype
JOIN pctl_quotetype qt
  ON qt.ID = j.QuoteType
JOIN pctl_jurisdiction jur
  ON jur.ID = pp.BaseState
JOIN pc_policylocation ploc
  ON ploc.BranchID = pp.ID
 AND ploc.LocationNum = 1
 AND ploc.ExpirationDate IS NULL
JOIN pc_territorycode tc
  ON tc.BranchID = ploc.BranchID
 AND tc.PolicyLocation = ploc.FixedID
 AND tc.ExpirationDate IS NULL
LEFT JOIN cteVehicleCount cv
  ON cv.PolicyPeriodID = pp.ID
LEFT JOIN ctePolicyBinds cpb
  ON cpb.PolicyID = pp.PolicyID
LEFT JOIN pcx_esescoreprop_ext propcen
  ON propcen.BranchID = pp.ID
LEFT JOIN pcx_esescoreliab_ext libcen
  ON libcen.BranchID = pp.ID
WHERE p.ProductCode IN ('BP7BusinessOwners', 'CA7CommAuto')
  AND (
    (pp.PolicyNumber IS NOT NULL AND ptlj.NAME = 'Submission' AND pps.NAME = 'Bound')
    OR (pp.PolicyNumber IS NULL AND pp.BranchNumber = 1 AND cpb.policyExists IS NULL)
  )
ORDER BY submissionNumber;
