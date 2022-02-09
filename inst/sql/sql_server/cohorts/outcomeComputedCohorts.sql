IF OBJECT_ID('tempdb..#computed_o_cohorts', 'U') IS NOT NULL
	DROP TABLE #computed_o_cohorts;

--HINT DISTRIBUTE_ON_KEY(cohort_definition_id)
create table #computed_o_cohorts AS
SELECT DISTINCT ct.cohort_definition_id
FROM @cohort_database_schema.@cohort_table ct
INNER JOIN @reference_schema.@outcome_cohort oc ON oc.cohort_definition_id = ct.cohort_definition_id
;