IF OBJECT_ID('tempdb..#computed_o_cohorts', 'U') IS NOT NULL
	DROP TABLE #computed_o_cohorts;

--HINT DISTRIBUTE_ON_KEY(cohort_definition_id)
create table #computed_o_cohorts AS
SELECT DISTINCT cohort_definition_id
FROM @cohort_database_schema.@outcome_cohort_table;