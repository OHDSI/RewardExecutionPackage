IF OBJECT_ID('@cohort_database_schema.computed_o_cohorts', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.computed_o_cohorts;

--HINT DISTRIBUTE_ON_KEY(cohort_definition_id)
create table @cohort_database_schema.computed_o_cohorts AS
SELECT DISTINCT ct.cohort_definition_id
FROM @cohort_database_schema.@cohort_table ct
INNER JOIN @reference_schema.@outcome_cohort ot ON ct.cohort_definition_id = ot.cohort_definition_id;




