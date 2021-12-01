{@delete_existing} ? {
if object_id('@cohort_database_schema.@outcome_cohort_table', 'U') is not null
	drop table @cohort_database_schema.@outcome_cohort_table;
}

--HINT DISTRIBUTE_ON_KEY(subject_id)
if object_id('@cohort_database_schema.@outcome_cohort_table', 'U') is null
    create table @cohort_database_schema.@outcome_cohort_table
    (
        cohort_definition_id bigint
        , subject_id bigint
        ,	cohort_start_date date
        ,	cohort_end_date date
    );


IF OBJECT_ID('@cohort_database_schema.computed_o_cohorts', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.computed_o_cohorts;

--HINT DISTRIBUTE_ON_KEY(cohort_definition_id)
create table @cohort_database_schema.computed_o_cohorts AS
SELECT DISTINCT cohort_definition_id
FROM @cohort_database_schema.@outcome_cohort_table;




