IF OBJECT_ID('tempdb..#computed_cohorts', 'U') IS NOT NULL
	DROP TABLE #computed_cohorts;

IF OBJECT_ID('tempdb..#custom_eras', 'U') IS NOT NULL
	DROP TABLE #custom_eras;

--HINT DISTRIBUTE_ON_KEY(cohort_definition_id)
CREATE TABLE #computed_cohorts AS
    SELECT DISTINCT ct.cohort_definition_id
    FROM @cohort_database_schema.@cohort_table ct
    INNER JOIN @reference_schema.@custom_exposure ce ON ct.cohort_definition_id = ce.cohort_definition_id;

--HINT DISTRIBUTE_ON_KEY(person_id)
create table #custom_eras as
select
  de1.cohort_definition_id
  , de1.concept_name
  , de1.person_id
  , de1.cohort_start_date
  , de1.cohort_end_date
from
  (
    select
        de0.person_id
        , custom_exposure_cohort.custom_exposure_id  as cohort_definition_id
        , custom_exposure_cohort.custom_exposure_name as concept_name
        , de0.drug_era_start_date as cohort_start_date
        , de0.drug_era_end_date as cohort_end_date
        , row_number() over (partition by de0.person_id, custom_exposure_cohort.custom_exposure_concept_id order by de0.drug_era_start_date asc) row_num
  FROM @drug_era_schema.drug_era de0
  inner join
      (
        SELECT
            ce.cohort_definition_id as custom_exposure_id,
            cd.short_name as custom_exposure_name,
            cec.concept_id as custom_exposure_concept_id
        from @reference_schema.@custom_exposure ce
        INNER JOIN @reference_schema.@cohort_definition cd ON cd.cohort_definition_id = ce.cohort_definition_id
        INNER JOIN @reference_schema.@custom_exposure_concept cec ON cec.cohort_definition_id = ce.cohort_definition_id
        left join #computed_cohorts cc ON cc.cohort_definition_id = ce.cohort_definition_id
        WHERE cec.include_descendants = 0
        AND  cc.cohort_definition_id IS NULL -- only compute cohorts with no results
        {@only_add_subset} ? {AND ce.cohort_definition_id IN (@custom_exposure_subset) }
        UNION

        SELECT
            ce.cohort_definition_id as custom_exposure_id,
            cd.short_name as custom_exposure_name,
            ca1.descendant_concept_id as custom_exposure_concept_id
        from @reference_schema.@custom_exposure ce
        INNER JOIN @reference_schema.@cohort_definition cd ON cd.cohort_definition_id = ce.cohort_definition_id
        INNER JOIN @reference_schema.@custom_exposure_concept cec ON cec.cohort_definition_id = ce.cohort_definition_id
        INNER JOIN @vocab_schema.concept_ancestor ca1 on cec.concept_id = ca1.ancestor_concept_id
        left join #computed_cohorts cc ON cc.cohort_definition_id = ce.cohort_definition_id
        WHERE cec.include_descendants = 1
        AND  cc.cohort_definition_id IS NULL -- only compute cohorts with no results
        {@only_add_subset} ? {AND ce.cohort_definition_id IN (@custom_exposure_subset) }

      ) custom_exposure_cohort
      on de0.drug_concept_id = custom_exposure_cohort.custom_exposure_concept_id
  ) de1
inner join @cdm_database_schema.observation_period op1
  on de1.person_id = op1.person_id
  and de1.cohort_start_date >= dateadd(dd,365,op1.observation_period_start_date)
  and de1.cohort_start_date <= op1.observation_period_end_date
  and de1.row_num = 1
;

insert into @cohort_database_schema.@cohort_table
(
  cohort_definition_id
  , subject_id
  , cohort_start_date
  , cohort_end_date
)
select
  cohort_definition_id
  , person_id
  , cohort_start_date
  , cohort_end_date
from #custom_eras
;

truncate table #custom_eras;
drop table #custom_eras;
truncate table #computed_cohorts;
drop table #computed_cohorts;