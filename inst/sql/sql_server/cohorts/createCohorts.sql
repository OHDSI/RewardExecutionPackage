IF OBJECT_ID('tempdb..#computed_cohorts', 'U') IS NOT NULL
	DROP TABLE #computed_cohorts;

--HINT DISTRIBUTE_ON_KEY(cohort_definition_id)
CREATE TABLE #computed_cohorts AS
SELECT DISTINCT cohort_definition_id
FROM @cohort_database_schema.@cohort_table;

-- First, create ingredient level cohorts
--HINT DISTRIBUTE_ON_KEY(person_id)
create table #ingredient_eras as
select
  cr.cohort_definition_id
  , de1.concept_name
  , de1.person_id
  , de1.cohort_start_date
  , de1.cohort_end_date
from
  (
    select
        de0.person_id
        , de0.drug_concept_id
        , ings.concept_name
        , de0.drug_era_start_date as cohort_start_date
        , de0.drug_era_end_date as cohort_end_date
        , row_number() over (partition by de0.person_id, ings.concept_id order by de0.drug_era_start_date asc) row_num
  FROM @drug_era_schema.drug_era de0
  inner join
      (
        SELECT concept_id, concept_name
        from @vocab_schema.concept
	      where concept_class_id = 'Ingredient' AND vocabulary_id = 'RxNorm' AND standard_concept = 'S'
	      -- the only thing this doesn't include from the drug era table are some vaccines which have vocabulary_id = 'CVx' and have era end dates in 2099
      ) ings
      on de0.drug_concept_id = ings.concept_id
      -- where de0.drug_concept_id in (42904205, 40226579, 1337068) -- REMOVE FOR FULL RUN
  ) de1
inner join @reference_schema.@cohort_definition cr ON (cr.drug_conceptset_id = de1.drug_concept_id  AND cr.atc_flg = 0)
left join #computed_cohorts cc ON cc.cohort_definition_id = cr.cohort_definition_id
inner join @cdm_database_schema.observation_period op1
  on de1.person_id = op1.person_id
  and de1.cohort_start_date >= dateadd(dd,365,op1.observation_period_start_date)
  and de1.cohort_start_date <= op1.observation_period_end_date
  and de1.row_num = 1

WHERE cc.cohort_definition_id IS NULL
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
from #ingredient_eras
;


-- Second, create ATC 4th level cohorts
--HINT DISTRIBUTE_ON_KEY(person_id)
create table #ATC_eras as
select
  cr.cohort_definition_id
  , de1.concept_name
  , de1.person_id
  , de1.cohort_start_date
  , de1.cohort_end_date
from
  (
    select
        de0.person_id
        , atc_rxnorm.atc_concept_id  as drug_concept_id
        , atc_rxnorm.atc_concept_name as concept_name
        , de0.drug_era_start_date as cohort_start_date
        , de0.drug_era_end_date as cohort_end_date
        , row_number() over (partition by de0.person_id, atc_rxnorm.atc_concept_id order by de0.drug_era_start_date asc) row_num
  FROM @drug_era_schema.drug_era de0
  inner join
      (
        SELECT c1.concept_id as descendant_concept_id, c1.concept_name as descendant_concept_name, c2.concept_id as atc_concept_id, c2.concept_name as atc_concept_name, c2.vocabulary_id as atc_id
        from @vocab_schema.concept c1
      	inner join @vocab_schema.concept_ancestor ca1 on c1.concept_id = ca1.descendant_concept_id
      	inner join @vocab_schema.concept c2 on ca1.ancestor_concept_id = c2.concept_id
      	where c1.vocabulary_id IN ('RxNorm') AND c2.concept_class_id = 'ATC 4th'
      ) atc_rxnorm
      on de0.drug_concept_id = atc_rxnorm.descendant_concept_id
    --  where de0.drug_concept_id in (42904205, 40226579, 1337068) -- REMOVE FOR FULL RUN
  ) de1
inner join @reference_schema.@cohort_definition cr ON (cr.drug_conceptset_id = de1.drug_concept_id  AND cr.atc_flg = 1)
left join #computed_cohorts cc ON cc.cohort_definition_id = cr.cohort_definition_id
inner join @cdm_database_schema.observation_period op1
  on de1.person_id = op1.person_id
  and de1.cohort_start_date >= dateadd(dd,365,op1.observation_period_start_date)
  and de1.cohort_start_date <= op1.observation_period_end_date
  and de1.row_num = 1
WHERE cc.cohort_definition_id IS NULL
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
from #atc_eras
;

truncate table #ingredient_eras;
drop table #ingredient_eras;

truncate table #atc_eras;
drop table #atc_eras;

truncate table #computed_cohorts;
drop table #computed_cohorts;
