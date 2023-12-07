{DEFAULT @schema = 'reward'}
{DEFAULT @cohort_definition = 'cohort_definition'}
{DEFAULT @exposure_cohort = 'exposure_cohort'}
{DEFAULT @outcome_cohort = 'outcome_cohort'}
{DEFAULT @cohort_group_definition = 'cohort_group_definition'}
{DEFAULT @cohort_group = 'cohort_group'}
{DEFAULT @concept_set_definition = 'concept_set_definition'}
{DEFAULT @atlas_cohort_reference = 'atlas_cohort_reference'}
{DEFAULT @cohort_concept_set = 'cohort_concept_set'}
{DEFAULT @analysis_setting = 'analysis_setting'}
{DEFAULT @cohort_subset_definition = 'cohort_subset_definition'}
{DEFAULT @cohort_subset_target = 'cohort_subset_target'}
{DEFAULT @include_constraints = FALSE}
{DEFAULT @reference_version = 'reference_version'}
{DEFAULT @copy_temp_table = FALSE}
{DEFAULT @store_atlas_refs = FALSE}

INSERT INTO @schema.@reference_version
SELECT
    reward_version_number
FROM #t_reference_version;
TRUNCATE TABLE #t_reference_version;
DROP TABLE #t_reference_version;

INSERT INTO @schema.@cohort_definition
SELECT
    CAST(cohort_definition_id AS bigint),
    CAST(cohort_definition_name AS varchar(max)),
    CAST(short_name AS varchar(max)),
    CAST(concept_set_id AS bigint),
    CAST(is_subset AS int),
    CAST(subset_parent AS bigint)
FROM #t_cohort_definition;
TRUNCATE TABLE #t_cohort_definition;
DROP TABLE #t_cohort_definition;

INSERT INTO @schema.@exposure_cohort
SELECT
    CAST(cohort_definition_id AS bigint),
    CAST(referent_concept_id AS bigint),
    CAST(atc_flg AS int)
FROM #t_exposure_cohort;
TRUNCATE TABLE #t_exposure_cohort;
DROP TABLE #t_exposure_cohort;

INSERT INTO @schema.@outcome_cohort
SELECT
    CAST(cohort_definition_id AS bigint),
    CAST(referent_concept_id AS bigint),
    CAST(outcome_type AS int)
FROM #t_outcome_cohort;
TRUNCATE TABLE #t_outcome_cohort;
DROP TABLE #t_outcome_cohort;

INSERT INTO @schema.@analysis_setting
SELECT
    CAST(analysis_id AS INT),
	CAST(type_id AS VARCHAR(5)),
	CAST(analysis_name AS varchar(255)),
	CAST(description AS TEXT),
	CAST(options AS TEXT)
FROM #t_analysis_setting;
TRUNCATE TABLE #t_analysis_setting;
DROP TABLE #t_analysis_setting;

INSERT INTO @schema.@concept_set_definition
SELECT
    CAST(cohort_definition_id AS BIGINT),
    CAST(concept_set_id AS BIGINT),
    CAST(concept_set_name AS varchar(max))
FROM #t_concept_set_definition;
TRUNCATE TABLE #t_concept_set_definition;
DROP TABLE #t_concept_set_definition;

INSERT INTO @schema.@cohort_concept_set
SELECT
    CAST(COHORT_DEFINITION_ID AS BIGINT),
	CAST(concept_set_id AS bigint),
	CAST(concept_id AS bigint),
	CAST(concept_name AS varchar(max)),
	CAST(is_excluded AS INT),
	CAST(include_descendants AS INT),
	CAST(include_mapped AS INT)
FROM #t_cohort_concept_set;
TRUNCATE TABLE #t_cohort_concept_set;
DROP TABLE #t_cohort_concept_set;

INSERT INTO @schema.@cohort_group_definition
SELECT
    CAST(cohort_group_definition_id AS INT),
    CAST(cohort_group_parent_id AS INT),
    CAST(group_name AS varchar(max))
FROM #t_cohort_group_definition;
TRUNCATE TABLE #t_cohort_group_definition;
DROP TABLE #t_cohort_group_definition;

INSERT INTO @schema.@cohort_group
SELECT
    CAST(cohort_definition_id AS BIGINT),
    CAST(cohort_group_definition_id AS INT),
    CAST(levels_of_separation AS INT)
FROM #t_cohort_group;
TRUNCATE TABLE #t_cohort_group;
DROP TABLE #t_cohort_group;

INSERT INTO @schema.@atlas_cohort_reference
SELECT
    CAST(cohort_definition_id AS BIGINT),
    CAST(ATLAS_ID AS BIGINT),
    CAST(atlas_url AS varchar(1000))
FROM #t_atlas_cohort_reference;
TRUNCATE TABLE #t_atlas_cohort_reference;
DROP TABLE #t_atlas_cohort_reference;


INSERT INTO @schema.@cohort_subset_definition
SELECT *
FROM #t_cohort_subset_definition;
TRUNCATE TABLE #t_cohort_subset_definition;
DROP TABLE #t_cohort_subset_definition;

INSERT INTO @schema.@cohort_subset_target
SELECT *
FROM #t_cohort_subset_target;
TRUNCATE TABLE #t_cohort_subset_target;
DROP TABLE #t_cohort_subset_target;