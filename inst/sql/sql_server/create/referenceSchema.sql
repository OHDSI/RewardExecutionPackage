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
{DEFAULT @include_constraints = FALSE}
{DEFAULT @reference_version = 'reference_version'}
{DEFAULT @copy_temp_table = FALSE}
{DEFAULT @store_atlas_refs = FALSE}

DROP TABLE IF EXISTS @schema.@reference_version;
create table @schema.@reference_version
(
   reward_version_number varchar(100)
);

{@copy_temp_table} ? {
SELECT
    reward_version_number
FROM #reference_version;
TRUNCATE TABLE #reference_version;
DROP TABLE #reference_version;
}

/* COHORT DEFINITION TABLE */
DROP TABLE IF EXISTS @schema.@cohort_definition {@include_constraints} ? {cascade};
create table @schema.@cohort_definition
(
	cohort_definition_id bigint {@include_constraints} ? {PRIMARY KEY},
	cohort_definition_name varchar(1000),
	short_name varchar(1000),
	concept_set_id bigint
);

{@copy_temp_table} ? {
INSERT INTO @schema.@cohort_definition
SELECT
    CAST(cohort_definition_id AS bigint),
    CAST(cohort_definition_name AS varchar(max)),
    CAST(short_name AS varchar(max)),
    CAST(concept_set_id AS bigint)
FROM #cohort_definition;
TRUNCATE TABLE #cohort_definition;
DROP TABLE #cohort_definition;
}

/* Exposure cohort table*/
DROP TABLE IF EXISTS @schema.@exposure_cohort {@include_constraints} ? {cascade};
create table @schema.@exposure_cohort (
    cohort_definition_id {@include_constraints} ? {bigint PRIMARY KEY} : {bigint},
	referent_concept_id bigint,
	atc_flg int
	{@include_constraints} ? {
    ,
    CONSTRAINT exp_cohort_def_fk
      FOREIGN KEY(COHORT_DEFINITION_ID)
	    REFERENCES @schema.@cohort_definition(COHORT_DEFINITION_ID)
	        ON DELETE CASCADE
    }
);

{@copy_temp_table} ? {
INSERT INTO @schema.@exposure_cohort
SELECT
    CAST(cohort_definition_id AS bigint),
    CAST(referent_concept_id AS bigint),
    CAST(atc_flg AS int)
FROM #exposure_cohort;
TRUNCATE TABLE #exposure_cohort;
DROP TABLE #exposure_cohort;
}

/* Outcome cohort table */
DROP TABLE IF EXISTS @schema.@outcome_cohort {@include_constraints} ? {cascade};
create table @schema.@outcome_cohort (
    cohort_definition_id {@include_constraints} ? {bigint PRIMARY KEY} : {bigint},
	referent_concept_id bigint,
	outcome_type int
	{@include_constraints} ? {
    ,
    CONSTRAINT out_cohort_def_fk
      FOREIGN KEY(cohort_definition_id)
	    REFERENCES @schema.@cohort_definition(cohort_definition_id)
	        ON DELETE CASCADE
    }
);

{@copy_temp_table} ? {
INSERT INTO @schema.@outcome_cohort
SELECT
    CAST(cohort_definition_id AS bigint),
    CAST(referent_concept_id AS bigint),
    CAST(outcome_type AS int)
FROM #outcome_cohort;
TRUNCATE TABLE #outcome_cohort;
DROP TABLE #outcome_cohort;
}

/* arbitrarily group cohorts together by group */
DROP TABLE IF EXISTS @schema.@cohort_group_definition {@include_constraints} ? {cascade};
create table @schema.@cohort_group_definition (
    cohort_group_definition_id {@include_constraints} ? {int PRIMARY KEY} : {int},
    cohort_group_parent_id INT,
    group_name varchar(max)
    {@include_constraints} ? {
    ,
     CONSTRAINT cohort_def_cohort_group_fk
      FOREIGN KEY(cohort_group_parent_id)
	    REFERENCES @schema.@cohort_group_definition(cohort_group_definition_id)

    }
);

{@copy_temp_table} ? {
INSERT INTO @schema.@cohort_group_definition
SELECT
    CAST(cohort_group_definition_id AS INT),
    CAST(cohort_group_parent_id AS INT),
    CAST(group_name AS varchar(max))
FROM #cohort_group_definition;
TRUNCATE TABLE #cohort_group_definition;
DROP TABLE #cohort_group_definition;
}


DROP TABLE IF EXISTS @schema.@cohort_group {@include_constraints} ? {cascade};
create table @schema.@cohort_group (
    cohort_definition_id BIGINT,
    cohort_group_definition_id INT,
    levels_of_separation INT DEFAULT 0
    {@include_constraints} ? {
    ,
    CONSTRAINT cohort_group_cohort_definition_fk
      FOREIGN KEY(cohort_definition_id)
	    REFERENCES @schema.@cohort_definition(cohort_definition_id)
	        ON DELETE CASCADE,

     CONSTRAINT cohort_def_cohort_group_fk
      FOREIGN KEY(cohort_group_definition_id)
	    REFERENCES @schema.@cohort_group_definition(cohort_group_definition_id)
	        ON DELETE CASCADE
    }
);

{@copy_temp_table} ? {
INSERT INTO @schema.@cohort_group
SELECT
    CAST(cohort_definition_id AS BIGINT),
    CAST(cohort_group_definition_id AS INT),
    CAST(levels_of_separation AS INT)
FROM #cohort_group;
TRUNCATE TABLE #cohort_group;
DROP TABLE #cohort_group;
}


/* COHORT CONCEPT SET LINK TABLE */
DROP TABLE IF EXISTS @schema.@concept_set_definition {@include_constraints} ? {cascade};
create table @schema.@concept_set_definition
(
    cohort_definition_id bigint,
	concept_set_id BIGINT,
	concept_set_name varchar(max)
	{@include_constraints} ? {
    ,
    CONSTRAINT cohort_concept_def_fk
      FOREIGN KEY(cohort_definition_id)
	    REFERENCES @schema.@cohort_definition (cohort_definition_id)
	        ON DELETE CASCADE
    }
);

{@copy_temp_table} ? {
INSERT INTO @schema.@concept_set_definition
SELECT
    CAST(cohort_definition_id AS BIGINT),
    CAST(concept_set_id AS BIGINT),
    CAST(concept_set_name AS varchar(max))
FROM #concept_set_definition;
TRUNCATE TABLE #concept_set_definition;
DROP TABLE #concept_set_definition;
}

DROP TABLE IF EXISTS @schema.@atlas_cohort_reference {@include_constraints} ? {cascade};
CREATE TABLE @schema.@atlas_cohort_reference (
    cohort_definition_id {@include_constraints} ? {int PRIMARY KEY} : {int},
    ATLAS_ID BIGINT,
    {@store_atlas_refs} ? {
    sql_definition  varchar(max),
    definition  varchar(max),}
    atlas_url varchar(1000) -- Base atlas url used to pull cohort


    {@include_constraints} ? {
    ,
    CONSTRAINT cohort_def
      FOREIGN KEY(COHORT_DEFINITION_ID)
	    REFERENCES @schema.@cohort_definition(COHORT_DEFINITION_ID)
	        ON DELETE CASCADE
	}
);

{@copy_temp_table} ? {
INSERT INTO @schema.@atlas_cohort_reference
SELECT
    CAST(cohort_definition_id AS BIGINT),
    CAST(ATLAS_ID AS BIGINT),
    CAST(atlas_url AS varchar(1000))
FROM #atlas_cohort_reference;
TRUNCATE TABLE #atlas_cohort_reference;
DROP TABLE #atlas_cohort_reference;
}


/* CONCEPT SET TABLE */
DROP TABLE IF EXISTS @schema.@cohort_concept_set {@include_constraints} ? {cascade};
create table @schema.@cohort_concept_set
(
    COHORT_DEFINITION_ID bigint,
	concept_set_id bigint,
	concept_id bigint,
	-- Stored because it can be used in shiny apps without requring full rxnorm/snomed vocab
	concept_name varchar(max),
	is_excluded INT,
	include_descendants INT,
	include_mapped INT
	{@include_constraints} ? {
    ,
    CONSTRAINT cohort_def
      FOREIGN KEY(COHORT_DEFINITION_ID)
	    REFERENCES @schema.@cohort_definition(COHORT_DEFINITION_ID)
	        ON DELETE CASCADE
    }
);

{@copy_temp_table} ? {
INSERT INTO @schema.@cohort_concept_set
SELECT
    CAST(COHORT_DEFINITION_ID AS BIGINT),
	CAST(concept_set_id AS bigint),
	CAST(concept_id AS bigint),
	CAST(concept_name AS varchar(max)),
	CAST(is_excluded AS INT),
	CAST(include_descendants AS INT),
	CAST(include_mapped AS INT)
FROM #cohort_concept_set;
TRUNCATE TABLE #cohort_concept_set;
DROP TABLE #cohort_concept_set;
}

DROP TABLE IF EXISTS @schema.@analysis_setting {@include_constraints} ? {cascade};
CREATE TABLE @schema.@analysis_setting (
    analysis_id INT {@include_constraints} ? {PRIMARY KEY},
    type_id VARCHAR(5),
    analysis_name VARCHAR(255),
    description TEXT,
    options TEXT -- JSON stored as base64 encoded string
);

{@copy_temp_table} ? {
INSERT INTO @schema.@analysis_setting
SELECT
    CAST(analysis_id AS INT),
	CAST(type_id AS VARCHAR(5)),
	CAST(analysis_name AS varchar(255)),
	CAST(description AS TEXT),
	CAST(options AS TEXT)
FROM #analysis_setting;
TRUNCATE TABLE #analysis_setting;
DROP TABLE #analysis_setting;
}
