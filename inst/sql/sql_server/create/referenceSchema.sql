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
{DEFAULT @migration = 'migration'}
{DEFAULT @store_atlas_refs = FALSE}


DROP TABLE IF EXISTS @schema.@migration;

DROP TABLE IF EXISTS @schema.@reference_version;
create table @schema.@reference_version
(
   reward_version_number varchar(100)
);

/* COHORT DEFINITION TABLE */
DROP TABLE IF EXISTS @schema.@cohort_definition {@include_constraints} ? {cascade};
create table @schema.@cohort_definition
(
	cohort_definition_id bigint {@include_constraints} ? {PRIMARY KEY},
	cohort_definition_name varchar(1000),
	short_name varchar(1000),
	concept_set_id bigint
);

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

/* CONCEPT SET TABLE */
DROP TABLE IF EXISTS @schema.@cohort_concept_set {@include_constraints} ? {cascade};
create table @schema.@cohort_concept_set
(
    COHORT_DEFINITION_ID bigint,
	concept_set_id bigint,
	concept_id bigint,
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

DROP TABLE IF EXISTS @schema.@analysis_setting {@include_constraints} ? {cascade};
CREATE TABLE @schema.@analysis_setting (
    analysis_id INT {@include_constraints} ? {PRIMARY KEY},
    type_id VARCHAR(5),
    analysis_name VARCHAR(255),
    description varchar(max),
    options varchar(max) -- JSON stored as base64 encoded string
);

