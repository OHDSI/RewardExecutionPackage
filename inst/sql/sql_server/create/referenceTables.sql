{DEFAULT @concept_set_definition = 'concept_set_definition'}
{DEFAULT @cohort_definition = 'cohort_definition'}
{DEFAULT @outcome_cohort_definition = 'outcome_cohort_definition'}
{DEFAULT @atlas_outcome_reference = 'atlas_outcome_reference'}
{DEFAULT @atlas_outcome_concept = 'atlas_outcome_concept'}

{DEFAULT @atlas_exposure_reference = 'atlas_exposure_reference'}
{DEFAULT @atlas_exposure_concept = 'atlas_exposure_concept'}

{DEFAULT @analysis_setting = 'analysis_setting'}
{DEFAULT @include_constraints = ''}

IF OBJECT_ID('@schema.@concept_set_definition', 'U') IS NOT NULL
	DROP TABLE @schema.@concept_set_definition;

IF OBJECT_ID('@schema.@cohort_definition', 'U') IS NOT NULL
	DROP TABLE @schema.@cohort_definition;

IF OBJECT_ID('@schema.@outcome_cohort_definition', 'U') IS NOT NULL
	DROP TABLE @schema.@outcome_cohort_definition;

IF OBJECT_ID('@schema.@atlas_outcome_reference', 'U') IS NOT NULL
	DROP TABLE @schema.@atlas_outcome_reference;

IF OBJECT_ID('@schema.@atlas_outcome_concept', 'U') IS NOT NULL
	DROP TABLE @schema.@atlas_outcome_concept;

IF OBJECT_ID('@schema.@analysis_setting', 'U') IS NOT NULL
	DROP TABLE @schema.@analysis_setting;

IF OBJECT_ID('@schema.@atlas_exposure_reference', 'U') IS NOT NULL
	DROP TABLE @schema.@atlas_exposure_reference;

IF OBJECT_ID('@schema.@atlas_exposure_concept', 'U') IS NOT NULL
	DROP TABLE @schema.@atlas_exposure_concept;

/* CONCEPT SET TABLE */
create table @schema.@concept_set_definition
(
	CONCEPTSET_ID bigint,
	concept_name varchar(1000),
	concept_id bigint,
	is_excluded INT,
	include_Descendants INT,
	include_Mapped INT
)
;

/* COHORT DEFINITION TABLE */
create table @schema.@cohort_definition
(
	cohort_definition_id {@include_constraints != ''} ? {SERIAL PRIMARY KEY} : {bigint}
	, cohort_definition_name varchar(1000)
	,	short_name varchar(1000)
	,	drug_CONCEPTSET_ID bigint
	,	target_cohort int   	/*1 - target cohort, 0- not target (could be used as comparator, -1 - other)*/
	, subgroup_cohort int   /*tells if cohort is a subgroup of interest: 0-none, 1-pediatrics, 2-elderly, 3-pregnant women, 4-renal impairment,5-hepatic impairment*/
	, ATC_flg int           /* 1- exposure is ATC 4th level, 0-exposure is ingredient 2 - custom exposure class */
)
;

/* OUTCOME COHORT DEFINITION TABLE */
create table @schema.@outcome_cohort_definition
(
	cohort_definition_id {@include_constraints != ''} ? {SERIAL PRIMARY KEY} : {bigint}
	, cohort_definition_name varchar(1000)
	,	short_name varchar(1000)
	, CONCEPTSET_ID bigint
	, outcome_type int /*0-outcomes for 2+ diagnoses, 1-hospitalizations, 2-custom outcome cohort*/
)
;


CREATE TABLE @schema.@atlas_outcome_reference (
    COHORT_DEFINITION_ID BIGINT,
    ATLAS_ID BIGINT,
    sql_definition text,
    DEFINITION text, -- Stores sql used to generate the cohort
    atlas_url varchar(1000) -- Base atlas url used to pull cohort
    {@include_constraints != ''} ? {
    ,CONSTRAINT cohort_def
      FOREIGN KEY(COHORT_DEFINITION_ID)
	    REFERENCES @schema.@outcome_cohort_definition(COHORT_DEFINITION_ID)
	        ON DELETE CASCADE
	}

);

CREATE TABLE @schema.@atlas_outcome_concept (
    COHORT_DEFINITION_ID INT,
    CONCEPT_ID BIGINT,
    INCLUDE_DESCENDANTS INT,
    IS_EXCLUDED INT,
    INCLUDE_MAPPED INT

    {@include_constraints != ''} ? {
    ,CONSTRAINT cohort_def
      FOREIGN KEY(COHORT_DEFINITION_ID)
	    REFERENCES @schema.@outcome_cohort_definition(COHORT_DEFINITION_ID)
	        ON DELETE CASCADE
    }
);

CREATE TABLE @schema.@atlas_exposure_reference (
    COHORT_DEFINITION_ID BIGINT,
    ATLAS_ID BIGINT,
    sql_definition text,
    DEFINITION text, -- Stores sql used to generate the cohort
    atlas_url varchar(1000) -- Base atlas url used to pull cohort
    {@include_constraints != ''} ? {
    ,CONSTRAINT cohort_def
      FOREIGN KEY(COHORT_DEFINITION_ID)
	    REFERENCES @schema.@cohort_definition(COHORT_DEFINITION_ID)
	        ON DELETE CASCADE
	}

);

CREATE TABLE @schema.@atlas_exposure_concept (
    COHORT_DEFINITION_ID INT,
    CONCEPT_ID BIGINT,
    INCLUDE_DESCENDANTS INT,
    IS_EXCLUDED INT,
    INCLUDE_MAPPED INT

    {@include_constraints != ''} ? {
    ,CONSTRAINT cohort_def
      FOREIGN KEY(COHORT_DEFINITION_ID)
	    REFERENCES @schema.@cohort_definition(COHORT_DEFINITION_ID)
	        ON DELETE CASCADE
    }
);
CREATE TABLE @schema.@analysis_setting (
    analysis_id {@include_constraints != ''} ? {SERIAL PRIMARY KEY} : {INT},
    type_id VARCHAR(5),
    analysis_name VARCHAR(255),
    description TEXT,
    options TEXT -- JSON stored as base64 encoded string
)