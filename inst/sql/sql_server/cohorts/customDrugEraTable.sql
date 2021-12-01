-- Create custom drug eras
IF object_id('@drug_era_schema.drug_era', 'U') is not null
    drop table @drug_era_schema.drug_era;

IF OBJECT_ID('@drug_era_schema.drug_era_info', 'U') IS NOT NULL
    drop table @drug_era_schema.drug_era_info;

create table @drug_era_schema.drug_era_info (
  ingredient_concept_id int NOT NULL,
	ingredient_name varchar(50) NOT NULL,
	maintenance_days int NOT NULL)
;