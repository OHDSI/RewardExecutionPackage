{DEFAULT @batch_size = 10000}

IF OBJECT_ID('tempdb..#cohorts_to_compute', 'U') IS NOT NULL
	DROP TABLE #cohorts_to_compute;

--HINT DISTRIBUTE_ON_KEY(cohort_definition_id)
create table #cohorts_to_compute AS
SELECT TOP @batch_size ocr.cohort_definition_id
FROM @reference_schema.@outcome_cohort_definition ocr
LEFT JOIN #computed_o_cohorts coc ON coc.cohort_definition_id = ocr.cohort_definition_id
WHERE coc.cohort_definition_id IS NULL
AND ocr.outcome_type = @outcome_type;