WITH base_pop AS (
    SELECT
        subject_id,
        cohort_definition_id,
        cohort_start_date,
        cohort_end_date,
        CASE
            WHEN cohort_definition_id = @target_cohort_id THEN 'target'
            WHEN cohort_definition_id = @comparator_cohort_id THEN 'comparator'
        END AS cohort_type
    FROM @result_database_schema.cohort
    WHERE cohort_definition_id IN (@target_cohort_id, @comparator_cohort_id)
),

subjects AS (
    SELECT
        subject_id,
        MAX(CASE WHEN cohort_type = 'target' THEN cohort_start_date END) AS target_start,
        MAX(CASE WHEN cohort_type = 'target' THEN cohort_end_date END) AS target_end,
        MAX(CASE WHEN cohort_type = 'comparator' THEN cohort_start_date END) AS comparator_start,
        MAX(CASE WHEN cohort_type = 'comparator' THEN cohort_end_date END) AS comparator_end
    FROM base_pop
    GROUP BY subject_id
),

events AS (
    SELECT
        s.*,
        c.cohort_definition_id AS outcome_cohort_id,
        oc.referent_concept_id AS outcome_concept_id,
        oc.outcome_type,
        c.cohort_start_date AS outcome_start
    FROM subjects s
    INNER JOIN @result_database_schema.cohort c ON s.subject_id = c.subject_id
    INNER JOIN @result_database_schema.outcome_cohort oc ON c.cohort_definition_id = oc.cohort_definition_id
)

SELECT
    e.outcome_cohort_id,
    e.outcome_concept_id,
    e.outcome_type,
    cd.cohort_definition_name AS outcome_cohort_name,
    SUM(CASE WHEN e.outcome_start < e.target_start THEN 1 ELSE 0 END) AS target_history_any,
SUM(CASE WHEN e.outcome_start < e.comparator_start THEN 1 ELSE 0 END) AS comparator_history_any,
SUM(CASE WHEN e.outcome_start BETWEEN DATEADD(day, -365, e.target_start) AND e.target_start THEN 1 ELSE 0 END) AS target_history_365d,
SUM(CASE WHEN e.outcome_start BETWEEN DATEADD(day, -365, e.comparator_start) AND e.comparator_start THEN 1 ELSE 0 END) AS comparator_history_365d,
SUM(CASE
WHEN (e.outcome_start BETWEEN DATEADD(day, 1, e.target_start) AND e.target_end) AND
(e.comparator_start IS NULL OR
(e.comparator_start > e.target_start AND
e.outcome_start BETWEEN DATEADD(day, 1, e.target_start) AND e.comparator_start)) THEN 1
ELSE 0
END) AS target_cases,
SUM(CASE
WHEN (e.outcome_start BETWEEN DATEADD(day, 1, e.comparator_start) AND e.comparator_end) AND
(e.target_start IS NULL OR
(e.target_start > e.comparator_start AND
e.outcome_start BETWEEN DATEADD(day, 1, e.comparator_start) AND e.target_start)) THEN 1
ELSE 0
END) AS comparator_cases
FROM events e
JOIN @result_database_schema.cohort_definition cd ON e.outcome_cohort_id = cd.cohort_definition_id
GROUP BY e.outcome_cohort_id, e.outcome_concept_id, e.outcome_type, cd.cohort_definition_name
ORDER BY e.outcome_cohort_id;

