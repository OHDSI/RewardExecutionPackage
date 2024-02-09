with base_pop as (

	select * from @result_database_schema.cohort where cohort_definition_id in (@target_cohort_id, @comparator_cohort_id)

),

denom as (
	
	select 
		cohort_definition_id,
		count(distinct subject_id) as n_new_users
	from base_pop
	group by cohort_definition_id 

),

target as (

	select 
		subject_id,
		cohort_start_date as target_start,
		cohort_end_date as target_end
	from base_pop 
	where cohort_definition_id = @target_cohort_id

),

comparator as (

	select 
		subject_id,
		cohort_start_date as comparator_start,
		cohort_end_date as comparator_end
	from base_pop 
	where cohort_definition_id = @comparator_cohort_id

),

subjects as (

	select 
		case 
			when t.subject_id is null then c.subject_id
			else t.subject_id
		end as subject_id,
		t.target_start,
		t.target_end,
		c.comparator_start,
		c.comparator_end
	from target t
	full join comparator c
		on t.subject_id = c.subject_id
),

events as (

	select 
		s.*,
		e.outcome_cohort_id,
		e.outcome_concept_id,
		e.outcome_type,
		e.outcome_start
	from (
		select 
			c.cohort_definition_id as outcome_cohort_id,
			oc.referent_concept_id as outcome_concept_id,
			oc.outcome_type,
			c.subject_id,
			c.cohort_start_date as outcome_start
		from @result_database_schema.cohort c
		inner join @result_database_schema.outcome_cohort oc 
			on c.cohort_definition_id = oc.cohort_definition_id 
	) as e
	inner join subjects s
		on e.subject_id = s.subject_id

)


select
	e.outcome_cohort_id,
	e.outcome_concept_id,
	e.outcome_type,
	cd.cohort_definition_name as outcome_cohort_name,
	sum(case when e.outcome_start < e.target_start then 1 else 0 end) as target_history_any,
	sum(case when e.outcome_start < e.comparator_start then 1 else 0 end) as comparator_history_any,
	sum(case when e.outcome_start between dateadd(day, -365, e.target_start) and e.target_start then 1 else 0 end) as target_history_365d,
	sum(case when e.outcome_start between dateadd(day, -365, e.comparator_start) and e.comparator_start then 1 else 0 end) as comparator_history_365d,
	sum(case 
		when (e.outcome_start between dateadd(day, 1, e.target_start) and e.target_end) and 
		     (e.comparator_start is null or 
		      (e.comparator_start > e.target_start and 
		       e.outcome_start between dateadd(day, 1, e.target_start) and e.comparator_start)) then 1 
		else 0 end) as target_cases,
	sum(case 
		when (e.outcome_start between dateadd(day, 1, e.comparator_start) and e.comparator_end) and 
		     (e.target_start is null or 
		      (e.target_start > e.comparator_start and 
		       e.outcome_start between dateadd(day, 1, e.comparator_start) and e.target_start)) then 1 
		else 0 end) as comparator_cases
from events e
join @result_database_schema.cohort_definition cd 
	on e.outcome_cohort_id = cd.cohort_definition_id 
group by e.outcome_cohort_id, e.outcome_concept_id, e.outcome_type, cd.cohort_definition_name 
order by e.outcome_cohort_id;
