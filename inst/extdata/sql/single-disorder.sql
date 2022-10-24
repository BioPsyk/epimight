WITH earliest_diagnosis AS (
  WITH diagnoses AS (
    SELECT *
      FROM medical.people_diagnoses
     WHERE diagnosis_icd_id ~ {{ quote_sql(diagnosis_filters.icd_codes_regexp) }}
     {% if exists("diagnosis_filters.diagnosis_kind") %}
       AND (  diagnosis_kind IN ({{ quote_sql(diagnosis_filters.diagnosis_kind) }})
           OR diagnosis_kind IS NULL
           )
     {% endif %}
     {% if exists("diagnosis_filters.record_origin") %}
       AND (  record_origin IN ({{ quote_sql(diagnosis_filters.record_origin) }})
           OR record_origin IS NULL
           )
     {% endif %}
     ORDER BY (person_id, diagnosed_at)
  ) SELECT DISTINCT ON (person_id) *
      FROM diagnoses
), tte AS (
  SELECT peo.id as person_id
       , peo.gender
       , peo.born_at
       , peo.father_id
       , peo.mother_id
       , peo.status
       , peo.status_changed
       , peo.birthplace_id
       , dia.record_origin
       , dia.diagnosed_at
       , dia.diagnosis_onset_age
       , dia.diagnosis_kind
       , dia.diagnosis_icd_edition
       , dia.diagnosis_icd_id
       , round(fail.failure_time)::int AS failure_time
       , fail.failure_at
       , fail.failure_status
       , fail.competing_risk_at
       , fail.study_end_at
    FROM civil.people AS peo
         LEFT JOIN earliest_diagnosis AS dia
                ON peo.id = dia.person_id
       , epidemiology.determine_failure(
             peo.born_at
           , {{ quote_sql(study_end_at) }}::date
           , dia.diagnosed_at
           , peo.status_changed
           , peo.status
           , array[
               'annulled-cpr-number', 'unknown-residency', 'emigrated'
             ]::civil.status[]
           , array[
               'dead'
             ]::civil.status[]
         ) AS fail
) SELECT *
       , CASE status
           WHEN 'unknown-residency'   THEN status_changed
           WHEN 'annulled-cpr-number' THEN status_changed
           WHEN 'emigrated'           THEN status_changed
                                      ELSE '9999-12-31'::date
         END AS censored_at
       , CASE failure_at
           WHEN diagnosed_at THEN 1
                             ELSE 0
         END AS was_diagnosed
       , CASE failure_at
           WHEN diagnosed_at      THEN 0
           WHEN competing_risk_at THEN 0
                                  ELSE 1
         END AS was_censored
   FROM tte
  WHERE
      ( birthplace_id BETWEEN   10 AND  900 -- Danish municipalities
     OR birthplace_id BETWEEN 1101 AND 1199 -- Danish courts
     OR birthplace_id BETWEEN 1301 AND 1315 -- Danish state offices, part 1
     OR birthplace_id BETWEEN 1317 AND 1325 -- Danish state offices, part 2
     OR birthplace_id BETWEEN 2401 AND 2599 -- Undisclosed place in Denmark
     OR birthplace_id BETWEEN 4601 AND 4688 -- Danish churches
     OR birthplace_id BETWEEN 6001 AND 6903 -- Danish church districts
     OR birthplace_id BETWEEN 7001 AND 9348 -- Danish parishes
     OR birthplace_id = 5100                -- Denmark (country)
     OR birthplace_id = 4998                -- Partially undisclosed place in Denmark
     )
{% if exists("sample_filters.born_at_min") %}
    AND born_at >= {{ quote_sql(sample_filters.born_at_min) }}
{% endif %}
{% if exists("sample_filters.born_at_max") %}
    AND born_at <= {{ quote_sql(sample_filters.born_at_max) }}
{% endif %}
{% if exists("sample_filters.gender") %}
    AND gender = {{ quote_sql(sample_filters.gender) }}
{% endif %}
{% if exists("sample_filters.status") %}
    AND status IN ({{ quote_sql(sample_filters.status) }})
{% endif %}
{% if exists("sample_filters.diagnosis_earliest_onset") %}
    AND failure_time >= {{ quote_sql(sample_filters.diagnosis_earliest_onset) }}
{% endif %}
{% if exists("sample_filters.diagnosis_latest_onset") %}
    AND failure_time <= {{ quote_sql(sample_filters.diagnosis_latest_onset) }}
{% endif %}
