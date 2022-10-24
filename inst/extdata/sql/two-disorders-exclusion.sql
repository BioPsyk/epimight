WITH single_disorder_tte AS (

{% include "single-disorder.sql" %}

), exclusion_diagnosis AS (
  WITH diagnoses AS (
    SELECT *
      FROM medical.people_diagnoses
     WHERE diagnosis_icd_id ~ {{ quote_sql(exclusion_diagnosis_filters.icd_codes_regexp) }}
     {% if exists("exclusion_diagnosis_filters.diagnosis_kind") %}
       AND (  diagnosis_kind IN ({{ quote_sql(exclusion_diagnosis_filters.diagnosis_kind) }})
           OR diagnosis_kind IS NULL
           )
     {% endif %}
     {% if exists("exclusion_diagnosis_filters.record_origin") %}
       AND (  record_origin IN ({{ quote_sql(exclusion_diagnosis_filters.record_origin) }})
           OR record_origin IS NULL
           )
     {% endif %}
     ORDER BY (person_id, diagnosed_at)
  ) SELECT DISTINCT ON (person_id) *
      FROM diagnoses
) SELECT tte.*
       , excl.diagnosed_at AS exclusion_diagnosed_at
    FROM single_disorder_tte AS tte
         LEFT JOIN exclusion_diagnosis AS excl
                   USING (person_id)
   WHERE tte.diagnosed_at  IS NULL
      OR excl.diagnosed_at IS NULL
      OR tte.diagnosed_at < excl.diagnosed_at
   ORDER BY tte.person_id
