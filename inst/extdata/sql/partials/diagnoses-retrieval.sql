  --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  -- earliest diagnosis for {{ samples_key }}:
{% for key, filter in diagnosis_filters %}
  --   - {{ key }}
{% endfor %}
  --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  WITH all_diagnoses AS (
    {% for key, filter in diagnosis_filters %}
      (WITH matched_icd_codes AS (
        -- Here we first select the subset of ICD codes that match the given regexp and
        -- then select all diagnoses that has a diagnosis in the retrieved ICD code subset.
        --
        -- This speeds up the diagnosis retrieval, since the regexp will only be applied to
        -- ~100k rows (the total amount of distinct ICD codes) and then an index will be used
        -- to select the diagnoses that has an ICD code inside the retrieved subset of ICD codes.
        --
        -- If we would use the regexp directly to diagnoses, it would be applied to ~200M rows
        -- and no index would be used.
        SELECT *
          FROM icd.codes
         WHERE id ~ {{ quote_sql(filter.icd_codes_regexp) }}
         {% if existsIn(filter, "icd_editions") %}
           AND edition IN ({{ quote_sql(filter.icd_editions) }})
         {% endif %}
      ), matched_diagnoses AS (
        SELECT peo.id                                           AS person_id
             , {{ quote_sql(key) }}                             AS diagnosis_group
             , peo.gender
             , peo.born_at
             , peo.status
             , peo.status_changed
             , peo.mother_id
             , peo.father_id
             , rec.origin                                       AS record_origin
             , rec.id                                           AS record_id
             , rec.patient_kind                                 AS record_patient_kind
             , rec.started_at                                   AS diagnosed_at
             , ((rec.started_at - peo.born_at) / 365.25)::float AS diagnosis_onset_age
             , dia.kind                                         AS diagnosis_kind
             , dia.icd_edition                                  AS diagnosis_icd_edition
             , dia.icd_id                                       AS diagnosis_icd_id
          FROM civil.people AS peo
               INNER JOIN medical.records AS rec
                       ON peo.id = rec.person_id
                    {% if existsIn(filter, "patient_kinds") %}
                      AND (  rec.patient_kind IN ({{ quote_sql(filter.patient_kinds) }})
                          OR rec.patient_kind IS NULL
                          )
                    {% endif %}
                    {% if existsIn(filter, "record_origin") %}
                      AND (  rec.origin IN ({{ quote_sql(filter.record_origin) }})
                          OR rec.origin IS NULL
                          )
                    {% endif %}
               INNER JOIN medical.diagnoses AS dia
                       ON rec.origin = dia.record_origin
                      AND rec.id     = dia.record_id
                    {% if existsIn(filter, "diagnosis_kinds") %}
                      AND (  dia.kind IN ({{ quote_sql(filter.diagnosis_kinds) }})
                          OR dia.kind IS NULL
                          )
                    {% endif %}
               INNER JOIN matched_icd_codes AS icd
                       ON dia.icd_id = icd.id
         ORDER BY (peo.id, rec.started_at)
      ) SELECT DISTINCT ON (person_id) *
          FROM matched_diagnoses)

      {% if not loop.is_last %}
      UNION
      {% endif %}
    {% endfor %}
  ), aggregated_diagnoses AS (
    SELECT person_id
         , jsonb_object_agg(DISTINCT diagnosis_group, diagnosed_at)          AS diagnosed_at
         , jsonb_object_agg(DISTINCT diagnosis_group, diagnosis_kind)        AS diagnosis_kind
         , jsonb_object_agg(DISTINCT diagnosis_group, diagnosis_icd_edition) AS diagnosis_icd_edition
         , jsonb_object_agg(DISTINCT diagnosis_group, diagnosis_icd_id)      AS diagnosis_icd_id
         , jsonb_object_agg(DISTINCT diagnosis_group, record_patient_kind)   AS record_patient_kind
      FROM all_diagnoses
     GROUP BY person_id
  ), final_diagnoses AS (
    SELECT dia.person_id
       {% for key, filter in diagnosis_filters %}
         , da.{{ key }} AS {{ key }}_diagnosed_at
         , dk.{{ key }} AS {{ key }}_diagnosis_kind
         , ie.{{ key }} AS {{ key }}_diagnosis_icd_edition
         , ii.{{ key }} AS {{ key }}_diagnosis_icd_id
         , pk.{{ key }} AS {{ key }}_record_patient_kind
       {% endfor %}
      FROM aggregated_diagnoses AS dia
         , jsonb_to_record(dia.diagnosed_at)
           AS da({% for key, filter in diagnosis_filters %}{% if not loop.is_first %}, {% endif %}{{ key }} date{% endfor %})
         , jsonb_to_record(dia.diagnosis_kind)
           AS dk({% for key, filter in diagnosis_filters %}{% if not loop.is_first %}, {% endif %}{{ key }} medical.diagnosis_kind{% endfor %})
         , jsonb_to_record(dia.diagnosis_icd_edition)
           AS ie({% for key, filter in diagnosis_filters %}{% if not loop.is_first %}, {% endif %}{{ key }} icd.edition{% endfor %})
         , jsonb_to_record(dia.diagnosis_icd_id)
           AS ii({% for key, filter in diagnosis_filters %}{% if not loop.is_first %}, {% endif %}{{ key }} icd.id{% endfor %})
         , jsonb_to_record(dia.record_patient_kind)
           AS pk({% for key, filter in diagnosis_filters %}{% if not loop.is_first %}, {% endif %}{{ key }} medical.patient_kind{% endfor %})
  ) SELECT sam.id as person_id
         , sam.gender
         , sam.born_at
         , sam.father_id
         , sam.mother_id
         , sam.status
         , sam.status_changed
         , sam.birthplace_id
       {% for key, filter in diagnosis_filters %}
         , dia.{{ key }}_diagnosed_at
         , dia.{{ key }}_diagnosis_kind
         , dia.{{ key }}_diagnosis_icd_edition
         , dia.{{ key }}_diagnosis_icd_id
         , dia.{{ key }}_record_patient_kind
         , {{ key }}_fail.failure_status AS {{ key }}_failure_status
         , {{ key }}_fail.failure_at     AS {{ key }}_failure_at
         , {{ key }}_fail.failure_time   AS {{ key }}_failure_time
       {% endfor %}
      FROM {{ samples_key }} AS sam
           LEFT JOIN final_diagnoses AS dia
                  ON sam.id = dia.person_id

       {% for key, filter in diagnosis_filters %}
         , epidemiology.determine_failure(
               sam.born_at
             , {{ quote_sql(study_end_at) }}::date
             , dia.{{ key }}_diagnosed_at
             , sam.status_changed
             , sam.status
             , array['annulled-cpr-number', 'unknown-residency', 'emigrated']::civil.status[]
             , array['dead']::civil.status[]
           ) AS {{ key }}_fail
       {% endfor %}
  -->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  -- end of diagnoses for {{ samples_key }}
  -->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
