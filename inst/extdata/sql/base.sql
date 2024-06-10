WITH samples AS (

  {% if exists("samples.individual_filters") %}
    {% set individual_filters=samples.individual_filters %}
  {% else %}
    {% set individual_filters=null %}
  {% endif %}

  {% include "partials/samples-retrieval.sql" %}
  {% set samples_key="samples" %}

), diagnosed_samples AS (

  {% set diagnosis_filters=samples.diagnosis_filters %}

  {% include "partials/diagnoses-retrieval.sql" %}

  {% if existsIn(individual_filters, "custom") %}
     -- Custom filters on diagnosed samples
     WHERE {{ individual_filters.custom }}
  {% endif %}

{% if exists("relatives") and exists("relatives.individual_filters") %}

), relatives AS (

  {% set individual_filters=relatives.individual_filters %}
  {% set samples_key="relatives" %}

  {% include "partials/samples-retrieval.sql" %}

{% endif %}

{% if exists("relatives") and exists("relatives.diagnosis_filters") %}

), diagnosed_relatives AS (

  {% set separate_diagnoses=true %}
  {% set diagnosis_filters=relatives.diagnosis_filters %}
  {% set diagnosed_samples_key="diagnosed_relatives" %}

  {% include "partials/diagnoses-retrieval.sql" %}

  {% if existsIn(individual_filters, "custom") %}
     -- Custom filters on diagnosed samples
     WHERE {{ individual_filters.custom }}
  {% endif %}


{% else %}

  {% set diagnosed_samples_key="diagnosed_samples" %}

{% endif %}

{% if exists("relatives") %}

), relative_counts AS (

{% include "partials/relatives-counting.sql" %}

) SELECT sam.person_id
   {% if exists("extra_columns") %}
       -- << extra columns
     {% for col in extra_columns %}
       , sam.{{ col }}
     {% endfor %}
       -- extra columns >>
   {% endif %}
     {% for key, filter in samples.diagnosis_filters %}
       , sam.{{ key }}_failure_status
       , sam.{{ key }}_failure_at
       , sam.{{ key }}_failure_time
     {% endfor %}
       , COALESCE(rel.relatives, 0) AS relatives
     {% for key, filter in diagnosis_filters %}
       , COALESCE(rel.{{ key }}_affected_relatives, 0) AS {{ key }}_affected_relatives
     {% endfor %}
    FROM diagnosed_samples AS sam
         LEFT JOIN relative_counts AS rel
             USING (person_id)
   ORDER BY sam.person_id;

{% else %}

) SELECT person_id
   {% if exists("extra_columns") %}
       -- << extra columns
     {% for col in extra_columns %}
       , {{ col }}
     {% endfor %}
       -- extra columns >>
   {% endif %}
     {% for key, filter in diagnosis_filters %}
       , {{ key }}_failure_status
       , {{ key }}_failure_at
       , {{ key }}_failure_time
     {% endfor %}
    FROM diagnosed_samples
   ORDER BY person_id;

{% endif %}
