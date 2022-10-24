WITH input_tte AS (
{% block tte %}{% endblock %}
), family_members AS (
 SELECT rel.person_a_id   AS person_id
      , rel.person_b_id   AS relative_id
      , tte.born_at       AS relative_born_at
      , tte.was_diagnosed AS relative_was_diagnosed
      , rel.coefficient
      , rel.kind
      , rel.component
   FROM genealogy.relationships AS rel
        INNER JOIN input_tte AS tte
                ON rel.person_b_id = tte.person_id
{% if exists("relationship_filters") %}
  {% for key, val in relationship_filters %}
               AND rel.{{ key }} = {{ quote_sql(val) }}
  {% endfor %}
{% endif %}

 UNION

 SELECT rel.person_b_id   AS person_id
      , rel.person_a_id   AS relative_id
      , tte.born_at       AS relative_born_at
      , tte.was_diagnosed AS relative_was_diagnosed
      , rel.coefficient
      , rel.kind
      , rel.component
   FROM genealogy.relationships AS rel
        INNER JOIN input_tte AS tte
                ON rel.person_a_id = tte.person_id
{% if exists("relationship_filters") %}
  {% for key, val in relationship_filters %}
               AND rel.{{ key }} = {{ quote_sql(val) }}
  {% endfor %}
{% endif %}
), diagnosed_count AS (
 SELECT tte.person_id
      , COUNT(*)                        AS relatives
      , SUM(fam.relative_was_diagnosed) AS diagnosed_relatives
   FROM family_members AS fam
        INNER JOIN input_tte AS tte
                   ON fam.person_id = tte.person_id
{% if exists("using_vertical_relationship") %}
  WHERE fam.relative_born_at < tte.born_at
{% endif %}
  GROUP BY (tte.person_id)
)
{% block final_select %}
  SELECT tte.person_id
       , tte.failure_time
       , tte.failure_status
       , tte.failure_at
       , COALESCE(dia.relatives, 0)           AS relatives
       , COALESCE(dia.diagnosed_relatives, 0) AS diagnosed_relatives
    FROM input_tte AS tte
         LEFT JOIN diagnosed_count AS dia
                   USING (person_id)
{% endblock %}
