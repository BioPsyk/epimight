  --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  -- samples
  --<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

  SELECT *
    FROM civil.people
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

  {% if existsIn(individual_filters, "born_at_min") %}
     AND born_at >= {{ quote_sql(individual_filters.born_at_min) }}
  {% endif %}
  {% if existsIn(individual_filters, "born_at_max") %}
     AND born_at <= {{ quote_sql(individual_filters.born_at_max) }}
  {% endif %}
  {% if existsIn(individual_filters, "gender") %}
     AND gender = {{ quote_sql(individual_filters.gender) }}
  {% endif %}
  {% if existsIn(individual_filters, "status") %}
     AND status IN ({{ quote_sql(individual_filters.status) }})
  {% endif %}

  -->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  -- end of samples
  -->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
