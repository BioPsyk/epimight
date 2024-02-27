  WITH family_members AS (
    ---------------------------------------------------------------------------------
    -- When person_b is the relative
    ---------------------------------------------------------------------------------
    (SELECT rel.person_a_id   AS person_id
          , rel.person_b_id   AS relative_id
          , sam.born_at       AS relative_born_at
          , rel.coefficient
          , rel.kind
          , rel.component
        {% for key, filter in diagnosis_filters %}
          , sam.{{ key }}_failure_status
        {% endfor %}
       FROM genealogy.relationships AS rel
            INNER JOIN {{ diagnosed_samples_key }} AS sam
                    ON rel.person_b_id = sam.person_id
               {% if existsIn(relatives, "relationship_filters") %}
                 {% for key, val in relatives.relationship_filters %}
                   AND rel.{{ key }} = {{ quote_sql(val) }}
                 {% endfor %}
               {% endif %})
    ---------------------------------------------------------------------------------

    UNION

    ---------------------------------------------------------------------------------
    -- When person_a is the relative
    ---------------------------------------------------------------------------------
    (SELECT rel.person_b_id AS person_id
          , rel.person_a_id AS relative_id
          , sam.born_at     AS relative_born_at
          , rel.coefficient
          , rel.kind
          , rel.component
        {% for key, filter in diagnosis_filters %}
          , sam.{{ key }}_failure_status
        {% endfor %}
       FROM genealogy.relationships AS rel
            INNER JOIN {{ diagnosed_samples_key }} AS sam
                    ON rel.person_a_id = sam.person_id
               {% if existsIn(relatives, "relationship_filters") %}
                 {% for key, val in relatives.relationship_filters %}
                   AND rel.{{ key }} = {{ quote_sql(val) }}
                 {% endfor %}
               {% endif %})
    ---------------------------------------------------------------------------------
  ) SELECT sam.person_id
         , COUNT(*)                        AS relatives
       {% for key, filter in diagnosis_filters %}
         , SUM(fam.{{ key }}_failure_status & 1) AS {{ key }}_affected_relatives
           --  Note that we use a bitwise AND here!
           --  It's so that we only sum up failure statuses that are 1.
       {% endfor %}
      FROM family_members AS fam
           INNER JOIN diagnosed_samples AS sam
                      ON fam.person_id = sam.person_id
                   {% if existsIn(relatives, "using_vertical_relationship") %}
                     AND fam.relative_born_at < sam.born_at
                     -- When using vertical relationships we don't include the descendant.
                     --
                     -- For example, when using the "PO" (parent/offspring) relationship,
                     -- the outcome of the child has no effect on the parents outcome genetically.
                     --
                     -- In contrast, when using the "FS" (full siblings) relationship,
                     -- the outcome of all siblings has an effect on each other, since they
                     -- all share the same parents.
                   {% endif %}
     GROUP BY (sam.person_id)
