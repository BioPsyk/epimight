relationship_kinds = list(
  "1C" = 0.125,
  "1C1R" = 0.0625,
  "1C2R" = 0.03125,
  "1C3R" = 0.015625,
  "1G" = 0.25,
  "1GAv" = 0.125,
  "1GHAv" = 0.0625,
  "2C" = 0.03125,
  "2C1R" = 0.015625,
  "2C2R" = 0.0078125,
  "2G" = 0.125,
  "2GHAv" = 0.03125,
  "2GAv" = 0.0625,
  "3C" = 0.0078125,
  "3C1R" = 0.00390625,
  "3G" = 0.0625,
  "3GAv" = 0.03125,
  "4C" = 0.001953125,
  "4G" = 0.03125,
  "Av" = 0.25,
  "FS" = 0.5,
  "H1C" = 0.0625,
  "H1C1R" = 0.03125,
  "H1C2R" = 0.015625,
  "H2C" = 0.015625,
  "H2C1R" = 0.0078125,
  "HAv" = 0.125,
  "HS" = 0.25,
  "mHS" = 0.25,
  "pHS" = 0.25,
  "PO" = 0.5
)

vertical_relationship_kinds = list(
  "PO",
  "1G",
  "2G",
  "3G",
  "4G",
  "Av",
  "1GAv",
  "2GAv",
  "3GAv"
)

civil_statuses = list(
  {%- for val in civil_statuses %}
    "{{ val }}"{% if not loop.last %},{% endif %}
  {%- endfor %}
)

diagnosis_kinds = list(
  {%- for val in diagnosis_kinds %}
    "{{ val }}"{% if not loop.last %},{% endif %}
  {%- endfor %}
)

genders = list(
  {%- for val in genders %}
    "{{ val }}"{% if not loop.last %},{% endif %}
  {%- endfor %}
)

usethis::use_data(
  relationship_kinds,
  vertical_relationship_kinds,
  civil_statuses,
  diagnosis_kinds,
  genders,
  internal  = TRUE,
  overwrite = TRUE
)
