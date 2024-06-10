Feature: getting TTE for a single disorder

  Background:
    Given the following set of people
      | id |    born_at | gender | status          |
      |  1 | 1983-08-10 | female | danish-resident |
      |  2 | 1985-12-04 | male   | danish-resident |
      |  3 | 1987-01-30 | male   | danish-resident |
      |  4 | 1981-05-10 | female | danish-resident |
      |  5 | 1979-08-12 | male   | danish-resident |
      |  6 | 1983-10-03 | female | danish-resident |

  Scenario: no one is born in Denmark
    When I retrieve TTE for the following diagnoses
      | key   | icd_codes_regexp |
      | fract | ^F500            |
    Then the results are empty

  Scenario: some born in Denmark and some outside of Denmark
    Given the following set of peoples birthplaces
      | person_id | birthplace_name |
      |         1 | Gentofte Sogn   |
      |         2 | Valby Sogn      |
      |         3 | Brede Sogn      |
      |         4 | Gerlev Sogn     |
      |         5 | Sverige         |
    When I retrieve TTE for the following diagnoses
      | key   | icd_codes_regexp |
      | fract | ^F500            |
    Then the results contains the following rows
      | person_id | fract_failure_status | fract_failure_at |
      |         1 |                    0 |       2016-12-31 |
      |         2 |                    0 |       2016-12-31 |
      |         3 |                    0 |       2016-12-31 |
      |         4 |                    0 |       2016-12-31 |

  Scenario: all born in Denmark with some target diagnoses
    Given the following set of peoples birthplaces
     | person_id | birthplace_name |
     |         1 | Gentofte Sogn   |
     |         2 | Valby Sogn      |
     |         3 | Brede Sogn      |
     |         4 | Gerlev Sogn     |
     |         5 | Sverige         |
    And person 1 was diagnosed with the following set of diagnoses
     | diagnosis_at | kind | icd_edition | icd_id |
     |   1990-05-23 | main | icd8        | 30650  |
     |   1996-01-02 | main | icd10       | F500   |
    And person 2 was diagnosed with the following set of diagnoses
     | diagnosis_at | kind | icd_edition | icd_id |
     |   1990-05-23 | main | icd8        | 30650  |
    And person 3 was diagnosed with the following set of diagnoses
     | diagnosis_at | kind | icd_edition | icd_id |
     |   1996-01-02 | main | icd10       | F5001  |
    When I retrieve TTE for the following diagnoses
      | key   | icd_codes_regexp |
      | fract | ^F500            |
    Then the results contains the following rows
     | person_id | fract_failure_status | fract_failure_at |
     |         1 |                    1 |       1996-01-02 |
     |         2 |                    0 |       2016-12-31 |
     |         3 |                    1 |       1996-01-02 |
     |         4 |                    0 |       2016-12-31 |
