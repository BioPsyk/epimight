Feature: getting two disorders exclusion

  Background:
    Given the following set of people
      | id |    born_at | gender | status          |
      |  1 | 1983-08-10 | female | danish-resident |
      |  2 | 1985-12-04 | male   | danish-resident |
      |  3 | 1987-01-30 | male   | danish-resident |
      |  4 | 1981-05-10 | female | danish-resident |
      |  5 | 1979-08-12 | male   | danish-resident |
      |  6 | 1983-10-03 | female | danish-resident |
    And the following set of peoples birthplaces
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

  Scenario: ICD-10 target diagnosis and ICD-8 exclusion diagnosis
    When I retrieve TTE for "^F500|^F501" with "^30650" excluded
    Then the results contains the following rows
      | person_id | failure_status | failure_at |
      |         2 |              0 | 2016-12-31 |
      |         3 |              1 | 1996-01-02 |
      |         4 |              0 | 2016-12-31 |

  Scenario: ICD-10 target diagnosis and ICD-8 exclusion diagnosis with full siblings
    Given the following set of genealogical relationships
      | person_a_id | person_b_id | coefficient | kind | component |
      |           2 |           3 |         0.5 | FS   | pedigree1 |
    When I retrieve TTE for "^F500|^F501" with "^30650" excluded with "FS" relatives
    Then the results contains the following rows
      | person_id | failure_status | failure_at | relatives | diagnosed_relatives |
      |         2 |              0 | 2016-12-31 |         1 |                   1 |
      |         3 |              1 | 1996-01-02 |         1 |                   0 |
      |         4 |              0 | 2016-12-31 |         0 |                   0 |
