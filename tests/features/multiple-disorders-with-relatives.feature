Feature: getting TTE for multiple disorders with relatives

  Scenario: three disorders and parent-offspring relationship
    Given the following set of people
      | id |    born_at | gender | status          |
      |  1 | 1963-08-10 | female | danish-resident |
      |  2 | 1965-12-04 | male   | danish-resident |
      |  3 | 1967-01-30 | male   | danish-resident |
      |  4 | 1961-05-10 | female | danish-resident |
      |  5 | 1959-08-12 | male   | danish-resident |
      |  6 | 1963-10-03 | female | danish-resident |
      | 21 | 1983-08-10 | female | danish-resident |
      | 22 | 1985-12-04 | male   | danish-resident |
      | 23 | 1987-01-30 | male   | danish-resident |
      | 24 | 1981-05-10 | female | danish-resident |
      | 25 | 1979-08-12 | male   | danish-resident |
      | 26 | 1983-10-03 | female | danish-resident |
    And the following set of peoples birthplaces
      | person_id | birthplace_name |
      |         1 | Gentofte Sogn   |
      |         2 | Valby Sogn      |
      |         3 | Brede Sogn      |
      |         4 | Gerlev Sogn     |
      |         5 | Sverige         |
      |        21 | Gentofte Sogn   |
      |        22 | Valby Sogn      |
      |        23 | Brede Sogn      |
      |        24 | Gerlev Sogn     |
      |        25 | Sverige         |
    And the following set of genealogical relationships
      | person_a_id | person_b_id | coefficient | kind |
      |          1 |           21 |         0.5 | PO   |
      |          2 |           22 |         0.5 | PO   |
      |          3 |           23 |         0.5 | PO   |
      |          4 |           24 |         0.5 | PO   |
      |          5 |           25 |         0.5 | PO   |
      |          6 |           26 |         0.5 | PO   |
    And person 1 was diagnosed with the following set of diagnoses
      | diagnosis_at | kind | icd_edition | icd_id |
      |   1990-01-01 | main | icd10       | F500   |
      |   1991-01-01 | main | icd10       | F502   |
      |   1992-01-01 | main | icd10       | F509   |
    And person 2 was diagnosed with the following set of diagnoses
      | diagnosis_at | kind | icd_edition | icd_id |
      |   1990-01-01 | main | icd10       | F500   |
      |   1991-01-01 | main | icd10       | F502   |
    And person 3 was diagnosed with the following set of diagnoses
      | diagnosis_at | kind | icd_edition | icd_id |
      |   1986-01-02 | main | icd10       | F500   |
    And person 21 was diagnosed with the following set of diagnoses
      | diagnosis_at | kind | icd_edition | icd_id |
      |   1990-01-01 | main | icd10       | F500   |
      |   1991-01-01 | main | icd10       | F502   |
      |   1992-01-01 | main | icd10       | F509   |
    And person 22 was diagnosed with the following set of diagnoses
      | diagnosis_at | kind | icd_edition | icd_id |
      |   1990-01-01 | main | icd10       | F500   |
      |   1991-01-01 | main | icd10       | F502   |
    And person 23 was diagnosed with the following set of diagnoses
      | diagnosis_at | kind | icd_edition | icd_id |
      |   1986-01-02 | main | icd10       | F500   |
    When I retrieve TTE for the following diagnoses with "PO" relatives
      | key | icd_codes_regexp |
      | ano | ^F500            |
      | bul | ^F502            |
      | uns | ^F509            |
    Then the results contains the following rows
      | person_id | ano_failure_status | ano_failure_at | bul_failure_status | bul_failure_at | uns_failure_status | uns_failure_at |
      |         1 |                  1 |     1990-01-01 |                  1 |     1991-01-01 |                  1 |     1992-01-01 |
      |         2 |                  1 |     1990-01-01 |                  1 |     1991-01-01 |                  0 |     2016-12-31 |
      |         3 |                  1 |     1986-01-02 |                  0 |     2016-12-31 |                  0 |     2016-12-31 |
      |         4 |                  0 |     2016-12-31 |                  0 |     2016-12-31 |                  0 |     2016-12-31 |
      |        21 |                  1 |     1990-01-01 |                  1 |     1991-01-01 |                  1 |     1992-01-01 |
      |        22 |                  1 |     1990-01-01 |                  1 |     1991-01-01 |                  0 |     2016-12-31 |
      |        23 |                  1 |     1986-01-02 |                  0 |     2016-12-31 |                  0 |     2016-12-31 |
      |        24 |                  0 |     2016-12-31 |                  0 |     2016-12-31 |                  0 |     2016-12-31 |
    And the results contains the following rows
      | person_id | relatives | ano_affected_relatives | bul_affected_relatives | uns_affected_relatives |
      |         1 |         0 |                      0 |                      0 |                      0 |
      |         2 |         0 |                      0 |                      0 |                      0 |
      |         3 |         0 |                      0 |                      0 |                      0 |
      |         4 |         0 |                      0 |                      0 |                      0 |
      |        21 |         1 |                      1 |                      1 |                      1 |
      |        22 |         1 |                      1 |                      1 |                      0 |
      |        23 |         1 |                      1 |                      0 |                      0 |
      |        24 |         1 |                      0 |                      0 |                      0 |
