#' @title Generates raw SQL queries by provided column filters.
#' @description
#' R6 class that generates raw SQL queries for different types of advanced data retrieval
#' tasks. The generated SQL queries are generated in a way that should be readable and easy
#' to debug and analyze.
#' @docType class
#' @import R6
#' @import stringr
#' @export
QueryGenerator <- R6::R6Class(
  "QueryGenerator",
  public = list(
    #' @description
    #' Generates an SQL query that retrieves survival data for a single disease/disorder/trait/risk factor
    #' for the whole population in the given study period. Each row represents a single person
    #' in the population. The following columns are provided in the results:
    #'
    #'   - person_id
    #'   - gender
    #'   - born_at
    #'   - status
    #'   - mother_id
    #'   - father_id
    #'   - status_changed
    #'   - diagnosed_at
    #'   - competing_risk_at
    #'   - censored_at
    #'   - study_end_at
    #'   - record_origin
    #'   - diagnosis_onset_age
    #'   - diagnosis_kind
    #'   - diagnosis_icd_edition
    #'   - diagnosis_icd_id
    #'   - failure_at
    #'   - failure_time
    #'   - failure_status
    #'   - was_diagnosed
    #'   - was_censored
    #'
    #' Some facts about how the survival data is retrieved:
    #'
    #'   - The start_date of the medical record that the earliest diagnosis belonged to is used as diagnosis date
    #'   - There are 4 different events that can happen during the study:
    #'     - Person was diagnosed
    #'     - Person was censored
    #'     - Person was affected by competing risk
    #'     - Person survived study with no other event
    #'   - failure_at (date of event) is derived by picking the event that happened first of the 4 events for each person.
    #'   - failure_time is the age of the person at the time of the failure.
    #'   - failure_status is:
    #'     - 2 if person was affected by competing risk
    #'     - 1 if person was diagnosed
    #'     - 0 if censored or survived study with no other event
    #'
    #' @param icd_codes_regexp Regular expression to use when selecting ICD codes for diagnosis.
    #' @param study_end_at Date of when the study ends.
    #' @param birth_date_min Minimum birth date of persons to include in final result.
    #' @param birth_date_max Maximum birth date of persons to include in final result.
    #' @param earliest_onset Earliest age of onset of persons to include in final result (Non-diagnosed are always included).
    #' @param latest_onset Latest age of onset of persons to include in final result (Non-diagnosed are always included).
    #' @param gender Gender of persons to include in final result.
    #' @param status Statuses of persons to include in final result.
    #' @param diagnosis_kind Diagnosis kinds to use when classifying if a person was diagnosed or not.
    #' @param record_origin Record origin (which register) to use when classifying if a person was diagnosed or not
    #' @return Raw SQL query as a regular string.
    survival_by_icd_codes = function(
      icd_codes_regexp,
      study_end_at,
      birth_date_min=NULL,
      birth_date_max=NULL,
      earliest_onset=NULL,
      latest_onset=NULL,
      gender=NULL,
      status=NULL,
      diagnosis_kind=NULL,
      record_origin=NULL
    ) {
      query_template <- "
      WITH earliest_diagnoses AS (
        SELECT dia.person_id
             , dia.record_origin
             , dia.diagnosed_at
             , dia.diagnosis_onset_age
             , dia.diagnosis_kind
             , dia.diagnosis_icd_edition
             , dia.diagnosis_icd_id
          FROM medical.people_diagnoses AS dia
               INNER JOIN icd.codes AS icd
                       ON dia.diagnosis_icd_edition = icd.edition
                      AND dia.diagnosis_icd_id      = icd.id
                      AND icd.id ~ '${icd_codes_regexp}'
         ${diagnosis_where_clause}
         ORDER BY (person_id, diagnosed_at)
      ), single_diagnosis AS (
        SELECT DISTINCT ON (person_id) *
          FROM earliest_diagnoses
      ), population AS (
        SELECT peo.id AS person_id
             , peo.gender
             , peo.born_at
             , peo.status
             , peo.mother_id
             , peo.father_id
             , COALESCE(peo.status_changed, '9999-12-31')::date AS status_changed
             , COALESCE(dia.diagnosed_at, '9999-12-31')::date   AS diagnosed_at
             , CASE status
                 WHEN 'dead' THEN peo.status_changed
                             ELSE '9999-12-31'::date
               END AS competing_risk_at
             , CASE peo.status
                 WHEN 'unknown-residency'   THEN peo.status_changed
                 WHEN 'annulled-cpr-number' THEN peo.status_changed
                 WHEN 'emigrated'           THEN peo.status_changed
                                            ELSE '9999-12-31'::date
               END AS censored_at
             , '${study_end_at}'::date AS study_end_at
             , dia.record_origin
             , dia.diagnosis_onset_age
             , dia.diagnosis_kind
             , dia.diagnosis_icd_edition
             , dia.diagnosis_icd_id
          FROM civil.people AS peo
               LEFT JOIN single_diagnosis AS dia
                      ON peo.id = dia.person_id
         WHERE peo.birthplace_id BETWEEN   10 AND  900 -- Danish municipalities
            OR peo.birthplace_id BETWEEN 1101 AND 1199 -- Danish courts
            OR peo.birthplace_id BETWEEN 1301 AND 1315 -- Danish state offices, part 1
            OR peo.birthplace_id BETWEEN 1317 AND 1325 -- Danish state offices, part 2
            OR peo.birthplace_id BETWEEN 2401 AND 2599 -- Undisclosed place in Denmark
            OR peo.birthplace_id BETWEEN 4601 AND 4688 -- Danish churches
            OR peo.birthplace_id BETWEEN 6001 AND 6903 -- Danish church districts
            OR peo.birthplace_id BETWEEN 7001 AND 9348 -- Danish parishes
            OR peo.birthplace_id = 5100                -- Denmark (country)
            OR peo.birthplace_id = 4998                -- Partially undisclosed place in Denmark
      ), first_date_resolve AS (
        SELECT *
             , LEAST(study_end_at, diagnosed_at, competing_risk_at, censored_at)::date AS failure_at
          FROM population
      ), final AS (
        SELECT *
             , CASE failure_at
                 WHEN diagnosed_at      THEN 1
                 WHEN competing_risk_at THEN 2
                                        ELSE 0
               END AS failure_status
             , ((failure_at - born_at) / 365.25)::int AS failure_time
             , CASE failure_at
                 WHEN diagnosed_at THEN 1
                                   ELSE 0
               END AS was_diagnosed
             , CASE failure_at
                 WHEN diagnosed_at      THEN 0
                 WHEN competing_risk_at THEN 0
                                        ELSE 1
               END AS was_censored
          FROM first_date_resolve
      ) SELECT *
          FROM final
          ${final_where_clause}
      "

      final_where_clause <- list()

      if (!is.null(birth_date_min)) {
        final_where_clause <- append(
          final_where_clause,
          sprintf("born_at >= '%s'", birth_date_min)
        )
      }

      if (!is.null(birth_date_max)) {
        final_where_clause <- append(
          final_where_clause,
          sprintf("born_at <= '%s'", birth_date_max)
        )
      }

      if (!is.null(earliest_onset)) {
        final_where_clause <- append(
          final_where_clause,
          sprintf("failure_time >= %s", earliest_onset)
        )
      }

      if (!is.null(latest_onset)) {
        final_where_clause <- append(
          final_where_clause,
          sprintf("failure_time <= %s", latest_onset)
        )
      }

      if (!is.null(gender) && !setequal(gender, c("male", "female"))) {
        gender <- paste("'", gender, "'", sep="", collapse=",")
        final_where_clause <- append(final_where_clause, sprintf("gender IN (%s)", gender))
      }

      if (!is.null(status)) {
        status <- paste("'", status, "'", sep="", collapse=",")

        final_where_clause <- append(final_where_clause, sprintf("status IN (%s)", status))
      }

      diagnosis_where_clause <- list()

      if (!is.null(diagnosis_kind)) {
        diagnosis_kind <- paste("'", diagnosis_kind, "'", sep="", collapse=",")

        diagnosis_where_clause <- append(
          diagnosis_where_clause,
          sprintf("diagnosis_kind IN (%s) OR diagnosis_kind IS NULL", diagnosis_kind)
        )
      }

      if (!is.null(record_origin) && !setequal(record_origin, c("pcrr", "npr"))) {
        record_origin <- paste("'", record_origin, "'", sep="", collapse=",")

        diagnosis_where_clause <- append(
          diagnosis_where_clause,
          sprintf("(record_origin IN (%s) OR record_origin IS NULL)", record_origin)
        )
      }

      if (length(diagnosis_where_clause) > 0) {
        diagnosis_where_clause <- paste(diagnosis_where_clause, sep="", collapse="\n AND ")
        diagnosis_where_clause <- sprintf("WHERE %s", diagnosis_where_clause)
      } else {
        diagnosis_where_clause <- ""
      }

      if (length(final_where_clause) > 0) {
        final_where_clause <- paste(final_where_clause, sep="", collapse="\n AND ")
        final_where_clause <- sprintf("WHERE %s", final_where_clause)
      } else {
        final_where_clause <- ""
      }

      query_params <- list(
        icd_codes_regexp=icd_codes_regexp,
        diagnosis_where_clause=diagnosis_where_clause,
        final_where_clause=final_where_clause,
        study_end_at=study_end_at
      )
      query <- stringr::str_interp(query_template, query_params)

      return(query)
    },
    #' @description
    #' Generates an SQL query that retrieves survival data (by using survival_by_icd_codes) and calculates
    #' how many family members that were diagnosed each person has. Adds a new column at the end of all columns
    #' produced by survival_by_icd_codes called diagnosed_relatives.
    #'
    #' @param icd_codes_regexp Regular expression to use when selecting ICD codes for diagnosis.
    #' @param study_end_at Date of when the study ends.
    #' @param birth_date_min Minimum birth date of persons to include in final result.
    #' @param birth_date_max Maximum birth date of persons to include in final result.
    #' @param earliest_onset Earliest age of onset of persons to include in final result (Non-diagnosed are always included).
    #' @param latest_onset Latest age of onset of persons to include in final result (Non-diagnosed are always included).
    #' @param gender Gender of persons to include in final result.
    #' @param status Statuses of persons to include in final result.
    #' @param diagnosis_kind Diagnosis kinds to use when classifying if a person was diagnosed or not.
    #' @param record_origin Record origin (which register) to use when classifying if a person was diagnosed or not
    #' @param relationship_kind What kind of relationships should be inspected when counting diagnosed family members.
    #' @return Raw SQL query as a regular string.
    family_survival_by_icd_codes = function(
      icd_codes_regexp,
      study_end_at,
      birth_date_min=NULL,
      birth_date_max=NULL,
      earliest_onset=NULL,
      latest_onset=NULL,
      gender=NULL,
      status=NULL,
      diagnosis_kind=NULL,
      record_origin=NULL,
      relationship_kind=NULL,
      vertical_relationships=c("PO", "1G")
    ) {
      query_template <- "
      WITH survival AS (
        ${survival_query}
      ), family_members AS (
        SELECT rel.person_a_id   AS person_id
             , rel.person_b_id   AS relative_id
             , sur.born_at       AS relative_born_at
             , sur.was_diagnosed AS relative_was_diagnosed
             , rel.coefficient
             , rel.kind
             , rel.component
          FROM genealogy.relationships AS rel
               INNER JOIN survival AS sur
                       ON rel.person_b_id = sur.person_id
         ${relationship_where_clause}

        UNION

        SELECT rel.person_b_id   AS person_id
             , rel.person_a_id   AS relative_id
             , sur.born_at       AS relative_born_at
             , sur.was_diagnosed AS relative_was_diagnosed
             , rel.coefficient
             , rel.kind
             , rel.component
          FROM genealogy.relationships AS rel
               INNER JOIN survival AS sur
                       ON rel.person_a_id = sur.person_id
         ${relationship_where_clause}
      ), diagnosed_count AS (
        SELECT sur.person_id
             , COUNT(*)                        AS relatives
             , SUM(fam.relative_was_diagnosed) AS diagnosed_relatives
          FROM family_members AS fam
               INNER JOIN survival AS sur
                          ON fam.person_id = sur.person_id
          ${relative_count_where_clause}
         GROUP BY (sur.person_id)
       ) SELECT sur.person_id
              , sur.failure_time
              , sur.failure_status
              , COALESCE(dia.relatives, 0)           AS relatives
              , COALESCE(dia.diagnosed_relatives, 0) AS diagnosed_relatives
           FROM survival AS sur
                LEFT JOIN diagnosed_count AS dia
                          USING (person_id)
      "

      survival_query <- self$survival_by_icd_codes(
        icd_codes_regexp,
        study_end_at,
        birth_date_min,
        birth_date_max,
        earliest_onset,
        latest_onset,
        gender,
        status,
        diagnosis_kind,
        record_origin
      )

      relationship_where_clause <- list("rel.component = 'pedigree1'")
      relative_count_where_clause <- list()

      if (!is.null(relationship_kind)) {
        relationship_where_clause <- append(
          relationship_where_clause,
          sprintf("rel.kind = '%s'", relationship_kind)
        )

        if (relationship_kind %in% vertical_relationships) {
          relative_count_where_clause <- append(
            relative_count_where_clause,
            "fam.relative_born_at < sur.born_at"
          )
        }
      }

      if (length(relationship_where_clause) > 0) {
        relationship_where_clause <- paste(relationship_where_clause, sep="", collapse="\n AND ")
        relationship_where_clause <- sprintf("WHERE %s", relationship_where_clause)
      } else {
        relationship_where_clause <- ""
      }

      if (length(relative_count_where_clause) > 0) {
        relative_count_where_clause <- paste(relative_count_where_clause, sep="", collapse="\n AND ")
        relative_count_where_clause <- sprintf("WHERE %s", relative_count_where_clause)
      } else {
        relative_count_where_clause <- ""
      }

      query <- stringr::str_interp(query_template, list(
        survival_query=survival_query,
        relationship_where_clause=relationship_where_clause,
        relative_count_where_clause=relative_count_where_clause
      ))

      return(query)
    },
    #' @description
    #' Executes the given query using the PostgreSQL client psql outside of R
    #' and saves the results as a CSV-file of the given output path.
    #'
    #' @param query SQL query to execute.
    #' @param output_path Path of file to output the results into.
    #' @return Error code returned by the psql command.
    execute_query = function(query, output_path, hostname, username=NULL, password=NULL) {
      env <- c()

      if (!is.null(username)) {
        env <- append(env, sprintf("PGUSER=%s", username))
      }

      if (!is.null(password)) {
        env <- append(env, sprintf("PGPASSWORD=%s", password))
      }

      err_code <- system2(
        "psql",
        args=c(
          "-h", hostname,
          "-P", "footer=off",
          "-A",
          "-F','",
          "-o", output_path,
          "ibp_registry"
        ),
        env=env,
        input=query
      )

      return(err_code)
    }
  )
)
