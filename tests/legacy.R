
#=================================================================================
# General population
#=================================================================================

CIF_General_Population_Risk = function(cuminc_data, earliest_onset, latest_onset) {
  # Retain only diagnosis status and time to event
  cuminc_data = cuminc_data[,c("failure_status", "failure_time")]

	# Calculate Cumulative Incidence using competing risk
	cum1 = cuminc(ftime = cuminc_data$failure_time, fstatus = cuminc_data$failure_status, cencode = 0)

  # Make new matrix of cuminc function for 1 group
	x = data.frame(Time=cum1$`1 1`$time, Estimate=cum1$`1 1`$est,Variance=cum1$`1 1`$var)
	# Remove TTE below and above threshold
	x = x[(x[,1] >= earliest_onset & x[,1] <= latest_onset),]

  # Remove duplicated time to even estimates
  #x = x[seq(1,nrow(x),2),]
  x = data.frame(x |> group_by(Time) |> top_n(-1, Estimate))

	res = NULL
  #ctrls_c = 0
  cases_c = 0

	# Calculate upper and lower conficence interval
  # controls are defined as: everybody regardless of disorder status that have a follow-up time larger than a given time point e.g., everybody with a follow up time larger than 20
	for(i in 1:nrow(x))
	{
    #ctrls = sum(cuminc_data[cuminc_data[,2] == x[i,1],3] == 0)
    cases = sum(cuminc_data[cuminc_data[,2] == x[i,1],1] == 1)

    #population =  nrow(cuminc_data[cuminc_data[,2] >= x[i, 1],])
    cases_c = cases_c + cases
		#res   = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]), cases_c, population))
    res   = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]), cases_c))
	}

	#colnames(res) = c("L95","U95","Cases", "Controls")
  colnames(res) = c("L95","U95","Cases")
	x = cbind(x,res)

	return(x)
}

CumulativeIncidence_GenPop_byYOB = function(cuminc_data, earliest_onset, latest_onset) {
	#cuminc_data = data.frame(cuminc_data)
	cuminc_data$born_at = as.numeric(format(cuminc_data$born_at,'%Y'))
	# Retain only diagnosis status and time to event
	cuminc_data = cuminc_data[,c('born_at','failure_status', 'failure_time')]


	final = NULL
	for(year in c("all", sort(unique(cuminc_data$born_at))))
	{
		if(year == "all")
		{
			dat = cuminc_data
		} else {
			dat = cuminc_data[cuminc_data$born_at == year,]
		}

    # When there are only censored individuals, cuminc will fail with the internal error:
    # "Error in `cuminc(ftime = dat$failure_time, fstatus = dat$failure_status, # cencode = 0)`:
    # NAs in foreign function call (arg 3)".
    # Therefor we need to
    if (nrow(dat[dat$failure_status != 0,]) == 0) {
      next
    }

    cum1 = cuminc(ftime = dat$failure_time, fstatus = dat$failure_status, cencode = 0)
    x = data.frame(Time=cum1$`1 1`$time, Estimate=cum1$`1 1`$est,Variance=cum1$`1 1`$var)

    if (nrow(x) == 0) {
      next
    }

		# Remove TTE below and above threshold
		x = x[(x[,1] >= earliest_onset & x[,1] <= latest_onset),]
		# Remove duplicated time to even estimates
		#x = x[seq(1,nrow(x),2),]
		x = data.frame(x |> group_by(Time) |> top_n(-1, Estimate))

    if (nrow(x) == 0) {
      next
    }

		res = NULL

    cases_c = 0
    # Calculate upper and lower conficence interval
    # controls are defined as: everybody regardless of disorder status that have a follow-up time larger than a given time point e.g., everybody with a follow up time larger than 20
    for(i in 1:nrow(x))
    {
      if(year == "all")
      {
        cases = sum(cuminc_data[cuminc_data[,3] == x[i,1],2] == 1)
      } else{
        cases = sum(cuminc_data[cuminc_data[,3] == x[i,1] & cuminc_data$born_at == year,2] == 1)
      }
                                        #population =  nrow(cuminc_data[cuminc_data[,2] >= x[i, 1],])
      cases_c = cases_c + cases
      res   = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]),year,cases_c))
                                        #res   = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]),year))
    }

                                        #colnames(res) = c("L95","U95","Year")

    colnames(res) = c("L95","U95","Year", "N Affected Individuals")
    final = rbind(final,cbind(x,res))
	}

  for(col in 1:5)
  {
    final[,col] = as.numeric(final[,col])
  }

	mat = cbind(unique(final$Time))
	colnames(mat)= "Time"

	counter=2
	for(year in c("all", sort(unique(cuminc_data$born_at))))
	{
		mat = merge(mat, final[final$Year == year,c(1,2)], by="Time", all.x=T)
		colnames(mat)[counter] = year
		counter = counter + 1
	}

	colnames(mat)[1] = "Age"
	mat = list(final, mat)
  names(mat) = c("Combined", "General_population")
	return(mat)
}

#=================================================================================
# Familial
#=================================================================================

CumulativeIncidence_familial_withinDisorder = function(survival_data, earliest_onset, latest_onset, nFamMember) {
  #survival_data = data.frame(survival_data)

  letter = strsplit(nFamMember,"")[[1]][1]
  number = paste(strsplit(nFamMember,"")[[1]][2:length(strsplit(nFamMember,"")[[1]])], collapse="")

  #survival_data = survival_data[survival_data[,4] > 0,]

  if(letter == "e")
  {
    survival_data = survival_data[survival_data$diagnosed_relatives == number, c(1:3,5,4)]
  } else if(letter == "s"){
    survival_data = survival_data[survival_data$diagnosed_relatives <= number, c(1:3,5,4)]
  } else if(letter == "l"){
    survival_data = survival_data[survival_data$diagnosed_relatives >= number, c(1:3,5,4)]
  }

  if(nrow(survival_data) == 0)
  {
    print("No individuals left after filtering. Returning NULL")
    return(NULL)
    exit
  }

  #survival_data =  survival_data[survival_data[,4] == nFamMember, c(1:3,5)]
  #survival_data =  survival_data[survival_data[,4] > 0, c(1:3,5)]

  anygroup = survival_data[survival_data$diagnosed_relatives > 0,]
  anygroup$diagnosed_relatives = "Any"
  cuminc_data = rbind(anygroup,survival_data)
  rm(anygroup)

 	nonegroup = survival_data[survival_data$relatives == 0,]
	nonegroup$diagnosed_relatives = "NoFamilyMembers"
	cuminc_data = rbind(cuminc_data,nonegroup)
	rm(nonegroup)


  n_familymembers <- survival_data |>
                                      summarise(max_diagnosed_relatives = max(diagnosed_relatives)) |>
                                      pull(max_diagnosed_relatives)

#  stop("Joeri integration time!")

	x = NULL
	# Loop though classes to check if consuring column is multiple options. If not delete group
	for(i in c("Any", "NoFamilyMembers", 1:n_familymembers))
	{
    dat <- cuminc_data[cuminc_data$diagnosed_relatives == i,]

    # When there are only censored individuals, cuminc will fail with the internal error:
    # "Error in `cuminc(ftime = dat$failure_time, fstatus = dat$failure_status, # cencode = 0)`:
    # NAs in foreign function call (arg 3)".
    # Therefor we need to
    if (nrow(dat[dat$failure_status != 0,]) == 0) {
			cat("Group",i, "Affected Family Members: either did not excist or contained no censured individuals and was therefore removed", "\n")
      next
    }

		# Calculate Cumulative Incidence using competing risk
		# Some error occured when I tries it out using everybody in one go, but when running the data one by one it seems to work.
    # While the group statement is redundant I use it later on to keep the group names
		cum1 = cuminc(  ftime = cuminc_data[cuminc_data$diagnosed_relatives == i,]$failure_time,
                    fstatus = cuminc_data[cuminc_data$diagnosed_relatives == i,]$failure_status,
                    #group=cuminc_data[cuminc_data$diagnosed_relatives == i,]$diagnosed_relatives,
                    cencode = 0
                  )

    if (is.null(cum1$`1 1`)) {
      next
    }

    sel = cbind(data.frame(cum1$`1 1`), i)
    # Remove TTE below and above hreshold
	  sel = sel[(sel[,1] >= earliest_onset & sel[,1] <= latest_onset),]
    sel = data.frame(sel |> group_by(time) |> top_n(-1, est))

		x = rbind(x, sel)
	}

  if(is.null(x))
  {
    print("No CIF could be calculated. Returning NULL")
    return(NULL)
    exit
  }

	colnames(x) = c("Time","Estimate","Variance","N Affected Family Members")

	res = NULL
  #ctrls_c = 0
  cases_c = 0
  start_val = x[1,4]

	# Calculate upper and lower conficence interval and Ns
	for(i in 1:nrow(x))
	{
    #ctrls = sum(cuminc_data[cuminc_data[,2] == x[i,1] & cuminc_data[,5] == x[i,4] ,3] == 0)
    cases = sum(cuminc_data[cuminc_data$failure_time == x[i,1] & cuminc_data$diagnosed_relatives == x[i,4] ,3] == 1)

    if(x[i,4] == start_val)
    {
      cases_c = cases_c + cases
      res = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]), cases_c))
      start_val = x[i,4]
    } else {
       cases_c = cases
       res = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]), cases_c))
       start_val = x[i,4]
    }
	}

	colnames(res) = c("L95","U95","N Affected Individuals")
	x = cbind(x,res)

	# Make plot function here

	return(x)
}

CumulativeIncidence_familial_withinDisorder_byYOB = function(survival_data, earliest_onset, latest_onset, nFamMember) {
	survival_data$born_at = as.numeric(format(survival_data$born_at,'%Y'))

  ####

	letter = strsplit(nFamMember,"")[[1]][1]
	number = paste(strsplit(nFamMember,"")[[1]][2:length(strsplit(nFamMember,"")[[1]])], collapse="")

	if(letter == "e")
	{
		survival_data = survival_data[survival_data$diagnosed_relatives == number, c(1:3,5,4,6,7)]
	} else if(letter == "s"){
		survival_data = survival_data[survival_data$diagnosed_relatives <= number, c(1:3,5,4,6,7)]
	} else if(letter == "l"){
		survival_data = survival_data[survival_data$diagnosed_relatives >= number, c(1:3,5,4,6,7)]
	}

	if(nrow(survival_data) == 0)
	{
		print("No individuals left after filtering. Returning NULL")
		return(NULL)
	}

	# Having any affected family member given that you have at least 1 family member
	anygroup = survival_data[survival_data$diagnosed_relatives > 0,]
	anygroup$diagnosed_relatives = "Any"
	cuminc_data = rbind(anygroup,survival_data)
	rm(anygroup)


	# Having no affected family member because you don't have at least 1 family member
	nonegroup = survival_data[survival_data$relatives == 0,]
	nonegroup$diagnosed_relatives = "NoFamilyMembers"
	cuminc_data = rbind(cuminc_data,nonegroup)
	rm(nonegroup)

	n_familymembers <- survival_data |>
                                      summarise(max_diagnosed_relatives = max(diagnosed_relatives)) |>
                                      pull(max_diagnosed_relatives)

	final = NULL
	# Loop though classes to check if consuring column is multiple options. If not delete group
	for(nrFAM in c("Any","NoFamilyMembers",0:n_familymembers))
	{
		for(year in c("all", sort(unique(cuminc_data$born_at))))
		{
			#cat("Number of affected family member:",nrFAM, "& Year or birth:",year, "\n")

			if(year == "all") # all years
			{
				dat = cuminc_data[cuminc_data$diagnosed_relatives == nrFAM,]
			} else {
				dat = cuminc_data[cuminc_data$born_at == year & cuminc_data$diagnosed_relatives == nrFAM,]
			}

      # When there are only censored individuals, cuminc will fail with the internal error:
      # "Error in `cuminc(ftime = dat$failure_time, fstatus = dat$failure_status, # cencode = 0)`:
      # NAs in foreign function call (arg 3)".
      # Therefor we need to
      if (nrow(dat[dat$failure_status != 0,]) == 0) {
        next
      }

			# Calculate Cumulative Incidence using competing risk
			# Some error occured when I tries it out using everybody in one go, but when running the data one by one it seems to work.
			# While the group statement is redundant I use it later on to keep the group names
			cum1 = cuminc(
							ftime 	= dat$failure_time,
							fstatus = dat$failure_status,
							cencode = 0
						 )

      if (is.null(cum1$`1 1`)) {
        next
      }

      x = cbind(data.frame(cum1$`1 1`), nrFAM)
			# Remove TTE below and above hreshold
			x = x[(x[,1] >= earliest_onset & x[,1] <= latest_onset),]
			x = data.frame(x |> group_by(time) |> top_n(-1, est))
      cuminc_data$born_at = as.character(cuminc_data$born_at)

      if (nrow(x) == 0) {
        next
      }

			res = NULL
      cases_c = 0

			# Calculate upper and lower conficence interval
			# controls are defined as: everybody regardless of disorder status that have a follow-up time larger than a given time point e.g., everybody with a follow up time larger than 20
			for(i in 1:nrow(x))
			{
        if(year == "all")
        {
          cases = sum(cuminc_data[cuminc_data[,2] == x[i,1] & cuminc_data$diagnosed_relatives == x[i,4],3] == 1)
        } else{
          cases = sum(cuminc_data[cuminc_data[,2] == x[i,1] & cuminc_data$diagnosed_relatives == x[i,4] & cuminc_data$born_at == year,3] == 1)
        }
        cases_c = cases_c + cases

				res   = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]), year, cases_c))
			}

			final = rbind(final,cbind(x,res))
		}
	}

	if(is.null(final))
	{
		print("No CIF could be calculated. Returning NULL")
		return(NULL)
	}

  colnames(final) = c("Time", "Estimate", "Variance", "N Affected Family Members", "L95","U95","Year","N Affected Individuals")

  for(col in c(1:3,5:6))
  {
    final[,col] = as.numeric(final[,col])
  }

  return(final)

	#test = list()

	#counter2=1
	#for(sel in unique(final$`N Affected Family Members`))
	#{
	#	counter1=2
	#	mat = cbind(unique(final$Time))
	#	colnames(mat)= "Time"

	#	for(year in c("all", sort(unique(cuminc_data$born_at))))
	#	{
  #    mat = merge(mat, final[final$Year == year & final$`N Affected Family Members` == sel,c(1,2)], by="Time", all.x=T)
  #    colnames(mat)[counter1] = year
  #    counter1 = counter1 + 1
	#	}
	#	colnames(mat)[1] = "Age"
	#	test[[counter2]] = mat
	#	counter2 = counter2 + 1
	#}

  #                                      #final <- append(final,test)
	#final = c(list(final),test)
  #names(final) = c("Combined", unique(final[[1]]$`N Affected Family Members`))

	#return(final)
}

CumulativeIncidence_familial_betweenDisorder = function(
  survival_data,
  earliest_onset,
  latest_onset,
  earliest_onset_target,
  latest_onset_target,
  nFamMember
) {

	survival_data$born_at = as.numeric(format(survival_data$born_at,'%Y'))
  ####

  letter = strsplit(nFamMember,"")[[1]][1]
  number = paste(strsplit(nFamMember,"")[[1]][2:length(strsplit(nFamMember,"")[[1]])], collapse="")

  if(letter == "e")
  {
    survival_data = survival_data[survival_data$diagnosed_relatives == number, c(1:3,5,4)]
  } else if(letter == "s"){
    survival_data = survival_data[survival_data$diagnosed_relatives <= number, c(1:3,5,4)]
  } else if(letter == "l"){
    survival_data = survival_data[survival_data$diagnosed_relatives >= number, c(1:3,5,4)]
  }

  if(nrow(survival_data) == 0)
  {
    print("No individuals left after filtering. Returning NULL")
    return(NULL)
    exit
  }

  anygroup = survival_data[survival_data$diagnosed_relatives > 0,]
  anygroup$diagnosed_relatives = "Any"
  cuminc_data = rbind(anygroup,survival_data)
  rm(anygroup)

 	nonegroup = survival_data[survival_data$relatives == 0,]
	nonegroup$diagnosed_relatives = "NoFamilyMembers"
	cuminc_data = rbind(cuminc_data,nonegroup)
	rm(nonegroup)

  n_familymembers <- survival_data |>
                                      summarise(max_diagnosed_relatives = max(diagnosed_relatives)) |>
                                      pull(max_diagnosed_relatives)

	x = NULL
	# Loop though classes to check if consuring column is multiple options. If not delete group
	for(i in c("Any", "NoFamilyMembers", 0:n_familymembers))
	{
    dat <- cuminc_data[cuminc_data$diagnosed_relatives == i,]

    # When there are only censored individuals, cuminc will fail with the internal error:
    # "Error in `cuminc(ftime = dat$failure_time, fstatus = dat$failure_status, # cencode = 0)`:
    # NAs in foreign function call (arg 3)".
    # Therefor we need to
    if (nrow(dat[dat$failure_status != 0,]) == 0) {
			cat("Group",i, "Affected Family Members: either did not excist or contained no censured individuals and was therefore removed", "\n")
      next
    }
		# Calculate Cumulative Incidence using competing risk
		# Some error occured when I tries it out using everybody in one go, but when running the data one by one it seems to work.
    # While the group statement is redundant I use it later on to keep the group names
		cum1 = cuminc(
                    ftime = cuminc_data[cuminc_data$diagnosed_relatives == i,]$failure_time,
                    fstatus = cuminc_data[cuminc_data$diagnosed_relatives == i,]$failure_status,
                    #group=cuminc_data[cuminc_data$diagnosed_relatives == i,]$diagnosed_relatives,
                    cencode = 0
                  )

    if (is.null(cum1$`1 1`)) {
      next
    }

    sel = cbind(data.frame(cum1$`1 1`), i)
    # Remove TTE below and above hreshold
	  sel = sel[(sel[,1] >= earliest_onset_target & sel[,1] <= latest_onset_target),]
    sel = data.frame(sel |> group_by(time) |> top_n(-1, est))

		x = rbind(x, sel)
	}

	colnames(x) = c("Time","Estimate","Variance","N Affected Family Members")

	res = NULL
  #ctrls_c = 0
  cases_c = 0
  start_val = x[1,4]

	# Calculate upper and lower conficence interval and Ns
	for(i in 1:nrow(x))
	{
    #ctrls = sum(cuminc_data[cuminc_data[,2] == x[i,1] & cuminc_data[,5] == x[i,4] ,3] == 0)
    cases = sum(cuminc_data[cuminc_data$failure_time == x[i,1] & cuminc_data$diagnosed_relatives == x[i,4] ,3] == 1)

    if(x[i,4] == start_val)
    {
      cases_c = cases_c + cases
      res = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]), cases_c))
      start_val = x[i,4]
    } else {
       cases_c = cases
       res = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]), cases_c))
       start_val = x[i,4]
    }
	}

	colnames(res) = c("L95","U95","N Affected Individuals")
	x = cbind(x,res)

	return(x)
}

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
          sprintf("(diagnosis_kind IN (%s) OR diagnosis_kind IS NULL)", diagnosis_kind)
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

h2.calculation = function(K1,Kr,A1,Ar,ar=1/2)
{
	# h2 estimate
	T1   = qnorm(K1, lower.tail= FALSE) # lifetime prevalence unaffected/general population represeting the upper tail z value
	y    = dnorm(T1)
	i    = y/K1
	Tr   = qnorm(Kr, lower.tail= FALSE) # lifetime prevalence in the relatives of the affected ones represeting the upper tail z value
	yr   = dnorm(Tr)

	num  = T1-Tr * sqrt(1 - (1 - T1/i) * (T1^2 -Tr^2))
	den  = ar * (i + (i-T1)*Tr^2)
	h2   = num/den
	# se estimation
	Wg   = (((K1^2)/(y^2)) * (1-K1)) / A1
	vvg  = (1/i - ar*h2*(i-T1))^2 # there is a + in Wray and a - in Falconer
	Wr   = Kr^2/yr^2 * (1-Kr) / Ar
	vvr  = (1/i)^2

	se   = 1/ar * sqrt(vvg * Wg + vvr * Wr)
	ci.l = h2 - 1.96 * se
	ci.u = h2 + 1.96 * se
	output = rbind(c(h2, se, ci.l, ci.u))

	colnames(output) = c("h2", "se", "L95", "U95")
	return(output)
}

dk_h2byYOB = function(phenotype_ICDcode, survival_data, earliest_onset, latest_onset, nFamMember, relationship_kind, effect)
{
  family_type <- relationship_kind
	baseline    <- "Genpop"
	# Get family type database with relationship coefficient
	types = c("FS","PO","HS","mHS","pHS","Av","1G","1C")
	coefs = c(0.5,0.5,0.25,0.25,0.25,0.25,0.25,0.125)
	dat = cbind(types,coefs)

	# h2 for ADHD, ANO and ASD can only be calculated using FS in the iPSYCH window given that it is only and ICD10 disorder
	# Calculate familial risk for disorder more than 0 affected family members
	fam = CumulativeIncidence_familial_withinDisorder_byYOB(survival_data, earliest_onset, latest_onset, nFamMember)
  gp  = CumulativeIncidence_GenPop_byYOB(survival_data, earliest_onset, latest_onset)
  gp  = gp[[1]] |>
    mutate(`N Affected Family Members` = baseline) |>
    relocate(`N Affected Family Members`, .after = Variance) |>
    relocate(Year, .after = U95) |>
    relocate(`N Affected Individuals`, .after = Year) |>
    as.data.frame()

	fam = rbind(gp, fam) |>
    filter(
      Year != "all",
      `N Affected Family Members` %in% list(baseline, "Any")
    ) |>
    as.data.frame()

	# Get relationship coefficient based on the type of family relationship
	relatedness = as.numeric(dat[dat[,1] %in% family_type,2])
	cat("Requested Baseline for h2 calculation:\t ", baseline, "\n")
	cat("Relationship coefficient:\t\t ", relatedness, "\n")
  cat("Requested meta-analysis:\t\t ", effect, "\n")

	h2_output = NULL
	for(year in unique(fam$Year))
	{
		genpop = fam[fam[,4] == baseline & fam[,7] == year,]
		family = fam[fam[,4] == "Any" & fam[,7] == year,]

		if(nrow(family) == 0 | nrow(genpop) == 0)
		{
			next
		}

    timeObserved = table(c(
                						unique(genpop[genpop$Year == year,"Time"]),
                						unique(family[family$Year == year,"Time"])
						              ))

    lastFam = max(
      as.numeric( # Get the largest value of the column names
        names( # Get column names, as the failure time values are used as column names
          timeObserved[timeObserved==2] # Keep failure times observed twice (once in each cohort)
        )
      )
    )

		h2 = h2.calculation	(
			as.numeric(genpop[genpop[,1] == lastFam,2]),
			as.numeric(family[family[,1] == lastFam,2]),
			as.numeric(genpop[genpop[,1] == lastFam,8]),
			as.numeric(family[family[,1] == lastFam,8]),
			ar=relatedness
		)

		h2_output = rbind(h2_output, c(year,h2))
	}

	colnames(h2_output) = c("Year","h2","se", "L95", "U95")
	h2_output = data.frame(h2_output)

	for(i in 2:ncol(h2_output))
	{
		h2_output[,i] = as.numeric(h2_output[,i])
	}

	meta = h2_output[1,]

	if(effect == "fixed")
	{
    wkfixed=(1 / (h2_output[which(h2_output[,1] != "all"),3]^2))
		metah2fixed=sum(h2_output[which(h2_output[,1] != "all"),2]*wkfixed,na.rm=T)/sum(wkfixed,na.rm=T)
		metaSEfixed=sqrt((1 / sum(wkfixed,na.rm=T)))
		meta[1,1]   = "Meta_fixed"
		meta[1,2]   = metah2fixed
		meta[1,3]   = metaSEfixed
		meta[1,4]   = metah2fixed - 1.96*metaSEfixed
		meta[1,5]   = metah2fixed + 1.96*metaSEfixed
	} else if (effect == "random") {
			wkstar = (1 / ((h2_output[which(h2_output[,1] != "all"),3]^2) + var(h2_output[which(h2_output[,1] != "all"),2], na.rm=T)))
			metaRrandom  = sum(h2_output[which(h2_output[,1] != "all"),2]*wkstar,na.rm=T)/sum(wkstar,na.rm=T)
			metaSErandom=sqrt((1 / sum(wkstar,na.rm=T)))
			meta[1,1]   = "Meta_random"
			meta[1,2]   = metaRrandom
			meta[1,3]   = metaSErandom
			meta[1,4]   = metaRrandom - 1.96*metaSErandom
			meta[1,5]   = metaRrandom + 1.96*metaSErandom
	}

	h2_output = rbind(h2_output,meta)

	if(!(baseline == "Genpop"))
	{
		baseline = paste(baseline,"affected",family_type)
	}

	h2_output = cbind(paste(phenotype_ICDcode,collapse="/"), baseline,paste("Any affected",family_type), h2_output)

	colnames(h2_output) = c("ICD Codes", "Baseline risk population", "Familial risk population", "Year","h2", "Variance", "L95","U95")

	return(h2_output)
}

rhog.calculation <- function(Kc, Krc, Kf, Ac, Arc, Af, h2c,h2f,ar=1/2)
{
  Tc <- qnorm(Kc, lower.tail= FALSE)
  yc <- dnorm(Tc)
  Trc <- qnorm(Krc, lower.tail= FALSE)
  yrc <- dnorm(Trc)
  Tf <- qnorm(Kf, lower.tail= FALSE)
  yf <- dnorm(Tf)

  i <- yf/Kf
  num <- Tc-Trc * sqrt(1 - (1 - Tf/i) * (Tc^2 -Trc^2))
  den <- ar * (i + (i-Tf)*Trc^2)
  rhh <- num/den
  rhog <- rhh/sqrt(h2f*h2c)
  # se estimation
  Wg <- Kf^2/yf^2 * (1-Kf) / Af
  vvg <- (1/i - ar*rhh*(i-Tf))^2 # there is a + in Wray and a - in Falconer
  Wr <- Krc^2/yrc^2 *(1-Krc) / Arc + Kc^2/yc^2 * (1-Kc) / Ac
  vvr <- (1/i)^2
  se <- 1/ar * sqrt(vvg * Wg + vvr * Wr)
  ci.l <- rhh - 1.96 * se
  ci.u <- rhh + 1.96 * se
  ci.l.r <- ci.l/sqrt(h2f*h2c)
  ci.u.r <- ci.u/sqrt(h2f*h2c)
  result = c(rhh, rhog, se, ci.l, ci.u, ci.l.r, ci.u.r)
  names(result) = c("rhh","rhog", "SE","U95","L95","U95_h2","L95_h2")
  return(result)
}

dk_rg_byYOB = function(survival_data, d1_earliest_onset, d2_earliest_onset, nFamMember, relationship_kind, effect)
{
  # column order:
  # person_id failure_time failure_status relatives diagnosed_relative born_at

  d1_tte <- survival_data |>
    select(person_id, d1_failure_time, d1_failure_status, relatives, d1_diagnosed_relatives, born_at) |>
    rename(
      failure_time        = d1_failure_time,
      failure_status      = d1_failure_status,
      diagnosed_relatives = d1_diagnosed_relatives
    ) |>
    as.data.frame()

  d2_tte <- survival_data |>
    select(person_id, d2_failure_time, d2_failure_status, relatives, d2_diagnosed_relatives, born_at) |>
    rename(
      failure_time        = d2_failure_time,
      failure_status      = d2_failure_status,
      diagnosed_relatives = d2_diagnosed_relatives
    ) |>
    as.data.frame()

  d1_d2_tte <- survival_data |>
    select(person_id, d1_failure_time, d1_failure_status, relatives, d2_diagnosed_relatives, born_at) |>
    rename(
      failure_time        = d1_failure_time,
      failure_status      = d1_failure_status,
      diagnosed_relatives = d2_diagnosed_relatives
    ) |>
    as.data.frame()

  h2 <- "meta"
  effect <- "fixed"
  phenotype_ICDcode_target <- "d1"
  phenotype_ICDcode_targetfamily <- "d2"

	types = c("FS","PO","HS","mHS","pHS","AV","1G","1C")
	coefs = c(0.5,0.5,0.25,0.25,0.25,0.25,0.25,0.125)
	dat = cbind	(types,coefs)
  family_type <- relationship_kind

  # h1_d1
  outphen1 = dk_h2byYOB("d1", d1_tte, d1_earliest_onset, 100, nFamMember, relationship_kind, effect) |>
    filter_all(
      all_vars(!is.infinite(.) & !is.na(.))
    )
  # h1_d2
  outphen2 = dk_h2byYOB("d2", d2_tte, d2_earliest_onset, 100, nFamMember, relationship_kind, effect) |>
    filter_all(
      all_vars(!is.infinite(.) & !is.na(.))
    )

	#re_d1_c1 = CumulativeIncidence_GenPop_byYOB(d1_tte, earliest_onset = d1_earliest_onset, latest_onset = 100)
	genpop_phen1 = CumulativeIncidence_GenPop_byYOB(d1_tte, d1_earliest_onset, 100)

  # re_d1_c3 = CumulativeIncidence_familial_withinDisorder_byYOB(
  betweendisorder_phen1_phen2 = CumulativeIncidence_familial_withinDisorder_byYOB(d1_d2_tte, d1_earliest_onset, 100, nFamMember)

	#re_d2_c1 = CumulativeIncidence_GenPop_byYOB(d2_tte, earliest_onset = d2_earliest_onset, latest_onset = 100)
  genpop_phen2 = CumulativeIncidence_GenPop_byYOB(d2_tte, d2_earliest_onset, 100)

	#betweendisorder_phen1_phen2 = betweendisorder_phen1_phen2[[1]][betweendisorder_phen1_phen2[[1]][,4] == "Any",]
	betweendisorder_phen1_phen2 = betweendisorder_phen1_phen2[betweendisorder_phen1_phen2[,4] == "Any",]

  #write.table(betweendisorder_phen1_phen2, paste("/home/jmei/h2_rg/results/CVD_MH_project/data/data_fullPOP/CIF_CROSS_",phenotype1,"_",phenotype2), col.names=T, row.names=F)
	genpop_phen1 = genpop_phen1[[1]]
	genpop_phen2 = genpop_phen2[[1]]

	rg=NULL
	for(year in unique(genpop_phen1[,6]))
	{
		checker=length(betweendisorder_phen1_phen2[betweendisorder_phen1_phen2[,4] == "Any" & betweendisorder_phen1_phen2[,7] == year,2])!=0 &
            length(genpop_phen2[genpop_phen2[,6] == year,2]) != 0  &
            length(genpop_phen1[genpop_phen1[,6] == year,2]) != 0

		h2outphen1 = as.numeric(outphen1[outphen1[,4] == year,5])
		h2outphen2 = as.numeric(outphen2[outphen2[,4] == year,5])

		if(checker == TRUE & length(h2outphen1) == 1 & length(h2outphen2) == 1)
		{
			relatedness = as.numeric(dat[dat[,1] %in% family_type,2])
      timeObserved = table(c(unique(genpop_phen1[genpop_phen1$Year == year,"Time"]),unique(genpop_phen2[genpop_phen2$Year == year,"Time"]),unique(betweendisorder_phen1_phen2[betweendisorder_phen1_phen2$Year == year,"Time"])))
      if(3 %in% unname(timeObserved) == TRUE)
      {
        #selectedTime = as.numeric(max(names(timeObserved[timeObserved==3])))
        selectedTime = max(as.numeric(names(timeObserved[timeObserved==3])))
        rg.psych.cvd = rhog.calculation(
                        									genpop_phen1[genpop_phen1[,6] == year & genpop_phen1[,1] == selectedTime,2], #1 lifetime prevalence phen1
                        									betweendisorder_phen1_phen2[betweendisorder_phen1_phen2[,4] == "Any" & betweendisorder_phen1_phen2[,7] == year & betweendisorder_phen1_phen2[,1] == selectedTime,2],
                        									genpop_phen2[genpop_phen2[,6] == year & genpop_phen2[,1] == selectedTime,2],
                        									as.numeric(genpop_phen1[genpop_phen1[,6] == year & genpop_phen1[,1] == selectedTime,7]),
                        									as.numeric(betweendisorder_phen1_phen2[betweendisorder_phen1_phen2[,4] == "Any" & betweendisorder_phen1_phen2[,7] == year & betweendisorder_phen1_phen2[,1] == selectedTime,8]),
                        									as.numeric(genpop_phen2[genpop_phen2[,6] == year & genpop_phen2[,1] == selectedTime,7]),
                        									h2outphen1,			#7 h2 phen1
                        									h2outphen2,			#8 h2 phen2
                        									ar=relatedness		#9 relatedness e.g., 0.5
								                        )

      } else{
              rg.psych.cvd = matrix(ncol=7, nrow=1,NA)
			        colnames(rg.psych.cvd) = c("rhh", "rhog", "se", "L95", "U95", "L95_h2", "U95_h2")
      }
		} else {
			rg.psych.cvd = matrix(ncol=7, nrow=1,NA)
			colnames(rg.psych.cvd) = c("rhh", "rhog", "se", "L95", "U95", "L95_h2", "U95_h2")
		}

		rg.psych.cvd = cbind(paste(phenotype_ICDcode_target,collapse="_"), paste(phenotype_ICDcode_targetfamily,collapse="_"), family_type,year, rbind(rg.psych.cvd))
		colnames(rg.psych.cvd) = c("Phenotype1", "Phenotype2", "Relatedness", "Year","rhh", "rhog", "se", "L95", "U95", "L95_h2", "U95_h2")
		rownames(rg.psych.cvd) = NULL
		rg = rbind(rg,rg.psych.cvd)
	}


	rg = data.frame(rg)

	for(i in 5:11)
	{
		rg[,i] = as.numeric(rg[,i])
    rg[grepl("NaN|-Inf|Inf", rg[,i]),i] = NA
	}

  if(any(is.na(rg)) == TRUE)
  {
     rg[which(is.na(rowSums(rg[,5:ncol(rg)],))),5:ncol(rg)] = NA
  }

	meta = rg[1:2,]
  h2w = sqrt((as.numeric(outphen1[grepl("Meta",outphen1[,4]),5]))*(as.numeric(outphen2[grepl("Meta",outphen2[,4]),5])))

	wk=(1 / (rg[which(rg[,4] != "all"),7]^2))
	metaRfixed=sum(rg[which(rg[,4] != "all"),5]*wk,na.rm=T)/sum(wk,na.rm=T)

	wkstar = (1 / ((rg[which(rg[,4] != "all"),7]^2) + var(rg[which(rg[,4] != "all"),5], na.rm=T)))
	metaRrandom  = sum(rg[which(rg[,4] != "all"),5]*wkstar,na.rm=T)/sum(wkstar,na.rm=T)

	metaRgfixed=metaRfixed/h2w
	metaRgrandom=metaRrandom/h2w

	metaSEfixed=sqrt((1 / sum(wk,na.rm=T)))
	metaSErandom=sqrt((1 / sum(wkstar,na.rm=T)))

	meta[1,4]   = "Meta_fixed"
	meta[1,5]   = metaRfixed
	meta[1,6]   = metaRgfixed
	meta[1,7]   = metaSEfixed
	meta[1,8]   = metaRfixed - 1.96*metaSEfixed
	meta[1,9]   = metaRfixed + 1.96*metaSEfixed
	meta[1,10]  = (metaRfixed - (1.96*metaSEfixed)) / h2w
	meta[1,11]  = (metaRfixed + (1.96*metaSEfixed)) / h2w


	meta[2,4]   = "Meta_random"
	meta[2,5]   = metaRrandom
	meta[2,6]   = metaRgrandom
	meta[2,7]   = metaSErandom
	meta[2,8]   = metaRrandom - 1.96*metaSErandom
	meta[2,9]   = metaRrandom + 1.96*metaSErandom
	meta[2,10]  = (metaRrandom - (1.96*metaSErandom)) / h2w
	meta[2,11]  = (metaRrandom + (1.96*metaSErandom)) / h2w

	rg = rbind(rg,meta)

	return(rg)
}
