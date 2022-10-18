# Author: 					  	Joeri Meijsen
# Date:							    X-JUN-2021
# Function Name: 		  	Danish_Register_CumulativeIncidence_GenPop
#
#								        IbpRegistryClient
#								        dplyr
# Libraries needed:		  DBI
#								        RPostgres
#								        ggplot2
#								        cmprsk
#
# Description: 				    Function to estimate lifetime risk (taking censuring into censuring) in the general populatin using the Danish Register Data.
#
# Input Options:
#
#		phenotype_ICDcode:	  Individual ICD code or vector of multiple ICD codes (ICD8 or ICD10).
#								          Note: Codes are matched using a grepl(^) e.g., ^F30 so no mid string matches are made but short codes such as F3 will lead to the selection of F30,F31 etc etc
#										      Default = NULL
#
#		sex:					        Define sex of individuals to be compared
#										        both  	- 	Males and Females
#									    	    male  	- 	Male
#					Options are:	    female  - 	Females
#										        Default = 'both'
#
#		birth_date_min: 		  Minimum birth date of individuals in YYYY-MM-DD format. For Example:	'1989-10-12'
#										        Default = NULL
#
#		birth_date_max: 		  Maximum birth date of individuals in YYYY-MM-DD format. For Example:	'2000-01-01'
#									    	    Default = NULL
#
#		diagnosis_type:			  How is the diagnosis recorded in the system. Give as e.g., 'basic' for single type or as vector e.g., c('basic', 'main')
#										        main
#									          auxiliary
#					Options are:	    basic
#										        referral
#									    	    temporary
#									    	    complication
#									      	  associated
#									    	    ALL
#									    	    Default = c('main','auxiliary')
#
#		register:				      Which registers does one want. npr, pcrr, or both
#										        npr  - National Patient Register
#					Options are:	  	pcrr - Psychiatric Central Research Register
#										        ALL
#										        Default = 'ALL'
#
#		residential_status:	  Which residential status should be used.
#										        danish-resident
#									        	danish-resident-special-address
#										        greenlandic-resident
#					Options are:		  greenlandic-resident-special-address
#									        	emigrated
#										        dead
#										        ALL
#									        	Default = c('danish-resident', 'danish-resident-special-address','emigrated', 'dead')
#
#		earliest_onset:			  Remove individuals with a onset before this time point.
#										        Default = 0
#
#		latest_onset:			    Remove individuals with a onset after this age.
#										        Default = 100
#
#  study_end_at:          End observation period
#                          Default = NULL
#
#		plot_data:				    Provide path to location where plots will be stored.
#										        Default = NULL
#
# Outcome:
#
#
# Open environment:
# conda activate IbpRegistryEnv
#
# Open R:
# R
#
# Read in function:
# source("Cumulative_Incidence_genPOP.r")
#
# Example code how to run
# genPOPtest = Danish_Register_CumulativeIncidence_GenPop(phenotype_ICDcode="F20", sex = "both", birth_date_min="1967-01-01", birth_date_max="2016-12-31", earliest_onset= 10, plot_data="/home/jmei/plots/")
#
Danish_Register_CumulativeIncidence_GenPop = function(data_path, phenotype_ICDcode=NULL, study_end_at=NULL, birth_date_min=NULL, birth_date_max=NULL, latest_onset=100, sex = "both", residential_status=c('danish-resident', 'danish-resident-special-address','emigrated', 'dead'), earliest_onset=1, diagnosis_type=c('main','auxiliary'), register="ALL")
{

	############################################################################
	############################# libraries needed #############################
	############################################################################

  library(data.table)
	library(dplyr, warn.conflicts = FALSE)
  library(stringr)
	#library(ggplot2)
	library(cmprsk)

	############################################################################
	################################ databases #################################
	############################################################################

	# Database of available tables
	# birth.med, res.records, cause.death, and gen.phen are not needed for this function, but could be added.
	med.records		=	  "medical.records"
	med.diag 		  = 	"medical.diagnoses"
	gen.phen.inc 	= 	"genetics.phenotype_includes"
	genea.rel 		= 	"genealogy.relationships"
	civil.set 		=	  "civil.people"
	birt.med 	  	=	  "medical.birth_metrics"
	res.records		=	  "residential.records"
	cause.death		=	  "medical.death_causes"
	gen.phen		  = 	"genetics.phenotypes"

	# Diagnosis types library
	diagnosis_db	=	c('main',
      						  'auxiliary',
      						  'associated',
      						  'basic',
      						  'referral',
      						  'temporary',
      						  'complication'
      						  )

	# Registers library
	register_db	=	c('npr',
      					  'pcrr'
      					  )

	# Residency library
	residential_db = c('danish-resident',
        					   'danish-resident-special-address',
        					   'greenlandic-resident',
        					   'greenlandic-resident-special-address',
        					   'emigrated',
        					   'dead'
        					   )

	############################################################################
	############################### Input Checks ###############################
	############################################################################

	# Break if no ICD code is provided
	if(is.null(phenotype_ICDcode))
	{
		stop("Error: No ICD code provided")
 	}

	# Set birthday min if not present select all
	if(!is.null(birth_date_min))
	{
		if(!is.na(as.Date(birth_date_min,"%Y-%m-%d")) == FALSE)
		{
			stop("Error: Format of minimum date of birth is not correct. Please use YYYY-MM-DD format")
		}
  }

  if(!is.null(study_end_at))
	{
    if(!is.na(as.Date(study_end_at,"%Y-%m-%d")) == FALSE)
		{
			stop("Error: Format of end of study is not correct. Please use YYYY-MM-DD format")
		} else if(study_end_at < birth_date_max) {
      stop("Error: study_end_at cannot be smaller than birth_date_max")
    }
	} else {
    study_end_at = as.Date(birth_date_max,"%Y-%m-%d")+1
  }
  
	# Do sex check
  if (!is.null(sex)) {
    if (!grepl(sex, "both|male|female")) {
      stop("Error: This sex does not excist. Please use 'male', 'female', or 'both' ")
    } else if (sex == "both") {
      sex = c('male', 'female')
    }
  }
  # NULL means that both genders will be used.

	# Set birthday max which is also the end of ascertainment-observation threshold if not present select all
  if(!is.null(birth_date_max))
  {
	  if(!is.na(as.Date(birth_date_max,"%Y-%m-%d")) == FALSE)
		{
			stop("Error: Format of minimum date of birth is not correct. Please use YYYY-MM-DD format")
		}
  }

	if(!is.null(birth_date_min) && !is.null(birth_date_max) && birth_date_min > birth_date_max)
	{
		print("Error: Flipping birth_date_min and birth_date_max as provided birth_date_min > birth_date_max")
		tmp=birth_date_min
		birth_date_min = birth_date_max
		birth_date_max = tmp
		rm(tmp)
	}

	# If user wants all residential_status options
	if("ALL" %in% residential_status & !is.null(residential_status))
	{
		residential_status = residential_db
	}

	# Check if residential status options are correct
	if(sum(residential_status %in% residential_db) != length(residential_status))
	{
		stop("Error: One or more of the provided types of residential status are not found in the library.")
	}

	if("ALL" %in% diagnosis_type & !is.null(diagnosis_type))
	{
		diagnosis_type = diagnosis_db
	}

	# Check if diagnosis options are correct
	if(sum(diagnosis_type %in% diagnosis_db) != length(diagnosis_type))
	{
		stop("Error: One or more of the provided types of diagnosis are not found in the library.")
	}

	if(register == "ALL" & !is.null(register))
	{
		register = register_db
	}

	# Check if register options are correct
	if(sum(register %in% register_db) != length(register))
	{
		stop("Error: One or more of the provided types of registers are not found in the library.")
	}

	# Check if earliest_onset is correct
	if(earliest_onset < 1 | is.na(earliest_onset))
	{
		stop("Error: Earliest age of onset cannot be smaller than 1 or null. Default is 1.")
	}

	# Check if earliest_onset is correct
	if(latest_onset < 1 | is.na(latest_onset))
	{
		stop("Error: Maximum age cannot be smaller than 1 or null. Default is 100.")
	}

	# print all input variables
	cat("\n")
	cat("Requested ICD Code(s):\t\t ", paste(phenotype_ICDcode,collapse=", "), "\n")
	cat("Requested Ascertainment Window:\t ", paste(birth_date_min, " to ",  birth_date_max, " with end of observation set at ", study_end_at, sep=""), "\n")
	cat("Requested Residential Status(es):", paste(residential_status,collapse=", "), "\n")
	cat("Requested Diagnosis Type(s):\t ", paste(diagnosis_type,collapse=", "), "\n")
	cat("Requested Diagnosis Register(s): ", paste(register,collapse=", "), "\n")
	cat("Requested Earliest Age of Onset: ", earliest_onset, "\n")
	cat("Requested Latest Age of Onset:\t ", latest_onset, "\n")
	cat("Requested Sex:\t\t\t ", paste(sex,collapse=", "), "\n")

	############################################################################
	############################# Functional part ##############################
	############################################################################

  if (file.exists(data_path)) {
    print(sprintf("Info: Data file '%s' already exists, will not run SQL query to generate it", data_path))
  } else {
    icd_regexp <- paste("^", phenotype_ICDcode, sep="", collapse="|")

    generator <- QueryGenerator$new()

    survival_query <- generator$survival_by_icd_codes(
      icd_codes_regexp=icd_regexp,
      study_end_at=study_end_at,
      birth_date_min=birth_date_min,
      birth_date_max=birth_date_max,
      earliest_onset=earliest_onset,
      latest_onset=latest_onset,
      gender=sex,
      status=residential_status,
      diagnosis_kind=diagnosis_type,
      record_origin=register
    )
    
    err_code <- generator$execute_query(
                survival_query,
                data_path,
                "localhost"
                )

    #writeLines(survival_query)

    if (err_code != 0) {
	  	stop("Error: Failured to execute SQL query using psql")
    }

    print(sprintf("Info: SQL query successfully executed with results written to '%s'", data_path))
  }

  cuminc_data <- fread(
    file=data_path,
    sep=",",
    header=TRUE,
    encoding="UTF-8",
    data.table=TRUE
  )

#  See example file genPOP_example.txt for an example of the cuminc_data

  cuminc_data = data.frame(cuminc_data)
  # Retain only diagnosis status and time to event
  cuminc_data = cuminc_data[,c('failure_status', 'failure_time')]

	# Calculate Cumulative Incidence using competing risk
	cum1 = cuminc(ftime = cuminc_data$failure_time, fstatus = cuminc_data$failure_status, cencode = 0)

  # Make new matrix of cuminc function for 1 group
	x = data.frame(Time=cum1$`1 1`$time, Estimate=cum1$`1 1`$est,Variance=cum1$`1 1`$var)
	# Remove TTE below and above threshold
	x = x[(x[,1] >= earliest_onset & x[,1] <= latest_onset),]

  # Remove duplicated time to even estimates
  #x = x[seq(1,nrow(x),2),]
  x = data.frame(x %>% group_by(Time) %>% top_n(-1, Estimate))

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
