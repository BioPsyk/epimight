# Author: 						Joeri Meijsen
# Date:							X-JUN-2021
# Function Name: 				Danish_Register_CumulativeIncidence_familial_betweenDisorder
#
#								IbpRegistryClient
#								dplyr
# Libraries needed:				DBI
#								RPostgres
#								ggplot2
#                cmprsk
#
# Description: 					Function to estimate lifetime risk (taking censuring into censuring) with genealogy information using the Danish Register Data.
#
# Input Options:
#
#  phenotype_ICDcode_target:		    Individual ICD code or vector of multiple ICD codes (ICD8 or ICD10) for target population
#										                Note: Codes are matched using a grepl(^) e.g., ^F30 so no mid string matches are made but short codes such as F3 will lead to the selection of F30,F31 etc etc
#									                  	Default = NULL
#
#  phenotype_ICDcode_targetfamily:  Individual ICD code or vector of multiple ICD codes (ICD8 or ICD10) for family members of the target population
#										                Note: Codes are matched using a grepl(^) e.g., ^F30 so no mid string matches are made but short codes such as F3 will lead to the selection of F30,F31 etc etc
#										                  Default = NULL
#
#		sex:							              Define sex of individuals to be compared
#										                  both  	- 	Males and Females
#										                  male  	- 	Male
#					Options are:		            female  - 	Females
#										                  Default = 'both'
#
#		family_type:					          Type of familial relationship.
#										                  PO		- 	Parent-Offspring
#					Options are:		            FS		- 	Full-Sibling
#										                  HS		- 	Half-Sibling
#
#		birth_date_min: 				        Minimum birth date of individuals in YYYY-MM-DD format. For Example:	'1989-10-12'
#									                  	Default = NULL
#
#		birth_date_max: 			        	Maximum birth date of individuals in YYYY-MM-DD format. For Example:	'2000-01-01'
#										                  Default = NULL
#
#		diagnosis_type:					        How is the diagnosis recorded in the system. Give as e.g., 'basic' for single type or as vector e.g., c('basic', 'main')
#										                  main
#										                  auxiliary
#	                    		            basic
#				Options are:                   referral
#										                  temporary
#										                  complication
#										                  associated
#									                  	Default = c('main','auxiliary')
#
#		register:						            Which registers does one want. npr, pcrr, or both
#										                  npr  - National Patient Register
#					Options are:		            pcrr - Psychiatric Central Research Register
#										                  ALL
#										                  Default = 'ALL'
#
#		residential_status:				      Which residential status should be used.
#									                  	danish-resident
#										                  danish-resident-special-address
#										                  greenlandic-resident
#	        Options are:			          greenlandic-resident-special-address
#					            		            emigrated
#										                  dead
#										                  Default = c('danish-resident', 'danish-resident-special-address','emigrated', 'dead')
#
#		earliest_onset_target:			    Remove individuals with a onset before this time point in the target population
#										                  Default = 0
#
#		earliest_onset_targetfamily:	  Remove individuals with a onset before this time point in family members of the target population
#										                  Default = 0
#
#		latest_onset_target:			      Remove individuals with a  onset after this age in the target population
#										                  Default = 100
#
#		latest_onset_targetfamily:		  Remove individuals with a  onset after this age in family members of the target population
#										                  Default = 100
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
#
#
Danish_Register_CumulativeIncidence_familial_betweenDisorder = function(data_path, phenotype_ICDcode_target=NULL, phenotype_ICDcode_targetfamily=NULL, study_end_at=NULL, birth_date_min=NULL, birth_date_max=NULL, sex = "both", residential_status=c('danish-resident', 'danish-resident-special-address','emigrated', 'dead'), earliest_onset_target=1, earliest_onset_targetfamily=1, latest_onset_target=100, latest_onset_targetfamily=100,diagnosis_type_target=c('main','auxiliary'),diagnosis_type_targetfamily=c('main','auxiliary'),register_target="ALL",register_targetfamily="ALL", family_type="FS", nFamMember = "l0")
{

	############################################################################
	############################# libraries needed #############################
	############################################################################

  library(data.table)
	library(dplyr, warn.conflicts = FALSE)
	#library(ggplot2)
	library(cmprsk)

	############################################################################
	################################ databases #################################
	############################################################################

	entire_window="1800-01-01"

	# Database of available tables
	# birth.med, res.records, cause.death, and gen.phen are not needed for this function, but could be added.
	med.records		=	  "medical.records"
	med.diag 		  = 	"medical.diagnoses"
	gen.phen.inc 	= 	"genetics.phenotype_includes"
	genea.rel 		= 	"genealogy.relationships"
	civil.set 		=	  "civil.people"
	birt.med 		  =	  "medical.birth_metrics"
	res.records		=	  "residential.records"
	cause.death		=	  "medical.death_causes"
	gen.phen		  = 	"genetics.phenotypes"

	# Diagnosis types library
	diagnosis_db	  =	  c('main',
          							'auxiliary',
          							'associated',
          							'basic',
          							'referral',
          							'temporary',
          							'complication'
                        )

	# Registers library
	register_db		  =	  c('npr',
          							'pcrr'
                        )

	# Residency library
	residential_db 	= 	c('danish-resident',
          							'danish-resident-special-address',
          							'greenlandic-resident',
          							'greenlandic-resident-special-address',
          							'emigrated',
          							'dead'
                        )

  horizontal_db = c('HS',
                    'pHS',
                    'mHS', 
                    'FS'
                    )
                    
  vertical_db = c("PO",
                  "1G",
                  "1C",
                  "Av"
                  )
  
	family_db 		= 	c(horizontal_db,
        							vertical_db
                      )
                      
#	horizontal_relationships = paste(horizontal_db,collapse="|")

	############################################################################
	############################### Input Checks ###############################
	############################################################################

	# Break if no ICD code is provided
	if(is.null(phenotype_ICDcode_target) | is.null(phenotype_ICDcode_targetfamily))
	{
		stop("Error: No ICD code provided in either phenotype_ICDcode_target or phenotype_ICDcode_targetfamily")
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
  
	# Set birthday min if not present select all
	if(!is.null(birth_date_min))
	{
		if(!is.na(as.Date(birth_date_min,"%Y-%m-%d")) == FALSE)
		{
			stop("Error: Format of minimum date of birth is not correct. Please use YYYY-MM-DD format")
		}
  }

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

	if(("ALL" %in% diagnosis_type_target & !is.null(diagnosis_type_target)) & ("ALL" %in% diagnosis_type_targetfamily & !is.null(diagnosis_type_targetfamily)))
	{
		diagnosis_type_target = diagnosis_db
		diagnosis_type_targetfamily = diagnosis_db
	} else if(("ALL" %in% diagnosis_type_target & !is.null(diagnosis_type_target)) & !("ALL" %in% diagnosis_type_targetfamily & !is.null(diagnosis_type_targetfamily))){
		diagnosis_type_target = diagnosis_db
	} else if(!("ALL" %in% diagnosis_type_target & !is.null(diagnosis_type_target)) & ("ALL" %in% diagnosis_type_targetfamily & !is.null(diagnosis_type_targetfamily))){
		diagnosis_type_targetfamily = diagnosis_db
	}

	# Check if diagnosis options are correct
	if((sum(diagnosis_type_target %in% diagnosis_db) != length(diagnosis_type_target)) | (sum(diagnosis_type_targetfamily %in% diagnosis_db) != length(diagnosis_type_targetfamily)))
	{
		stop("Error: One or more of the provided types of diagnosis are not found in the library.")
	}

	if((register_target == "ALL" & !is.null(register_target)) & (register_targetfamily == "ALL" & !is.null(register_targetfamily)))
	{
		register_target = register_db
		register_targetfamily = register_db
	} else if((register_target == "ALL" & !is.null(register_target)) & (register_targetfamily == "ALL" & !is.null(register_targetfamily))){
		register_target = register_db
	} else if((register_target == "ALL" & !is.null(register_target)) & (register_targetfamily == "ALL" & !is.null(register_targetfamily))){
		register_targetfamily = register_db
	}

	# Check if register options are correct
	if((sum(register_target %in% register_db) != length(register_target)) | (sum(register_targetfamily %in% register_db) != length(register_targetfamily)))
	{
		stop("Error: One or more of the provided types of registers are not found in the library.")
	}

	# Check if earliest_onset is correct
	if((earliest_onset_target < 1 | is.na(earliest_onset_target)) | (earliest_onset_targetfamily < 1 | is.na(earliest_onset_targetfamily)))
	{
		stop("Error: Earliest age of onset cannot be smaller than 1 or null. Default is 1.")
	}

	if(!is.null(family_type))
	{
		if(length(family_type) != 1)
		{
			stop("Error: Only one familial relationship can be provided.")
		}

		if((family_type %in% family_db) == FALSE)
		{
			stop("Error: Entered familial relationship does not excist. For this function you can only choose between PO, FS, and HS. ")
		}
	} else{
		stop("Error: Familial relationship needs to be provided. For this function you can only choose between PO, FS, and HS. ")
	}

	# Do sex check
  if (!is.null(sex)) {
    if (!grepl(sex, "both|male|female")) {
      stop("Error: This sex does not excist. Please use 'male', 'female', or 'both' ")
    } else if (sex == "both") {
      sex = c('male', 'female')
    }
  }

	# Check if earliest_onset is correct
	if((latest_onset_target < 1 | is.na(latest_onset_target)) | (latest_onset_targetfamily < 1 | is.na(latest_onset_targetfamily)))
	{
		stop("Error: Maximum age cannot be smaller than 1 or null. Default is 100.")
	}

	if(!(nFamMember %in% c(paste0("e", 0:100), paste0("s", 1:100),paste0("l", 0:100))))
	{
		stop("Error: Provide correct number of family member e.g. e1 (1 family member), s2 (less than 2), or l2 (more than 2). Default is l0 (more than 0).")
	}

	# print all input variables
	cat("\n")
	cat("Requested ICD Code(s) target population:\t\t\t ", paste(phenotype_ICDcode_target,collapse=", "), "\n")
	cat("Requested ICD Code(s) target population family members:\t\t ", paste(phenotype_ICDcode_targetfamily,collapse=", "), "\n")
	cat("Requested Ascertainment Window:\t\t\t\t\t ", paste(birth_date_min, " to ",  birth_date_max, " with end of observation set at ", study_end_at, sep=""), "\n")
	cat("Requested Residential Status(es):\t\t\t\t ", paste(residential_status,collapse=", "), "\n")
	cat("Requested Diagnosis Type(s) target population:\t\t\t ", paste(diagnosis_type_target,collapse=", "), "\n")
	cat("Requested Diagnosis Type(s) target population family members:\t ", paste(diagnosis_type_targetfamily,collapse=", "), "\n")
	cat("Requested Diagnosis Register(s) target population:\t\t ", paste(register_target,collapse=", "), "\n")
	cat("Requested Diagnosis Register(s) target population family members:", paste(register_targetfamily,collapse=", "), "\n")
	cat("Requested Earliest Age of Onset target population:\t\t ", earliest_onset_target, "\n")
	cat("Requested Earliest Age of Onset target population family members:", earliest_onset_targetfamily, "\n")
	cat("Requested Latest Age of Onset target population:\t\t ", latest_onset_target, "\n")
	cat("Requested Latest Age of Onset target population family members:\t ", latest_onset_targetfamily, "\n")
	cat("Requested Family Type:\t\t\t\t\t\t ", family_type, "\n")
	cat("Requested Number of Family Members:\t\t\t\t ", nFamMember, "\n")
	cat("Requested Sex:\t\t\t\t\t\t\t ", paste(sex,collapse=" and "), "\n")

	############################################################################
	############################# Functional part ##############################
	############################################################################

  if (file.exists(data_path)) {
    print(sprintf("Info: Data file '%s' already exists, will not run SQL query to generate it", data_path))
  } else {
    generator <- QueryGenerator$new()

    target_icd_regexp <- paste("^", phenotype_ICDcode_target, sep="", collapse="|")
    targetfamily_icd_regexp <- paste("^", phenotype_ICDcode_targetfamily, sep="", collapse="|")

    target_query <- generator$family_survival_by_icd_codes(
      icd_codes_regexp=target_icd_regexp,
      study_end_at=study_end_at,
      birth_date_min=entire_window,
      birth_date_max=birth_date_max,
      earliest_onset=earliest_onset_target,
      latest_onset=latest_onset_target,
      gender=sex,
      status=residential_status,
      diagnosis_kind=diagnosis_type_target,
      record_origin=register_target,
      relationship_kind=family_type,
      vertical_relationships=vertical_db
      )
#    fileConn<-file("/home/jmei/targetquery.txt")
#    writeLines(target_query, fileConn)                 
#    close(fileConn)
    targetfamily_query <- generator$family_survival_by_icd_codes(
      icd_codes_regexp=targetfamily_icd_regexp,
      study_end_at=study_end_at,
      birth_date_min=entire_window,
      birth_date_max=birth_date_max,
      earliest_onset=earliest_onset_targetfamily,
      latest_onset=latest_onset_targetfamily,
      gender=sex,
      status=residential_status,
      diagnosis_kind=diagnosis_type_targetfamily,
      record_origin=register_targetfamily,
      relationship_kind=family_type,
      vertical_relationships=vertical_db
      )
#    fileConn<-file("/home/jmei/targetfamilyquery.txt")
#    writeLines(targetfamily_query, fileConn)                 
#    close(fileConn)
    query <- sprintf("
      WITH target AS (
        %s
      ), target_family AS (
        %s
      ) SELECT t.person_id
             , t.failure_time
             , t.failure_status
             , tf.relatives
             , tf.diagnosed_relatives
          FROM target AS t
               INNER JOIN target_family AS tf
                    USING (person_id)
    ", target_query, targetfamily_query)

    err_code <- generator$execute_query(
      query,
      data_path,
      "localhost",
    )

    if (err_code != 0) {
	  	stop("Error: Failured to execute SQL query using psql")
    }

    print(sprintf("Info: SQL query successfully executed with results written to '%s'", data_path))
  }

  survival_data <- fread(
    file=data_path,
    sep=",",
    header=TRUE,
    encoding="UTF-8",
    data.table=TRUE
  )

  survival_data = data.frame(survival_data)
  conn = IbpRegistryClient::Connection$new("localhost", 5432, NULL, NULL)
  genpop  = data.frame(conn$table("civil.people"))[,c(1,3)]
  genpop[,2] = as.character(genpop[,2])
  survival_data = survival_data[survival_data[,1] %in% genpop[genpop[,2] >= birth_date_min,1],]
  
  ####
  
  letter = strsplit(nFamMember,"")[[1]][1]
  number = paste(strsplit(nFamMember,"")[[1]][2:length(strsplit(nFamMember,"")[[1]])], collapse="")
  
  if(letter == "e")
  {
    survival_data = survival_data[survival_data[,4] == number, c(1:3,5,4)]
  } else if(letter == "s"){
    survival_data = survival_data[survival_data[,4] <= number, c(1:3,5,4)]
  } else if(letter == "l"){
    survival_data = survival_data[survival_data[,4] >= number, c(1:3,5,4)]
  }
  
  anygroup = survival_data[survival_data$diagnosed_relatives > 0,]
  anygroup$diagnosed_relatives = "Any"
  cuminc_data = rbind(anygroup,survival_data)
  rm(anygroup)

 	nonegroup = survival_data[survival_data$relatives == 0,]
	nonegroup$diagnosed_relatives = "NoFamilyMembers"
	cuminc_data = rbind(cuminc_data,nonegroup)
	rm(nonegroup)
   
  n_familymembers <- survival_data %>%
                                      summarise(max_diagnosed_relatives = max(diagnosed_relatives)) %>%
                                      pull(max_diagnosed_relatives)

	x = NULL
	# Loop though classes to check if consuring column is multiple options. If not delete group
	for(i in c("Any", "NoFamilyMembers", 0:n_familymembers))
	{
	  if(length(unique(cuminc_data[cuminc_data[,4] == i,3])) <= 1)
		{
      #cuminc_data = cuminc_data[cuminc_data[,5] != i,]
			cat("Group",i, "Affected Family Members: either did not excist or contained no censured individuals and was therefore removed", "\n")
			next
		}
		# Calculate Cumulative Incidence using competing risk
		# Some error occured when I tries it out using everybody in one go, but when running the data one by one it seems to work.
    # While the group statement is redundant I use it later on to keep the group names
		cum1 = cuminc(
                    ftime = cuminc_data[cuminc_data$diagnosed_relatives == i,]$failure_time, 
                    fstatus = cuminc_data[cuminc_data$diagnosed_relatives == i,]$failure_status, 
                    group=cuminc_data[cuminc_data$diagnosed_relatives == i,]$diagnosed_relatives, 
                    cencode = 0
                  )

		# Get Groupname name
		out = strsplit(names(cum1), " ")[[1]][1]
    sel = cbind(data.frame(cum1[[1]]),out)
    # Remove TTE below and above hreshold
	  sel = sel[(sel[,1] >= earliest_onset_target & sel[,1] <= latest_onset_target),]
    sel = data.frame(sel %>% group_by(time) %>% top_n(-1, est))

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
