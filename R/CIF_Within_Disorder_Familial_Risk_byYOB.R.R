# Author: 						Joeri Meijsen
# Date:							X-JUN-2021
# Function Name: 				Danish_Register_CumulativeIncidence_withinDisorder
#
#								IbpRegistryClient
#								dplyr
# Libraries needed:				DBI
#								RPostgres
#								ggplot2
#
# Description: 					Function to estimate lifetime risk (taking censuring into censuring) with genealogy information using the Danish Register Data.
#
# Input Options:
#
#		phenotype_ICDcode:		Individual ICD code or vector of multiple ICD codes (ICD8 or ICD10).
#									        Note: Codes are matched using a grepl(^) e.g., ^F30 so no mid string matches are made but short codes such as F3 will lead to the selection of F30,F31 etc etc
#										      Default = NULL
#
#		sex:						      Define sex of individuals to be compared
#										      both  	- 	Males and Females
#										      male  	- 	Male
#					Options are:		female  - 	Females
#										      Default = 'both'
#
#		family_type:				  Type of familial relationship.
#										      PO		-  Parent-Offspring
#					Options are:		FS		-  Full-Sibling
#										      HS		-  Half-Sibling
#                         1C    -  1st Cousin
#                         Av    -  Avuncular (Aunt/Uncle-Niece/Nephew)  
#                         1G    -  Grandparent-Grandchild  
#                         Default = FS
# 
#		birth_date_min: 			Minimum birth date of individuals in YYYY-MM-DD format. For Example:	'1989-10-12'
#										      Default = NULL
#
#		birth_date_max: 			Maximum birth date of individuals in YYYY-MM-DD format. For Example:	'2000-01-01'
#										Default = NULL
#
#		diagnosis_type:				How is the diagnosis recorded in the system. Give as e.g., 'basic' for single type or as vector e.g., c('basic', 'main')
#										      main
#										      auxiliary
#					Options are:		basic
#										      referral
#										      temporary
#										      complication
#										      associated
#										      Default = c('main','auxiliary')
#
#		register:				    	Which registers does one want. npr, pcrr, or both
#										      npr  - National Patient Register
#					Options are:		pcrr - Psychiatric Central Research Register
#										      ALL
#										      Default = 'ALL'
#
#		residential_status:		Which residential status should be used.
#										      danish-resident
#										      danish-resident-special-address
#										      greenlandic-resident
#										      greenlandic-resident-special-address
#					Options are:		emigrated
#										      dead
#									      	Default = c('danish-resident', 'danish-resident-special-address','emigrated', 'dead')
#
#		earliest_onset:				Remove individuals with a onset before this time point.
#										Default = 0
#
#		latest_onset:				Remove individuals with a onset after this time point.
#										Default = 100
#
#  study_end_at:          End observation period
#                          Default = NULL
#
#  nFamMember:
#
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
Danish_Register_CumulativeIncidence_familial_withinDisorder_byYOB = function(data_path, phenotype_ICDcode=NULL, birth_date_min=NULL, study_end_at=NULL, birth_date_max=NULL, latest_onset=100, sex = "both", residential_status=c('danish-resident', 'danish-resident-special-address','emigrated', 'dead'), earliest_onset=1, diagnosis_type=c('main','auxiliary'), register="ALL", family_type="FS", nFamMember = "l0")
{	
	############################################################################
	############################# libraries needed #############################
	############################################################################

  library(data.table)
	library(dplyr, warn.conflicts = FALSE)
  library(dtplyr)
  library(stringr)
	#library(ggplot2)
	library(cmprsk)

	############################################################################
	################################ databases #################################
	############################################################################

	entire_window="1800-01-01"

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

	############################################################################
	############################### Input Checks ###############################
	############################################################################

	# Break if no ICD code is provided
	if(is.null(phenotype_ICDcode))
	{
		stop("Error: No ICD code provided")
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
	if(earliest_onset < 1 || is.na(earliest_onset))
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
			stop(paste("Error: Entered familial relationship does not excist. For this function you can only choose between",  paste(family_db, collapse=", ")))
		}
	} else {
		stop(paste("Error: Familial relationship needs to be provided. For this function you can choose between ",  paste(family_db, collapse=", ")))
	}

  if (!is.null(sex)) 
  {
    if (!grepl(sex, "both|male|female")) 
    {
      stop("Error: This sex does not excist. Please use 'male', 'female', or 'both' ")
    } else if (sex == "both") {
      sex = c('male', 'female')
    }
  }

	# Check if latest_onset is correct
	if(latest_onset < 1 || is.na(latest_onset))
	{
		stop("Error: Maximum age cannot be smaller than 1 or null. Default is 100.")
	}

  if(!(nFamMember %in% c(paste0("e", 0:100), paste0("s", 1:100),paste0("l", 0:100))))
  {
    stop("Error: Provide correct number of affected family member given that you have at least 1 family member e.g. e1 (1 family member), s2 (less than 2), or l2 (more than 2). Default is l0 (more than 0).")
  }
    
	# print all input variables
	cat("\n")
	cat("Requested ICD Code(s):\t\t\t ", paste(phenotype_ICDcode,collapse=", "), "\n")
	cat("Requested Ascertainment Window:\t\t ", paste(birth_date_min, " to ",  birth_date_max, " with end of observation set at ", study_end_at, sep=""), "\n")
	cat("Requested Residential Status(es):\t ", paste(residential_status,collapse=", "), "\n")
	cat("Requested Diagnosis Type(s):\t\t ", paste(diagnosis_type,collapse=", "), "\n")
	cat("Requested Diagnosis Register(s):\t ", paste(register,collapse=", "), "\n")
	cat("Requested Earliest Age of Onset:\t ", earliest_onset, "\n")
	cat("Requested Latest Age of Onset:\t\t ", latest_onset, "\n")
	cat("Requested Family Type:\t\t\t ", family_type, "\n")
	cat("Requested Number of Family Members:\t ", nFamMember, "\n")
	cat("Requested Sex:\t\t\t\t ", paste(sex,collapse=" and "), "\n")

	############################################################################
	############################# Functional part ##############################
	############################################################################

  if (file.exists(data_path)) 
  {
    print(sprintf("Info: Data file '%s' already exists, will not run SQL query to generate it", data_path))
  } else {
    icd_regexp <- paste("^", phenotype_ICDcode, sep="", collapse="|")

    generator <- QueryGenerator$new()

    survival_query <- generator$family_survival_by_icd_codes(
                      icd_codes_regexp=icd_regexp,
                      study_end_at=study_end_at,
                      birth_date_min=entire_window,
                      birth_date_max=birth_date_max,
                      earliest_onset=earliest_onset,
                      latest_onset=latest_onset,
                      gender=sex,
                      status=residential_status,
                      diagnosis_kind=diagnosis_type,
                      record_origin=register,
                      relationship_kind=family_type,
                      vertical_relationships=vertical_db
                      )

    err_code <- generator$execute_query(
                survival_query,
                data_path,
                "localhost"
                )

    if (err_code != 0) 
    {
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
	genpop  = data.frame(conn$table("civil.people"))[,c(1:3)]
	survival_data = merge(survival_data, genpop[genpop$born_at >= birth_date_min,], by.x="person_id", by.y="id")
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
		exit
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
	  
	n_familymembers <- survival_data %>%
                                      summarise(max_diagnosed_relatives = max(diagnosed_relatives)) %>%
                                      pull(max_diagnosed_relatives)

	final = NULL
	# Loop though classes to check if consuring column is multiple options. If not delete group
	for(nrFAM in c("Any","NoFamilyMembers",0:n_familymembers))
	{
		for(year in c("all", sort(unique(cuminc_data$born_at))))
		{
			cat("Number of affected family member:",nrFAM, "& Year or birth:",year, "\n")
			
			if(year == "all") # all years
			{
				dat = cuminc_data[cuminc_data$diagnosed_relatives == nrFAM,]
			} else {
				dat = cuminc_data[cuminc_data$born_at == year & cuminc_data$diagnosed_relatives == nrFAM,]
			}
	
			if(length(unique(dat[dat[,4] == nrFAM,3])) <= 1)
			{
				cat("Group",nrFAM, "Affected Family Members born in", year,": either did not excist or contained no censured individuals and was therefore removed", "\n")
				next
			}
			
			# Calculate Cumulative Incidence using competing risk
			# Some error occured when I tries it out using everybody in one go, but when running the data one by one it seems to work.
			# While the group statement is redundant I use it later on to keep the group names
			cum1 = cuminc(  
							ftime 	= dat$failure_time, 
							fstatus = dat$failure_status, 
							group 	= dat$diagnosed_relatives, 
							cencode = 0
						 )

			# Get Groupname name
			out = strsplit(names(cum1), " ")[[1]][1]
			x = cbind(data.frame(cum1[[1]]),out)
			# Remove TTE below and above hreshold
			x = x[(x[,1] >= earliest_onset & x[,1] <= latest_onset),]
			x = data.frame(x %>% group_by(time) %>% top_n(-1, est))

			res = NULL
		
			# Calculate upper and lower conficence interval
			# controls are defined as: everybody regardless of disorder status that have a follow-up time larger than a given time point e.g., everybody with a follow up time larger than 20
			for(i in 1:nrow(x))
			{
				res   = rbind(res, c(x[i,2] - qnorm(0.975)* sqrt(x[i,3]), x[i,2] + qnorm(0.975)* sqrt(x[i,3]),year))
			}
		
			final = rbind(final,cbind(x,res))
		}
	}

	if(is.null(final))
	{
		print("No CIF could be calculated. Returning NULL")
		return(NULL)
		exit
	}
	
	colnames(final) = c("Time", "Estimate", "Variance", "N Affected Family Members", "L95","U95","Year")	
	
	for(col in c(1:3,5:6))
	{
		final[,col] = as.numeric(final[,col])
	}

	mat = cbind(unique(final$Time))
	colnames(mat)= "Time"
	
#	counter=2
#	for(year in c("all", sort(unique(cuminc_data$born_at))))
#	{
#		mat = merge(mat, final[final$Year == year,c(1,2)], by="Time", all.x=T)
#		colnames(mat)[counter] = year
#		counter = counter + 1
#	}
	
#	colnames(mat)[1] = "Age"
#	mat = list(final, mat)
	return(final)

}
