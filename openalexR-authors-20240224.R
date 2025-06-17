############# Author's search ##########
######## Author: Yan Han
######## Date: May 9, 2023
######## Updated: Feb 3, 2024
##### Search authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR
# OpenAlex Beta explorer: https://explore.openalex.org/ (the explorer seems not to display all the possible researchers. In ohter words, You shall use API
# The explorer can be only used as a verification/testing purpose!!!

install.packages("remotes")
remotes::install_github("ropensci/openalexR", force=TRUE) 

#install.packages("openalexR") #install.packages("openalexR")  ### use the latest development version due to issue with the production version openalexR 1.10. Waiting for 1.2.0
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("testthat")
install.packages("readr")
install.packages("readxl")
install.packages("openxlsx")

library(openalexR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(testthat)
library(openxlsx)
library(readr)

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/UA-datasets/openalexR-test")

######### Step 1 :  Get unit/dept/college authors ######################
######### LDAP search
#LDAP query against ldap.arizona.edu (public, no account required), e.g.
# Via linux terminal: ldapsearch -H ldap://ldap.arizona.edu -D "" -b "o=University of Arizona,c=US" -w -x 'departmentNumber=1705' givenName sn

# To find Dept HR code: log into apps.iam.arizona.edu to search person >> OrgSearch (partent org, child orgs) 
# Example: https://apps.iam.arizona.edu/orgs/ua_orgs/view/1705

# Department of Medicine at College of Medicine (UMC-Tucson): code : 0713
# 1. Get LDAP data by running 
#    > "ldapsearch -H ldap://ldap.arizona.edu -D "" -b "o=University of Arizona,c=US" -w -x 'departmentNumber=0713' sn > dept0713.ldif
# 2. Convert the LDAP data to CSV file and then compare it with Funk's CSV (XLSV converted to CSV), which generates two CSV files: 
#   {dept}.csv = dept LDAP ; {dept}_common.csv = common entries between the {dept}.csv and Funk's CSV
#   This is by Using "common_entries.py" to convert LDIF (LDAP data interchange format) to CSV format. 
#   > "Python common_entries.py" to enter dept name such as "dept0713". 


########################## Functions ###########################33

#####################################################
# Function: Find author via his/her affiliation 
# Two fields for filtering: affiliation_display_name first, and then affiliations_other
# Note: "affiliations_other" can be a string or a vector of strings. So both cases shall be taken care of.
# Note: Some Banner authors have affiliation of "University of Arizona" or alike. 
#####################################################
search_author <- function(author_name, affiliation_name) {
  UArizona <- c("https://openalex.org/I138006243", "https://ror.org/03m2x1q45", "University of Arizona", "I138006243", "Banner")
  
  author_from_names <- oa_fetch(entity = "author", search = author_name)

  # Check if any authors were retrieved
  if (!is.null(author_from_names) && nrow(author_from_names) > 0) {
    matches <- rep(FALSE, nrow(author_from_names))
    
    for (i in seq_len(nrow(author_from_names))) {
      display_name_match <- FALSE
      affiliations_other_match <- FALSE
      
      
      #if ("affiliation_display_name" %in% names(author_from_names) && !is.na(author_from_names$affiliation_display_name[i])) {
      #  display_name_match <- any(sapply(UArizona, function(pattern) grepl(pattern, author_from_names$affiliation_display_name[i], ignore.case = TRUE)))
      #}
      # Check primary affiliation only if the field exists
      if ("affiliation_display_name" %in% names(author_from_names)) {
        display_name_match <- !is.na(author_from_names$affiliation_display_name[i]) && 
          any(sapply(UArizona, function(pattern) 
            grepl(pattern, author_from_names$affiliation_display_name[i], ignore.case = TRUE)))
      }
      
      # Check affiliations_other with contains UArizona strings
      if ("affiliations_other" %in% names(author_from_names)) {
        affiliations <- author_from_names$affiliations_other[[i]]
        if (length(affiliations) > 0) {
          affiliations_other_match <- any(UArizona %in% affiliations)
        }
      }
      
      matches[i] <- display_name_match | affiliations_other_match
    }
    
    filtered_authors <- author_from_names[matches, ]
    
    if (nrow(filtered_authors) == 0) {
      message("No authors found matching the given affiliation.")
    } else {
      return(filtered_authors)
    }
  } else {
    message("No data retrieved from OpenAlex's API.")
    return(NULL)
  }
}


######### old
search_author3 <- function(author_name, affiliation_name) {
  UArizona <- c("https://openalex.org/I138006243", "https://ror.org/03m2x1q45", "University of Arizona", "I138006243")
  
  author_from_names <- oa_fetch(entity = "author", search = author_name)
  
  if (!is.null(author_from_names) && nrow(author_from_names) > 0) {
    matches <- rep(FALSE, nrow(author_from_names))
    
    for (i in seq_len(nrow(author_from_names))) {
      display_name_match <- FALSE
      affiliations_other_match <- FALSE
      
      # Check affiliation_display_name if it exists
      if ("affiliation_display_name" %in% names(author_from_names) && !is.na(author_from_names$affiliation_display_name[i])) {
        display_name_match <- any(sapply(UArizona, function(pattern) grepl(pattern, author_from_names$affiliation_display_name[i], ignore.case = TRUE)))
      }
      
      # Check affiliations_other
      if ("affiliations_other" %in% names(author_from_names)) {
        affiliations <- author_from_names$affiliations_other[[i]]
        if (length(affiliations) > 0) {
          affiliations_other_match <- any(UArizona %in% affiliations)
        }
      }
      
      matches[i] <- display_name_match | affiliations_other_match
    }
    
    filtered_authors <- author_from_names[matches, ]
    
    if (nrow(filtered_authors) == 0) {
      message("No authors found matching the given affiliation.")
    } else {
      return(filtered_authors)
    }
  } else {
    message("No data retrieved from OpenAlex's API.")
    return(NULL)
  }
}

#####################################################
# Function: Calculate works count
#####################################################
calculate_works_count <- function(author_name, affiliation_name, year) {
  # getting data from openAlexR API
  author_from_names <- oa_fetch(entity = "author", search = author_name )
  if (is.null(author_from_names)) {
    author_stats <- NULL
  }
  else {
    # Filter out not "University of Arizona" authors using "affiliation_display_name" column.
    # other filtering fields can be "affiliation_id", "affiliation_ror"
    filtered_authors <- subset(author_from_names, grepl(affiliation_name, affiliation_display_name, ignore.case=TRUE))
    print(filtered_authors)
    
    # If NOT found the author, return NULL
    if (nrow(filtered_authors) == 0 )  {
      author_stats <- NULL
    }
    else {
      # works_count is a list, getting "counts_by_year" column
      works_count  <- filtered_authors$counts_by_year
      print(works_count)
      works_sum_year <- 0
      total_works_sum_year <- 0
      
      # cited_by_count is a list
      cited_by_count <- filtered_authors$cited_by_count
      cited_sum_year <- 0
      total_cited_sum_year <-0
      
      # check the works_count
      for (i in 1:length(works_count)) {
        # Access the data frame within the list
        df <- works_count[[i]]
        
        if (is.data.frame(df)) {
          # Filter the data frame by year
          filtered_df_year <- df[df$year == year, ]
          # Calculate the sum of the filtered 'works_count' column
          works_sum_year <- sum(filtered_df_year$works_count)
          total_works_sum_year <- total_works_sum_year + works_sum_year
          # Calculate the sum of the filtered 'cited_by_count' column
          cited_sum_year <- sum(filtered_df_year$cited_by_count)
          total_cited_sum_year <- total_cited_sum_year + cited_sum_year
          
          # reset this number to 0 after each iteration
          works_sum_year <- 0
          cited_sum_year <- 0
        } else {
          ##### Note: If you see error msg: Error: $ operator is invalid for atomic vectors
          # That means certain works_count  is logical and has no data
          print("This is NOT a dataframe. Data Wrong")
          # set value as -1 for warning message
          Total_sum_of_works = -1
          Total_cited_sum_year = -1
        }
      } # for loop
      
      # Build output dataframe author_stats
      author_stats <-data.frame (
        Name = author_name,
        OpenAlexId = filtered_authors$id,
        Year = year,
        Total_sum_of_works = total_works_sum_year,
        Total_sum_of_cited = total_cited_sum_year
      )
      
      # reset the var after done
      total_works_sum_year <- 0
      total_cited_sum_year <- 0
    } # if
    
    return (author_stats)
  }
}

################################################################
##### Function: Get works from OA's author id and publication year #########
get_works_from_authorid_by_year <- function(author_id, publication_year) {
  # Check if author_id is NULL
  if (is.null(author_id)) {
    message("author_id not found for this author.")
    return (NULL)
  }
  # Attempt to fetch author works and handle potential errors
  author_works <- tryCatch({
    oa_fetch(
      entity = "works",
      author.id = author_id, 
      publication_year = publication_year,
      verbose = TRUE
    )
  }, error = function(e) {
    message("An error occurred: ", e$message)
    return(NULL)
  })
  return(author_works)
}

##### Function: Get works from author ORCID and year
get_works_from_orcid_by_year <- function(orcid, publication_year) {
  if (is.null(orcid)) {
    stop("ORCID cannot be NULL.")
  }
  
  author_works <- tryCatch({
    oa_fetch(
    entity = "works",
    author.orcid = orcid, 
    publication_year = publication_year,
    verbose = TRUE
  )
  }, error = function(e) {
    message ("An error occured: ", e$message)
    return (NULL)
  })
  return(author_works)
}

################### Function ######################
get_dept_authors_names <- function(dept_code, affiliation_name) {
  file_path <- sprintf("%s_common.csv", dept_code)
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  LDAPdata <- read_csv(file_path, show_col_types = FALSE)
  
  if (nrow(LDAPdata) == 0) {
    message("The CSV file is empty: ", file_path)
    return(list())  # Return an empty list
  }
  
  
  authors_names <- LDAPdata$cn
  
  dept_authors_names <- list() 
  
  for (i in 1:length(authors_names) ) {
    # Access the current row
    author_name <- authors_names[i]
    print (paste(author_name, affiliation_name))
    
    tryCatch({
      author_result_affiliation <- search_author(author_name, affiliation_name )
      author_stats              <- calculate_works_count(author_name, affiliation_name, 2022)
    }, error = function(e) {
      message("Error processing author ", author_name, ": ", e$message)
    })
    
    # Check if any of the results are NULL or have zero rows; handle accordingly
    if (is.null(author_result_affiliation) || nrow(author_result_affiliation) == 0) {
      author_result_affiliation <- NA  # or some other placeholder value
    }
    if (is.null(author_stats) || length(author_stats) == 0) {
      author_stats <- NA  # or some other placeholder value
    }
    # Append the results to the list
    dept_authors_names[[i]] <- data.frame(author_name = author_name, 
                                        author_result_affiliation = author_result_affiliation, 
                                        author_stats = author_stats)
  }
  return (dept_authors_names)
}

###################################################################################
### Function: output author's works by its oa author_id and year. 
### It is necessary to do so, because this author_works df are very complex (a df of dfs)
### It is a small db, cannot be represented by a single sheet of XLSX
output_works_by_authorid_by_year <- function(author_name, author_id, year, output_path) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is not installed. Please install it using install.packages('dplyr').")
  }
  if (!requireNamespace("writexl", quietly = TRUE)) {
    stop("writexl package is not installed. Please install it using install.packages('writexl').")
  }
  
  if (is.null(author_id)) {
    stop("author_id cannot be NULL.")
  }
  
  author_works <- get_works_from_authorid_by_year(author_id, year)
  if (is.null(author_works) || length(author_works) == 0) {
    message("No works found for the provided author ID and year.")
    return(NULL)
  }
  
  #!!!!! Very important: Need to handle NA value in OpenAlex data. Otherwise, it will crash other functions such as writexlsx()!!!
  # The best way so far is to "as.character" and then replace NUL. 
  author_works <- author_works %>%
  #  mutate(across(where(is.character), ~replace_na(., "N/A")),
  #         across(where(is.numeric), ~replace_na(., "N/A")) )
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~replace_na(., "N/A")))
  
  # Exclude certain columns.
  author_works <-select(author_works, -c(counts_by_year, concepts))
  print(author_works)

  #full_output_path <- file.path(getwd(), output_path)
  
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    print(paste("Directory created at:", output_path))  # Confirm directory creation
  }
  print(paste("Current working directory:", getwd()))  # Confirm current directory
  
  author_works <- author_works %>%
    mutate(across(where(is.character), ~ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))
  
  # Generating the filename
  filename <- paste0(gsub("[[:punct:]]", "", author_name), "_", year, ".xlsx")
  full_path <- file.path(output_path, filename)
  
  writexl::write_xlsx(author_works, path = full_path)
  message("File '", full_path, "' has been created.")
  #write.csv(author_works, file=filename, row.names = FALSE)
}

############################################################################
#### Now getting every author's works by each dept ###
### Function: Parameter: dept_author_data: list 
output_dept_author_works_by_year <- function(dept_name, dept_authors_names, year) {
  # Check if dept_name is provided and valid
  if (is.null(dept_name) || !nzchar(dept_name)) {
    stop("Department name ('dept_name') must be provided and cannot be empty.")
  }
  
  # Check if dept_authors_names is empty
  if (length(dept_authors_names) == 0) {
    message("No authors found for department: ", dept_name)
    return()  # Exit the function early
  }
  
  output_path <- file.path(getwd(), "output", dept_name) # Example path construction
  for (i in 1: length(dept_authors_names)) {
    current_df <- dept_authors_names[[i]]
    author_name <- current_df$author_name
    author_id <- current_df$author_result_affiliation.id
    print(author_name)
    get_works_from_authorid_by_year(author_id, year)
    # if author_id is NOT NULL, get the works from this author and output
    if (!is.null(author_id) && nzchar(author_id)) {
      output_works_by_authorid_by_year(author_name, author_id, year, output_path)
    } else {
      message("Skipping due to NULL author_id for ", author_name)
    }
  }
  
}



#######################################

#dept_code <- readline(prompt = "Please enter the department code: ")
#affiliation_name <- readline(prompt = "Please enter the affiliation: ")

#### Dept of Medicine (HR code: 0713 and 0788) Test date: 2024-01-24 
##### Test cases: data is not uniformed.  
author_works <- get_works_from_authorid_by_year("a5082148123", 2022)
output_works_by_authorid_by_year("Keith A Joiner", "a5082148123", 2022, "./output/test")

# Error in x[is.na(x)] <- na.string : replacement has length zero. Why? 
author_works <- get_works_from_authorid_by_year("a5080182165", 2022)
output_works_by_authorid_by_year("Sara Centuori", "a5080182165", 2022, "./output/test")

# Bekir Tanriover: https://openalex.org/authors/a5016874418 
author_name <- "Bekir Tanriover"
author_id <- "a5016874418"
publication_year <- "2022"
author_works <- get_works_from_authorid_by_year(author_id, publication_year)
UAresult1 <- search_author(author_name, affiliation_name)

output_works_by_authorid_by_year(author_name, author_id, publication_year, "./output/test")

# H.-H Sherry Chow
author_name <-"H. H. Sherry Chow"
author_id <- "a5018050941"
author_works <- get_works_from_authorid_by_year(author_id, publication_year)
output_works_by_authorid_by_year(author_name, author_id, publication_year, "./output/test")

# author_name <- "H. Sherry Chow"
#authod_id <- 
#author_works <- get_works_from_authorid_by_year(author_id, publication_year)
#output_works_by_authorid_by_year(author_name, author_id, publication_year)

### 2024-01-25: Affiliation issues: 
### [1] "Tejo K Vemulapalli University" Error in is.factor(x) : object 'affiliation_display_name' not found In addition: Warning messages:
###  1: In oa_request(oa_query(filter = filter_i, multiple_id = multiple_id,                       No records found!
### 2024-01-26: opened an issue with openAlexR https://github.com/ropensci/openalexR/issues/196 and can now find from "affiliations_other"

author_name <- "Tejo K Vemulapalli"
affiliation_name <- "University of Arizona"
author_from_names <- oa_fetch(entity = "author", search = author_name)
UAresult1 <- search_author(author_name, affiliation_name)

author_name <- "Vivian Kominos"
affiliation_name <- "University"
author_from_names <- oa_fetch(entity = "author", search = author_name)
UAresult1 <- search_author(author_name, affiliation_name)

### These have no affiliation with "University". https://openalex.org/authors/a5019422724
author_name <- "Lise Alschuler"  
affiliation_name <- "University of Arizona"
author_from_names <- oa_fetch(entity = "author", search = author_name)
author_from_names$affiliations_other
UAresult1 <- search_author(author_name, affiliation_name)

#### Banner: 
##### Aaron Scott: 2022: 21 articles?? 
#### I am very positive: This is wrong: alternative name Jesse Aaron. Jesse Aaron is not associated with University of Arizona. see https://www.biorxiv.org/content/10.1101/2022.07.07.499185v2
### see https://www.molbiolcell.org/doi/10.1091/mbc.E21-10-0506
### However, oa records says that Aaron Scott is with University of Arizona' 'Imaging Center'



# step2: get dept author name and filtering 
# step 3: get authors works based on the names and affiliation
affiliation_name <- "Unversity of Arizona"
year <- 2022

dept_name <- "dept0701"
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)

### dept0712 has no people.
dept_name <- "dept0712"
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)

### Need to check " "Tejo K Vemulapalli"
dept_name <- "dept0713"
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)

### dept0721 also have H.H. Chow?? 
dept_name <- "dept0721"
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)

dept_name <- "dept0747"
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)

# dept0782 no data 
dept_name <- "dept0782"
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)

dept_name <- "dept0788"
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)

dept_name <- "dept0795" # no people
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)


### Banner- University Medical Center Tucson  ID: i4210124665
### Banner Health ID: i2802412784
### Some of Banner authors have affiliation of "University of Arizona", "University of Arizona Cancer Center". 
### so 
### Affiliations: Using UArizona vector of string (not containing "Banner": found 83 authors' works. There are 148
### 
dept_name <- "dept_banner" 
affiliation_name <- "Banner" 
dept_authors_names <- list()
dept_authors_names <- get_dept_authors_names(dept_name, affiliation_name)
output_dept_author_works_by_year(dept_name, dept_authors_names, year)


#dept07xx_names <- list()
#dept07xx_names <- get_dept_authors_names("dept07xx", "University of Arizona")
#output_dept_author_works_by_year("dept07xx", dept07xx_names, 2022)


#get_dept_author_works_by_year(dept07xx_names, 2022)

#### Test data 
class(dept07xx_results)

dept_data <- dept07xx_results
first_df <- dept_data[[1]]
firt_row<-first_df$author_name
second_row <- first_df$author_result_affiliation.id

##############################################################################

### Banner faculty. No LDAP match??
library(readxl)

file_path <- "Funk_faculty_list.xlsx"
# read the "Banner" sheet
data_sheet_by_name <- read_excel(file_path, sheet="Banner")
head(data_sheet_by_name)

# Banner has no LDAP, so making a new dept with Banner's authors by combing the sheet's LAST NAME and FIRST NAME
data_sheet_by_name$cn <-paste(data_sheet_by_name$`FIRST NAME`, data_sheet_by_name$`LAST NAME`, sep = " ")
new_csv <-data.frame(cn = data_sheet_by_name$cn)
head(new_csv)

write.csv(new_csv, "dept_banner_common.csv", row.names = FALSE)

# run the Banner as a UA's dept
dept_banner <- list()
# Use "Arizona" to see how many authors have  no affiliation of "Arizona"
dept_banner <- get_dept_author_data("dept_banner", "Arizona")
### results:
#[1] "Anthony Witten Arizona"
# Column 'affiliation_display_name' not found in the data.

dept_banner <- list()
# Use "University" to see how many authors have  no affiliation of "University"
dept_banner <- get_dept_author_data("dept_banner", "University")

#### OpenAlex records:

### 2024-01-30: 
# 1. Hong Lee https://deptmedicine.arizona.edu/profile/hong-seok-lee-md-mph
# publications: https://www.ncbi.nlm.nih.gov/myncbi/101LmwjcFptkp/bibliography/public/ 
# His works list has "Lee H", "Lee HS", "Seok Lee H", publishing works from 2008 - latest 2020
# Also his affiliation with NLM is "Department of Cardiovascular Diseases, Mayo Clinic, Scottsdale, AZ, USA."! 

# (note: NLM author search link is not correct with over 70,000 results. Many authors have names such as "Lee H", "Lee HH", "Lee HK")

# openAlex records: searching "Hong S. Lee" returns 12 works from 1990 - 2022. None is from him. 
# Note: Same name, different person

# 2. Amanpreet S. Bains, https://deptmedicine.arizona.edu/profile/amanpreet-s-bains-md
# MD, Assistant Professor, Medicine, Medical Director, Clinical Decision Unit, Division of Inpatient Medicine
# Degrees: MD: University of the West Indies Faculty of Medical Sciences, 2002. Residency: Wilson Memorial Medical Center Internal Medicine
# openAlex records: https://openalex.org/authors/A5005342862 or https://openalex.org/authors/a5076616077 (not the same person)


# To display the list certain fields. to output 

#############################################################
# check to see if openAlexR has the latest entities in OpenAlex (OpenAlex updated its data model(Entities) in June 2023)
# Before April 2023: they are [1] "works"        "authors"      "venues"       "institutions" "concepts"    
# If not, need to use openalexR developer's version
oa_entities()

############################## Testing Cases #######################

############################## ORCID as filter ####################################
# Download all works published by a set of authors using ORCIDs
# use author.orcid as a filter
# https://api.openalex.org/authors/https://orcid.org/0000-0001-9518-2684
### NOTE: May 2023, Not all the works are there, NEED TO discuss with OpenAlex. Most likely the disambigation alg not working well. https://docs.openalex.org/api-entities/authors
### NOTE: Aug 2023, OpenAlex has a new author data with a new disambiguation model. It is getting better but there are still some bugs/errors.
### Example: Aug 2023, there are 80 works associated with "0000-0001-9518-2684". About 40 works are NOT authored by me. (My works are all written in English. For some reason, these works are NOT pulled from ORCID)
### NOTE: Jan 2024, ORCID 0000-0001-9518-2684 returns "No records found!"
works_from_orcids <- oa_fetch(
  entity = "works",
  #author.orcid = c("0000-0001-9518-2684"),  
   author.orcid = c("0000-0001-6187-6610", "0000-0002-8517-9411"),
  verbose = TRUE  
) 


########################### Author matching criteria: 1) starting with ORCID; 2) using latest publication's affiliations 3) 
#### July 2023:  Bekir affiliation shows "Columbia University", which is wrong. He (2009-2013) is at Columbia Univeristy, then he moved to UT Southwestern, and now he is at UA
#### Aug 11, 2023: Bekir affiliation shows "University of Arizona", which is correct now. 
author_from_names <- oa_fetch(entity = "author", search = "Bekir Tanriover" )


### Aug 11, 2023: Results contain wrong info (Haitong Tai: https://openalex.org/A5060511275 ) affiliation not updated yet (probably based on last publication's affiliation)
author_from_names <- oa_fetch(entity = "author", search = "Haw-chih Tai")

#### This authorID upgrade does show my work/citation seems right with affiliation. There are 692 obs of "Yan Han". Most have ORCIDs. 
### Aug 2023: Yan Han: affiliation Jilin university. Wrong. 
author_from_names <- oa_fetch(entity = "author", search = "Yan Han")
author_from_names <- search_author("Yan Han", "University of Arizona")

#### This upgrade found and contains Hong Cui's ID and correct affiliation. but needs further filtering via affiliation
author_from_names <- oa_fetch(entity = "author", search = "Hong Cui")



# R does not have LDAP packages??
# clean all objects from the environment to start
rm(list = ls())

################### Test data
test_data_UAL_authors     <- c("Yan Han", "Ellen Dubinski", "Fernando Rios", "Ahlam Saleh")
test_data_COM_authors     <- c("Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh")
test_data_COM_Tucson_authors <- c("Che Carrie Liu", "Robert M. Aaronson", "Alexa Aasronson", "Mohammed Abbas", "")
test_data_science_authors <- c("Marek Rychlik", "Ali Bilgin", "Beichuan Zhang")
test_data_ischool_authors <- c("Hong Cui")
test_data_others          <- c("Leila Hudson", "Mona Hymel")
test_data_not_updated_authors <-c("Karen Padilla", "Haw-chih Tai")

test_data_affiliation <- c("University of Arizona")
test_data_year <- c("2022", "2021", "2020", "2012")

#### Some testing first 
author_from_names <- oa_fetch(entity = "author", search = "Bekir Tanriover" )
## openalexR 1.10 package failed on the search below. 
author_from_names <- oa_fetch(entity = "author", search = test_data_not_updated_authors[1] )
author_from_names <- oa_fetch(entity = "author", search = "Haw-chih Tai")

# Read test data from CSV: column: LastName, FirstName
file_path <- "test_authors_COM_0713.csv"    #"test_authors_1.csv"
data <- read.csv(file_path) 

# create an empty vector
data_unit_staff <- list()
# iterate 
for (i in 1:nrow(data)) {
  firstname <- as.character(data[i, "givenName"])
  lastname <- as.character(data[i, "sn"])
  data_unit_person <- paste(firstname, lastname)
  data_unit_staff  <- c(data_unit_staff, data_unit_person)
}


################################ Finding Author's name, affiliation, sum of works, and total citations ##############################
# Filter Doc: https://github.com/ropensci/openalexR/blob/main/vignettes/articles/Filters.Rmd

#### 1. First do a fuzzy search on author's name ##########################
###  do NOT use display_name because it requires an exact match. Often there are multiple middle names for an author
author_from_names <- oa_fetch(entity = "author",
                               search = test_data_COM_authors[2] ) ### "search" syntax allows fuzzy search for middle name
# first checking
if (!is.null(author_from_names)) {
  print("author found")
  # Filter out not "University of Arizona" authors using "affiliation_display_name" column.
  # other filtering fields can be "affiliation_id", "affiliation_ror"
  filtered_authors <- subset(author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE))
  # Showing the results
  filtered_authors |>
    show_authors() |>
    knitr::kable()
  if (nrow(filtered_authors) == 0 )  {
    print("The filtered_authors dataframe is empty")
  }
  # output works_count in 2022
  # works_count is a list
  works_count  <- filtered_authors$counts_by_year
  print(works_count)

  # cited_by_count
  cited_by_count <- filtered_authors$cited_by_count

  # Testing data
  works_sum_year <- 0
  total_works_sum_year <- 0
  cited_sum_year <- 0
  total_cited_sum_year <-0

  ### Handle empty list
  df <-works_count[[20]]
  if (is.null(df)) {
    print("Emement is null")
  } else {
    print("CONT")
  }

  filtered_df_2022 <- df[df$year == 2022, ]
  # Loop through each data frame in the list
  length(works_count)
  for (i in 1:length(works_count)) {
    # Access the data frame within the list
    df <- works_count[[i]]

    if (is.data.frame(df)) {
    ##### Note: If you see error msg: Error: $ operator is invalid for atomic vectors
    # That means certain works_count  is logical and has no data
  ########## Building block for count each year
    # Filter the data frame for the year 2022
    filtered_df_2022 <- df[df$year == 2022, ]
    # Calculate the sum of the filtered 'works_count' column
    works_sum_2022 <- sum(filtered_df_2022$works_count)
    total_works_sum_2022 <- total_works_sum_2022 + works_sum_2022
    # Calculate the sum of the filtered 'cited_by_count' column
    cited_sum_2022 <- sum(filtered_df_2022$cited_by_count)
    total_cited_sum_2022 <- total_cited_sum_2022 + cited_sum_2022

    # reset this number to 0
    works_sum_2022 <- 0
    cited_sum_2022 <- 0
    } else {
      print("This is NOT a dataframe. Data wrong")
    }
}

  total_cited_sum_2022 <- 0
  total_works_sum_2022 <- 0
}

########################## TESTING PEOPLE ####################
### Format: Name: Year: Works/Cited



############# College of Medicine Tucson Test Date:  2023-05-14: If test in a different date, result may vary
#### U of Arizona College of Medicine Faculty and Staff Directory https://medicine.arizona.edu/directory/faculty-staff
#### Phillip Kuo: 2022: 30 and 133: 26 IDs
####            : 2023-07: after authorID updates: 17 and 330
author_name <- "Phillip H. Kuo"
affiliation_name <- "University"
author_result_fuzzy       <- oa_fetch(entity = "author", search = author_name)
author_result_affiliation <- search_author(author_name, affiliation_name )
author_stats              <- calculate_works_count(author_name, affiliation_name, 2022)

### Bekir Tanriover returns 5 openAlex ID: 2022: works 11, cited 166; 2021: works 4 cited 167
###: 2023-07: after authorID updates: NULL (??? error )
### 2023-08: After authorID updates: correct affiliation: 2022: 16 and 170
author_name <- "Bekir Tanriover"
author_result_fuzzy       <- oa_fetch(entity = "author", search = author_name)
author_result_affiliation <- search_author(author_name, affiliation_name )
author_stats              <- calculate_works_count(author_name, affiliation_name, 2022)



author_stats <- calculate_works_count(test_data_COM_authors[2], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_authors[2], test_data_affiliation[1], test_data_year[2])
###  Ahlam Saleh returns 0, because of "One list does not contain a valid OpenAlex collection" ????
author_stats <- calculate_works_count(test_data_COM_authors[3], test_data_affiliation[1], test_data_year[1])

###### College of Medicine Tucson
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[1], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[2], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[3], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[4], test_data_affiliation[1], test_data_year[1])
author_stats <- calculate_works_count(test_data_COM_Tucson_authors[5], test_data_affiliation[1], test_data_year[1])


######## Science
### June 2023: Marek Rychlik returns : works 0, cited 11
### Aug 2023: Good result: works 35, cited 502
author_name <- "Marek Rychlik"
author_result_fuzzy       <- oa_fetch(entity = "author", search = author_name)
author_result_affiliation <- search_author(author_name, affiliation_name )
author_stats              <- calculate_works_count(author_name, affiliation_name, 2022)

### Ali Bilgin: works 4, cited 187
### June 2023: Showing 2 results. The other searches show one combined result
### Aug 2023: 2 results: "University of Arizona Medical Center" and "University of Arizona". Need to combine 
author_name <- "Ali Bilgin"
author_result_fuzzy       <- oa_fetch(entity = "author", search = author_name)
author_result_affiliation <- search_author(author_name, affiliation_name )
author_stats              <- calculate_works_count(author_name, affiliation_name, 2022)

####### iSchool
### Hong Cui: NOT found: 2022: 
### June 2023: Showing 1 results. 
### Aug 2023: showing 2 results: one with ORCID, and the other shows none. Need to combine 
author_name <- "Hong Cui"
author_result_fuzzy       <- oa_fetch(entity = "author", search = author_name)
author_result_affiliation <- search_author(author_name, affiliation_name )
author_stats              <- calculate_works_count(author_name, affiliation_name, 2022)

###### Others
### Leila Hudson: 2022: 0/2
### Aug 2023: good result.
author_name <- "Leila Hudson"
author_result_fuzzy       <- oa_fetch(entity = "author", search = author_name)
author_result_affiliation <- search_author(author_name, affiliation_name )
author_stats              <- calculate_works_count(author_name, affiliation_name, 2022)

# Error in works_count[[i]] : subscript out of bounds
author_stats <- calculate_works_count(test_data_others[2], test_data_affiliation[1], test_data_year[1])


##########################################################
#### testing all authors in College of Medicine - Tucson ###
# 10 people per line
# Author's name only contain First name and Last name.
# Author's middle name is not included due to index often does not contain such.
test_data_COM_Tucson_authors <- c("Che Liu", "Robert Aaronson", "Alexa Aaronson", "Joy Abaidoo", "Stephanie Abalos", "Mohammed Abbas", "Adnan Abbasi",  "Kristopher Abbate", "Michelle Abbate", "Lara Abbottt",
                                  "Gebran Abbound", "Mazin Abdaigadir", "Adel Abdallah", "Arwa Abdel-Raheem", "Yasmeen Abdulrahman", "Kalkidan Abebe", "Michael Abecassis", "Anne Abel", "Ty Abel", "Yulia Abidov",
                                  "Richard Ablin", "Ahmed Aboul-Nasr", "Arin Aboulian", "Suzanne Abrahamson", "Amber Abrams", "Artyom Abramyan", "Edward Abril", "Anas Husni Mohamad Abu Assi", "Laila Zaid", "Joe Abuhakmeh",
                                  "Xiaohong Zhang", "Hui Zhang")

### Including all faculty and staff at https://medicine.arizona.edu/directory/faculty-staff
rm(unit_authors_list)

unit_authors_list <- list(author_stats)
unit_authors_list_org_arizona <- list(author_stats)
#author_stats <-calculate_works_count(test_data_COM_Tucson_authors[1], "University of Arizona",2022)
#unit_authors_list <- append(unit_authors_list, list(author_stats))
#unit_authors_list


##### Test specific unit 
authors <- data_unit_staff    # Here is COM_0713

#### Retrieving one author at a time.
for (author in authors) {
  print(author)
  # Setting org_name can be critical for the # of results got.
  # If org_name = "", there will be many unrelated people.
  # College of Medicine OpenAlex data some do not have affiliation with University of Arizona.
  org_name = "university"
  
  # filter results using org_name
  temp_author_status <- calculate_works_count(author, org_name, 2022)
  temp_author_status
  unit_authors_list <- append(unit_authors_list, list(temp_author_status))
  }

unit_authors_list_org_arizona <- unit_authors_list


###########################################

# search and filter using openAlex institution ID "affiliation_id"
author2_from_names <- oa_fetch(entity = "author", search = "Yan Han" ) ### "search" syntax allows fuzzy search for middle name
filtered_author2 <- subset(author2_from_names, grepl("https://openalex.org/I138006243", affiliation_id, ignore.case=TRUE))

# display_name and filter using ROR "affiliation_ror"
author3_from_names <- oa_fetch(entity = "author", display_name = c("Bekir Tanriover", "Ahlam Saleh") ) ### "search" syntax allows fuzzy search for middle name
filtered_author3 <- subset(author3_from_names, grepl("https://ror.org/03m2x1q45", affiliation_ror, ignore.case=TRUE))

##### Testing with multiple authors
unit_authors_names <- list( "Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh")
unit_authors_list <- list()

# 1. Filter author one by one using his/her name using fuzzy search option
for (unit_author in unit_authors_names) {
  # find an author's name using fuzzy search
  unit_author_from_names <- oa_fetch(entity = "author", search = unit_author)
  # create an empty df
  unit_author <- data.frame()
  # filter author based on his/her affiliation_display_name
  unit_author <- subset(unit_author_from_names, grepl("University of Arizona", affiliation_display_name, ignore.case=TRUE))

  # Append the author (df) as an element to the list
  unit_authors_list <- append(unit_authors_list, list(unit_author))

  # Clear unit_author
  unit_author <- NULL

}

# 2. Output

for (i in 1:length(unit_authors_list)) {
 print(unit_authors_list[[i]])
  # Loop through each element in the author list to calculate the sum of publications
  author_works_count <-sum (unit_authors_list[[i]]$works_count)
  output_string <- "Researcher" + unit_authors_list[[i]]$display_name + "Year" + counts_by_year

  }

