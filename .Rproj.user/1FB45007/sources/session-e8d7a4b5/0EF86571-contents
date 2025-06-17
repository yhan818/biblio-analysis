#### Collaboration analysis
######## Author: Yan Han with help of Gemini / Cursor
######## Updated: June 12, 2025
##### Comment style: ##### (5#: like H1, Step) 
#####                 ### (3#: like H2, sub-step)
#####
##### Analyze an institution authors' and his/her co-authors nation and institutions 
# OpenAlex R Documentation: https://github.com/ropensci/openalexR
install.packages("dplyr")
install.packages("tidyverse")
install.packages("countrycode")

library(openalexR)
packageVersion("openalexR")
library(jsonlite)
library(dplyr)
library(tidyverse)

library(openxlsx)
library(writexl)
# free unused obj to manage memory
rm(list=ls())
gc()

options("max.print" = 100000)
options (openalexR.mailto="yhan@arizona.edu")
getwd()

source("my_functions.R")

works_published_2024 <- readRDS("../works_published_2024.rds")

works_published_2022 <- readRDS("../works_published_2022.rds")

works_published_2021 <- readRDS("../works_published_2021.rds")

works_published_2020 <- readRDS("../works_published_2020.rds")


works_published <- works_published_2024

##### Steps:
##### Step 1: filter by type = article 
works_published_type_articles    <- works_published %>% filter(type == "article")
works_published_type_nonarticles <- works_published %>% filter(type != "article")

##### Step 2: Get the authors of the works and related topic-subfield-field-domain structure
# including primary_topic_display_name, primary_topic_subfield_display_name, primary_topic_field_display_name, and primary_topic_domain_display_name
#
# works_published_type_article_authors <- works_published_type_article %>% select(id, doi, title, publication_date, so, host_organization, author, type, referenced_works, topics)

##### Step 2: Identify multi-author papers
works_published_type_articles_authors <- works_published_type_articles %>%
  mutate(
    author_count = map_int(author, nrow), # Get the number of rows in each nested df
    has_multiple_authors = author_count > 1 # Check if there are multiple rows
  )

# 2025-06-15: works change field name "author" to "authorship". 
# "institution_country_code" changed to affiliation > country_code"
works_published_type_articles_authors <- works_published_type_articles %>%
  mutate(
    author_count = map_int(authorships, nrow), # Get the number of rows in each nested df
    has_multiple_authors = author_count > 1 # Check if there are multiple rows
  )

##### Step 3: Split it into two works df: US and nonUS authors
# works_published_multi_authors has a new column:
### Step 3.1: nonus_author: TRUE if at least one author is from a non-US country, FALSE otherwise.
works_published_type_articles_authors_nonus <- works_published_type_articles_authors %>%
  mutate(
    nonus_author = map_lgl(authorships, function(author_df) {
      if (nrow(author_df) == 0) {
        return(FALSE) # No authors, so no non-US authors
      } else if (!"affiliations" %in% names(author_df)) {
        warning("Nested author data frame missing 'affilliations' and 'country_code' column.")
        return(FALSE) # Missing column, assume no non-US authors
      } else { ### error
        any(author_df$affiliations$country_code != "US" & !is.na(author_df$affiliations$country_code))
      }
    })
  ) %>% 
  filter(nonus_author)


works_published_type_articles_authors_nonus <- works_published_type_articles_authors %>%
  mutate(
    nonus_author = map_lgl(author, function(author_df) {
      if (nrow(author_df) == 0) {
        return(FALSE) # No authors, so no non-US authors
      } else if (!"institution_country_code" %in% names(author_df)) {
        warning("Nested author data frame missing 'institution_country_code' column.")
        return(FALSE) # Missing column, assume no non-US authors
      } else {
        any(author_df$institution_country_code != "US" & !is.na(author_df$institution_country_code))
      }
    })
  ) %>% 
  filter(nonus_author)

### Step 3.2: US authors only (collaboration within US institutions). For future use. 
works_published_type_authors_us 


##### Step 4: Use NonUS author works to figure out collaboration.
head(works_published_type_articles_authors_nonus)

### Step 4.1: Add country codes summary for each work
# It uses unique(), so only unique countries will be outputed. 
works_published_w_country_codes <- works_published_type_articles_authors_nonus %>%
  mutate(
    country_codes_summary = map(author, function(author_df) {
      if ("institution_country_code" %in% names(author_df)) {
        codes <- unique(author_df$institution_country_code[!is.na(author_df$institution_country_code)])
        return(paste(sort(codes), collapse = ", "))
      } else {
        return(NA_character_)
      }
    })
  ) %>%
  select(id, title, country_codes_summary, everything())

# Display the results
head(works_published_w_country_codes %>% select(title, country_codes_summary))

### Step 4.2: Find out Country_code = NA (openAlex data has no country code). 
library(dplyr)

works_with_missing_country_code <- works_published_w_country_codes %>%
  mutate(has_missing_country_code = map_lgl(author, function(author_df) {
    if ("institution_country_code" %in% names(author_df)) {
      any(is.na(author_df$institution_country_code))
    } else {
      TRUE # Or FALSE, depending on how you want to treat missing column
    }
  })) %>%
  filter(has_missing_country_code)

print("Works with at least one author having missing country code:")
print(works_with_missing_country_code)

#### Step 5: Working on a specific country

### Step 5.1: Change country code here: "IN", "MX", 
# AU= Australia, IN = India, MX = Mexico, KZ = Kazakhstan
# Create a variable for the target country code. change this value for a different county
# Looks like 2024: using API India: 140 vs web interface: 130 ?? 

target_country_code <- "AU"

works_published_ua_country <- works_published_w_country_codes %>%
  filter(
    map_lgl(author, function(author_df) {
      target_country_code %in% author_df$institution_country_code
    })
  )


##### Step 6: Find the dept/unit within the University of Arizona. 
### From here, we can find out the author relationship between UA authors and other country authors. 
# extract U of Arizona au_affiliation_raw from each work: So that we know which dept/author with that country dept/author

### Well. no complete data

### Step 6: Topics: Primary topic 
# primary_topics <- extract_topics_by_level(works_cited_type_articles_publisher, 1)

# Check if parameters exist and are valid
if (!exists("works_published_ua_country")) {
  stop("Required data frame ' not found")
}

### Step 6.1: Extract primary_topic_subfield_field_domain out of Topics column. 
works_published_ua_country_topics <- tryCatch({
  extract_topics_by_level(works_published_ua_country, 1)
}, error = function(e) {
  message("Error extracting topics: ", e)
  return(NULL)
})

head(works_published_ua_country_topics)

##### Step 7: Unique institutions at that country
##### We need two pass to read the data. 

### Step 7.1: Find distinct institutions in that country
target_country_code <- "AU"  # Change this value for different countries

# Function: find distinct_institutions. 
# Need to handle "NA" value, as openAlex data country code can be "NA". "NA" value interacts with logic condition. 
find_distinct_institutions <- function(articles_df, target_country_code, debug = FALSE) {
  distinct_institutions <- new.env(hash = TRUE)
  
  if (debug)
    message("DEBUG: Processing articles_df.")
  
  for (i in 1:nrow(articles_df)) {
    article <- articles_df[i,]
    author_data <- article$author
    
    if (debug)
      message("DEBUG: author_data class for row ", i, ": ", class(author_data))
    
    if (is.list(author_data) && length(author_data) > 0 && is.data.frame(author_data[[1]])) {
      authors <- author_data[[1]]
      if (debug)
        message("DEBUG: authors extracted as list of data frame for row ", i, ".")
    } else if (is.data.frame(author_data)) {
      authors <- author_data
      if (debug)
        message("DEBUG: authors extracted as data frame for row ", i, ".")
    } else {
      if (debug)
        message("DEBUG: Unexpected author data type for row ", i, ".")
      next
    }
    
    if (debug)
      message("DEBUG: Number of authors for row ", i, ": ", nrow(authors))
    
    if (nrow(authors) > 0) {
      for (j in 1:nrow(authors)) {
        author <- authors[j,]
        country_code <- author$institution_country_code
        institution_name <- author$institution_display_name
        
        if (debug) {
          message("DEBUG: Country code for row ", i, ", author ", j, ": ", country_code)
          message("DEBUG: Institution name for row ", i, ", author ", j, ": ", institution_name)
        }
        
        if (!is.null(country_code) && !is.na(country_code) && tolower(country_code) == tolower(target_country_code) && !is.null(institution_name)) {
          distinct_institutions[[institution_name]] <- TRUE
          if (debug)
            message("DEBUG: Institution added for row ", i, ", author ", j, ": ", institution_name)
        } else {
          if(debug){
            message("DEBUG: Institution not added for row ", i, ", author ", j, ". Country code: ", country_code, ", Institution name: ", institution_name)
          }
        }
      }
    }
  }
  
  return(ls(distinct_institutions))
}

# Test
first_row <- works_published_ua_country[1, ]
result <- find_distinct_institutions(first_row, "US") # Replace "us" with your target
print(result)


# Example Usage: Find distinct institutions from India

#country_institutions <- find_distinct_institutions(works_published_ua_country, "US") # Replace "us" with your target
country_institutions <- find_distinct_institutions(works_published_ua_country, "AU") # Replace "AU" with your target
print(country_institutions)

##### Step 8: Count 
  ### Step 8.1: Distinct works: count only once even if an institution appears 50 times in a work.

count_institution_works_with_works_id <- function(articles_df, target_country_code, debug = FALSE) {
  institution_data <- data.frame(institution = character(), works = integer(), work_ids = character())
  
  if (debug)
    message("DEBUG: Starting count_institution_works_with_works_id.")
  
  for (i in 1:nrow(articles_df)) {
    article <- articles_df[i, ]
    author_data <- article$author
    work_id <- as.character(article$id) # Ensure character type
    
    if (debug) {
      print(paste("DEBUG: article$id (raw) =", article$id))
      print(paste("DEBUG: article$id (class) =", class(article$id)))
      print(paste("DEBUG: work_id =", work_id, "for row", i))
    }
    
    article_institutions <- new.env(hash = TRUE)
    
    if (is.list(author_data) && length(author_data) > 0 && is.data.frame(author_data[[1]])) {
      authors <- author_data[[1]]
    } else if (is.data.frame(author_data)) {
      authors <- author_data
    } else {
      if (debug)
        message(paste("DEBUG: Unexpected author data for row", i))
      next
    }
    
    if (nrow(authors) > 0) {
      for (j in 1:nrow(authors)) {
        author <- authors[j, ]
        country_code <- author$institution_country_code
        institution_name <- author$institution_display_name
        
        if (debug) {
          message(paste("DEBUG: Country code for row", i, ", author", j, ":", country_code))
          message(paste("DEBUG: Institution name for row", i, ", author", j, ":", institution_name))
        }
        
        if (!is.null(country_code) && !is.na(country_code) && tolower(country_code) == tolower(target_country_code) && !is.null(institution_name)) {
          article_institutions[[institution_name]] <- TRUE
        }
      }
    }
    
    for (inst in ls(article_institutions)) {
      if (inst %in% institution_data$institution) {
        row_index <- which(institution_data$institution == inst)
        if (debug)
          print(paste("DEBUG: Before adding, work_id =", work_id))
        institution_data$works[row_index] <- institution_data$works[row_index] + 1
        institution_data$work_ids[row_index] <- paste(institution_data$work_ids[row_index], work_id, sep = "; ")
        if (debug) {
          print(paste("DEBUG: After adding, work_ids for", inst, ":", institution_data$work_ids[row_index]))
          message(paste("DEBUG: Institution count incremented for", inst, ", work_id added"))
        }
      } else {
        if (debug)
          print(paste("DEBUG: Before adding, work_id =", work_id))
        new_row <- data.frame(institution = inst, works = 1, work_ids = work_id)
        institution_data <- rbind(institution_data, new_row)
        if (debug) {
          print(paste("DEBUG: After adding, work_ids for", inst, ":", institution_data$work_ids[nrow(institution_data)]))
          message(paste("DEBUG: Institution added for", inst, ", work_id added"))
        }
      }
    }
    
    if (debug) {
      print("DEBUG: institution_data after processing row:")
      print(str(institution_data)) # Print structure
    }
  }
  
  if (debug)
    message("DEBUG: Ending count_institution_works_with_works_id.")
  
  print("DEBUG: Final institution_data structure:")
  print(str(institution_data))
  return(institution_data)
}


### Step 8.2: total appearances: count the Number of Works an Institution Appears regardless one institution can appear 5 times in a work 
count_institution_appearances_with_works_id <- function(articles_df, target_country_code, debug = FALSE) {
  institution_data <- data.frame(institution = character(), appearances = integer(), work_ids = character())  # Changed "works" to "appearances"
  
  if (debug)
    message("DEBUG: Starting count_institution_appearances_with_works_id.")
  
  for (i in 1:nrow(articles_df)) {
    article <- articles_df[i, ]
    author_data <- article$author
    work_id <- as.character(article$id)  # Ensure character type
    
    if (debug) {
      print(paste("DEBUG: article$id (raw) =", article$id))
      print(paste("DEBUG: article$id (class) =", class(article$id)))
      print(paste("DEBUG: work_id =", work_id, "for row", i))
    }
    
    if (is.list(author_data) && length(author_data) > 0 && is.data.frame(author_data[[1]])) {
      authors <- author_data[[1]]
    } else if (is.data.frame(author_data)) {
      authors <- author_data
    } else {
      if (debug)
        message(paste("DEBUG: Unexpected author data for row", i))
      next
    }
    
    if (nrow(authors) > 0) {
      for (j in 1:nrow(authors)) {
        author <- authors[j, ]
        country_code <- author$institution_country_code
        institution_name <- author$institution_display_name
        
        if (debug) {
          message(paste("DEBUG: Country code for row", i, ", author", j, ":", country_code))
          message(paste("DEBUG: Institution name for row", i, ", author", j, ":", institution_name))
        }
        
        if (!is.null(country_code) && !is.na(country_code) && tolower(country_code) == tolower(target_country_code) && !is.null(institution_name)) {
          if (institution_name %in% institution_data$institution) {
            row_index <- which(institution_data$institution == institution_name)
            institution_data$appearances[row_index] <- institution_data$appearances[row_index] + 1  # Changed "works" to "appearances"
            institution_data$work_ids[row_index] <- paste(institution_data$work_ids[row_index], work_id, sep = "; ")
            if (debug) {
              print(paste("DEBUG: Institution count incremented for", institution_name, ", work_id added:", work_id))
              print(paste("DEBUG: work_ids for", institution_name, ":", institution_data$work_ids[row_index]))
            }
          } else {
            if (debug)
              print(paste("DEBUG: Adding new institution:", institution_name, ", work_id:", work_id))
            new_row <- data.frame(institution = institution_name, appearances = 1, work_ids = work_id)  # Changed "works" to "appearances"
            institution_data <- rbind(institution_data, new_row)
            if (debug)
              message(paste("DEBUG: Institution added for", institution_name, ", work_id added:", work_id))
          }
        }
      }
    }
  }
  
  if (debug)
    message("DEBUG: Ending count_institution_appearances_with_works_id.")
  
  return(institution_data)
}



# Test case: Distinct works with work IDs (using "id") for the first row, with debug
first_row <- works_published_ua_country_topics[1, , drop = FALSE] # Extract the first row as a data frame
inst_work_counts_with_works_id_debug <- count_institution_works_with_works_id(first_row, "US")
head(inst_work_counts_with_works_id_debug)

inst_total_counts_with_works_id <- count_institution_appearances_with_works_id(works_published_ua_country_topics, "AU")
inst_total_counts_with_works_id <- inst_total_counts_with_works_id %>%
  arrange(desc(appearances))
print(inst_total_counts_with_works_id)

inst_work_counts_with_works_id <- count_institution_works_with_works_id(works_published_ua_country_topics, "AU")
inst_work_counts_with_works_id <- inst_work_counts_with_works_id %>%
  arrange(desc(works))
print(inst_work_counts_with_works_id)

# Write out to Excel 
#write_df_to_excel(inst_total_counts_with_works_id, "collaborations/")

write_df_to_excel(inst_work_counts_with_works_id, "collaborations/")


# 2. Combine Excel Files
#excel_files <- c("collaborations/inst_total_counts_with_works_id.xlsx", "collaborations/inst_work_counts_with_works_id.xlsx")

excel_files <- c("collaborations/inst_work_counts_with_works_id.xlsx")

tryCatch({
  wb <- createWorkbook()
  
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("collaborations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  
  saveWorkbook(wb, "collaborations/AU_institution_counts_combined_2021_v1.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
  
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})


### 2025-06-16: UA- AU collaboration
# 2021: UA articles (API: 7,058, Web: 7,061, output: 7,018). Some off? why?   
#       UA-AU: 302, web: 335, 26/335 do not have authorship data. but some of these 26 articles' data is available from API. For example, https://doi.org/10.1016/j.ajhg.2021.08.003
# 
# 2024: UA (API: 6,225, Web: 6,224. output in Excel: 6199)
#       UA-AU: API: 247; web 263; 10/263 do not have authorship data. Excel output has limitations (special chars, etc ) For example, Fourâ€dimensionalâ€STEM analysis 
#      one example: https://api.openalex.org/works/W4396615866  (one author: 2 affiliations Anne Medling Australia and U of Toledo, USA)
#      https://api.openalex.org/works/W4396696608 (D. R. Russell. one author two affiliations. )
#      https://api.openalex.org/works/W4395070473 (Mike Martin. One author 5 affiliations)
#      https://openalex.org/W4403824371  

# Testing the difference
# open web downloaded Excel data
library(readxl)
file_path <- "../UAauthorship-2024.xlsx"
web_data <- read_excel(file_path, sheet = "AU and UTS")

dois_only_in_web_data <- setdiff(web_data$doi, works_published_ua_country$doi)
cat("IDs found only in full_data:\n")
print(dois_only_in_web_data)

dois_only_in_API_df <- setdiff(works_published_ua_country$doi, web_data$doi)
cat("\nIDs found only in another_df:\n")
print(dois_only_in_API_df)

common_dois <- intersect(web_data$doi, works_published_ua_country$doi)
cat("\nIDs common to both data frames:\n")
print(common_dois)
