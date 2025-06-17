###################### 2. Using found openAlex IDs to get publications #####
######## Author: Yan Han
######## Date: June 5, 2023
######## Updated: Junly 11, 2024
##### Search authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR
# OpenAlex Beta explorer: https://explore.openalex.org/ (the explorer seems not to display all the possible researchers. In ohter words, You shall use API
# The explorer can be only used as a verification/testing purpose!!!

install.packages("remotes")
remotes::install_github("ropensci/openalexR", force=TRUE)

#install.packages("openalexR")  
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("knitr")
install.packages("testthat")

# common libraries
library(openalexR)
packageVersion("openalexR")

library(dplyr)
library(ggplot2)
library(knitr)
library(testthat)
library(readr)

citation("openalexR")

# check to see if openAlexR has the latest entities in OpenAlex (OpenAlex updated its data model(Entities) in June 2023)
# Before April 2023: they are [1] "works"        "authors"      "venues"       "institutions" "concepts"    
# Aug 2024: [1] "works"        "authors"      "institutions" "concepts"     "funders"      "sources"      "publishers"   "topics"      

oa_entities()

options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test")


### Test data
test_data_UAL_authors     <- c("Yan Han", "Ellen Dubinski", "Fernando Rios", "Ahlam Saleh")
test_data_COM_authors     <- c("Phillip Kuo", "Bekir Tanriover", "Ahlam Saleh")
test_data_COM_Tucson_authors <- c("Che Carrie Liu", "Robert M. Aaronson", "Alexa Aasronson", "Mohammed Abbas", "")
test_data_science_authors <- c("Marek Rychlik", "Ali Bilgin", "Beichuan Zhang")
test_data_ischool_authors <- c("Hong Cui")
test_data_others          <- c("Leila Hudson", "Mona Hymel")
test_data_not_updated_authors <-c("Karen Padilla", "Haw-chih Tai")

test_data_affiliation <- c("University of Arizona")
test_data_year <- c("2022", "2021", "2020", "2012")

# Test works 
works_from_dois <- oa_fetch(entity = "works", doi = c("https://doi.org/10.1093/ofid/ofac186", "https://doi.org/10.1007/s11192-013-1221-3"),  verbose = TRUE)

### Testing three datasets citations recall and precision using one article (published in 2022)
### OpenAlex: Precision 
### OpenCitaitons: Precision 100%. Recall: 2/3 
### Google scholar: Precision 100%. Recall 100%
works_from_dois <- oa_fetch(entity = "works", doi = c("https://doi.org/10.6017/ital.v40i1.12553"),  verbose = TRUE)
works_from_dois$cited_by_api_url
works_from_dois$ids

##################################################################################3
#### Location info:
### 1. Documentation: https://docs.openalex.org/api-entities/works/filter-works 
### 2. three options: best_oa_location, locations and preimary_location

### UA institution       ID: "https://openalex.org/I138006243"
### UA campus repository ID:"https://openalex.org/S2764879211"
## 
# Example : primarly_location and best _oa_location: https://api.openalex.org/works/W4300666610 <-> https://repository.arizona.edu/handle/10150/612997
# Example 2 : best_oa_location: https://openalex.org/W2585292421   <->  https://repository.arizona.edu/handle/10150/623545  


# All locations: 
# count: 14903 (2024-07-11)
UA_host_all_location <- oa_fetch (
  entity = "works",
  locations.source.host_organization = "https://openalex.org/I138006243",
  #count_only = TRUE
)

# Best OA location. find out host organization. 
# count: 8394 (2024-07-11)
# This fetch will take a few minutes. So be patient . 
UA_host_best_location <- oa_fetch(
  entity = "works",
  # UA campus repository ID does not work as a filter
  best_oa_location.source.host_organization = "https://openalex.org/I138006243",

  # If only need count, uncomment the below line for a quick run.   
  count_only = TRUE
  # If only need some samples. using the below line.
  # options = list(sample = 100, seed = 1)
)

# Primary_location.source.host_organization. 
# count: 24 (2024-07-11)
UA_host2 <- oa_fetch (
  entity = "works",
  primary_location.source.host_organization = "https://openalex.org/I138006243",
  count_only = TRUE
)

############### Use campus repository source ID. 
# no result using UA campus repository source ID. 
UA_host5 <- oa_fetch (
  entity = "works",
  best_oa_location.source.host_organization = "https://openalex.org/S2764879211",
  count_only = TRUE
)

UA_host6 <- oa_fetch (
  entity = "works",
  locations.source.host_organization = "https://openalex.org/S2764879211",
  count_only = TRUE
)


# Filter the dataframe to get all rows where "so" is "journal of range management" (JRM) (case insenstive)
df_jrm <- subset(UA_host_all_location, grepl("journal of range management", so, ignore.case = TRUE))  #4,183

# count_jrm <- nrow(df_jrm) + nrow (df_rem)

# Count the number of occurrences of each unique value in the "source" column using dplyr
source_counts_df <- UA_host_all_location %>%
  count(so, sort = TRUE)

# Display the dataframe with counts
# JRM/REM:  4183+836 (3991 + 804 for best_oa) vs UA (6,029)
# Rangelands: 782 (685 for best_oa) vs UA (1,160)

print(source_counts_df)

library(dplyr)
(select(UA_host_all_location, 'id', 'so'))

## counting DOIs 
# Count rows with DOIs
count_with_doi <- UA_host_all_location %>% filter(grepl("doi.org", url, ignore.case = TRUE)) %>% nrow()

# there is only 261 items having doi:10.2458  
# most are JRM, rangelands, radiocarbon, and JPE
# some of the URLs have "uair.arizona.edu" (possibly harvested from crossref)
ua_doi1 <- UA_host_all_location %>% filter(grepl("10.2458", url, ignore.case = TRUE)) 
count2 <- ua_doi1%>% nrow()

# 10.1016 from Elsevier DOI for its journals : more than REM (432 obj)
rem_doi <- UA_host_all_location %>% filter(grepl("10.1016", url, ignore.case=TRUE))


### get metadata for a DOI
install.packages("rcrossref")
library(rcrossref)
library (httr)
library(jsonlite)
# Function to get metadata for a DOI
get_doi_metadata <- function(doi) {
  url <- paste0("https://api.crossref.org/works/", doi)
  response <- GET(url)
  
  if (status_code(response) == 200) {
    metadata <- content(response, as = "text", encoding = "UTF-8")
    metadata <- fromJSON(metadata, flatten = TRUE)
    return(metadata$message)
  } else {
    message("Error retrieving metadata: ", status_code(response))
    return(NULL)
  }
}

#### Example 1: More like openalex pulled directly from crossref. 
###  Campus repo: 
doi <- "10.2458/v24i1.22003"
metadata <- get_doi_metadata(doi)
if (!is.null(metadata)) {
  print(metadata)
} else {
  print("No metadata found for the given DOI.")
}

### Example 2: More like openAlex pulled directly from crossref by checking DOI's metadata (which do not contain other DOIs, and have date info 2006
### UA: https://repository.arizona.edu/handle/10150/643523 (it contains other DOIs. also issue date as 2004.  have additional link rangelands.org
doi <- "https://doi.org/10.2458/azu_jrm_v57i2_cox"
metadata <- get_doi_metadata(doi)
if (!is.null(metadata)) {
  print(metadata)
} else {
  print("No metadata found for the given DOI.")
}

### Example 3:
# https://doi.org/10.1038/ng.3667" 
doi <- "https://doi.org/10.1038/ng.3667"
metadata <- get_doi_metadata(doi)
if (!is.null(metadata)) {
  print(metadata)
} else {
  print("No metadata found for the given DOI.")
}

################################ Coding not working ####################3

# code generated by GPT-4
# check "is_oa_anywhere" field
# Set the filter for works that are open access anywhere
oa_filter <- "is_oa_anywhere:true"

# Search for works with the specified filter
results <- oa_search(
  entity = "works",
  filter = oa_filter,
  per_page = 100  # Adjust per_page as needed
)

# code generated by Gemini
# Set your API key (replace "YOUR_API_KEY" with your actual key)
oa_auth(api_key = "YOUR_API_KEY")
# Define the search parameters
filters <- list(is_oa_anywhere = TRUE)
# Fetch publications with the specified filters
publications <- oa_fetch(filters = filters)

# Print the first 10 publications (or adjust the number as desired)
print(publications[1:10, ])

############################################################################


# Check the number of results retrieved
total_results <- results$meta$count
print(paste("Total number of works with 'is_oa_anywhere' tag:", total_results))

# View the first few results
head(results$data)

# To see is_oa field

# use "search" option with no middle name 
author_from_names <- oa_fetch(entity = "author", search = test_data_COM_authors[2]) 

# first checking if author exists
if (!is.null(author_from_names)) {
  print("author found")
  # Filter out not "University of Arizona" authors using "affiliation_display_name" column.
  # other filtering fields can be "affiliation_id", "affiliation_ror"
  filtered_authors <- subset(author_from_names, grepl("University", affiliation_display_name, ignore.case=TRUE))
  if (nrow(filtered_authors) == 0 )  {
    print("The filtered_authors dataframe is empty")
  }
}
# print openalex ID
print(filtered_authors)
print(filtered_authors[1][1])

filtered_authors <- author_from_names[1][1]


## Testing 
# 2023-08: openalex changed id to id[1]. it was id[2]
openalex_author_id <- filtered_authors$id[1]
author_works <- oa_fetch ( entity = "works",
                           author.id=(openalex_author_id),
                           verbose = TRUE )
object_type <- class(author_works)
print(object_type)

# Filter publications after year 2010
filtered_author_works <- author_works %>%
  filter(substr(publication_date, 1,4) >"2010")

# Filter source of work
so_values <- filtered_author_works$so

#convert author_works$author (list) to , separated by "," using sapply
# List "author" contains all the authors info
filtered_author_works$author <- sapply(filtered_author_works$author, paste, collapse = ",")
# List "ids" contains openAlexId, doi, and pmid
filtered_author_works$ids <- sapply(filtered_author_works$ids, paste, collapse = ",")
# List "referenced_works" contains references
filtered_author_works$referenced_works <- sapply(filtered_author_works$referenced_works, paste, collapse = ",")
# List "related_works" contains related works
filtered_author_works$related_works <- sapply(filtered_author_works$related_works, paste, collapse = ",")

filtered_author_works$counts_by_year <- NULL
filtered_author_works$concepts <- NULL

field_names <-colnames(filtered_author_works)
print(field_names)


# 2023-08: "relevance_score" removed
selected_columns <- filtered_author_works[c("id", "display_name", "author", "ab", "publication_date", "so", "so_id", "host_organization", "issn_l", "url"
                                            , "pdf_url", "license", "version", "first_page", "last_page", "volume", "issue", "is_oa", "cited_by_count", "publication_year"
                                            ,"cited_by_api_url", "ids", "doi", "type", "referenced_works", "related_works")]
write.csv(selected_columns, file = "author_works.csv", row.names = TRUE)

# get work
works1 <- "W4213067910"
test1 <- oa_fetch(
  identifier = works1,
  entity = "works"
)
print(works1)

## Questions for openAlex
# ORCID search not returning correct result. see Yan as an example
# affiliation shall use ORCID's employment first, then the most recent publication source. see Bekir as example


# Testing multiple authors
openalex_ids_authors <- c("A4353685810", "A4354460443")

# Create an empty list to store the downloaded data for each author
authors_data <- list()

# Iterate over the author IDs and download data for each
for (id in openalex_ids_authors) {
  authors_data[[id]] <- oa_fetch(entity = "author", openalex = id)
}

# 3. Now that we have data in list, iterate over the list elements
for (author_data in authors_data) {
  #print(author_data)
  print(author_data$works_count)
  print(author_data$counts_by_year)
  
}

