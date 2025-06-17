# For Saleh Ahlam 
# Author: Yan Han with assistant ChatGPT 4o


# install.packages("readr")  # Uncomment if you don't have the readr package
# install.packages("httr")
# install.packages("jsonlite")



library(openalexR)
library(dplyr)
library(tidyverse)
library(writexl)


library(readr)
library(httr)
library(jsonlite)



##### Fetch directly from DOI

# Step 1: Read the CSV file
setwd("/home/yhan/Documents/openalexR-test/")
file_path <- "2024-10-29.csv"  # Replace with your actual file path
data <- read_csv(file_path)

# Step 2: Extract the "DOI" column and remove empty cells
doi_column <- data$DOI[!is.na(data$DOI) & data$DOI != ""]

# Example list of DOIs (replace this with your DOI vector from the CSV)
#doi_column <- c("10.1007/s11069-020-03875-3", "10.1111/ssqu.12699")

doi_column <- doi_column[1:100]

# Remove NA values from doi_column
doi_column <- na.omit(doi_column)

print(doi_column)



# A more flexible pattern for DOIs
doi_pattern <- "^10\\.\\d{4,9}/.+$"

# Identify which DOIs are invalid
rm(invalid_dois)
invalid_dois <- doi_column[!grepl(doi_pattern, doi_column)]
print(invalid_dois)

#### Fetch from OpenAlex
works_from_dois <- oa_fetch(
  entity = "works", 
  doi = doi_column,
  verbose = TRUE)

write_xlsx(works_from_dois, "UMC/openalex_doi_data1.xlsx")






#### Fetch DOIs directly from Crossref

get_doi_metadata_org <- function(doi) {
  # Format the URL for the CrossRef API request
  url <- paste0("https://api.crossref.org/works/", doi)
  
  # Make the GET request
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    # Return the metadata part of the response
    return(data$message)
  } else {
    warning(paste("Failed to fetch metadata for DOI:", doi))
    return(NULL)
  }
}

get_doi_metadata <- function(doi) {
  # Format the URL for the CrossRef API request
  url <- paste0("https://api.crossref.org/works/", doi)
  
  attempt <- 1
  max_attempts <- 3
  
  while (attempt <= max_attempts) {
    response <- tryCatch({
      GET(url)
    }, error = function(e) {
      message("Error fetching DOI:", doi, " - ", e)
      return(NULL)
    })
    # Check if the response was successful and not NULL
    if (!is.null(response) && status_code(response) == 200) {
      data <- fromJSON(content(response, "text", encoding = "UTF-8"))
      return(data$message)
    } else if (!is.null(response) && status_code(response) != 200) {
      warning(paste("Failed to fetch metadata for DOI:", doi, "Status code:", status_code(response)))
      return(NULL)
    } else {
      message(paste("Retrying DOI:", doi, " - Attempt", attempt))
      attempt <- attempt + 1
      Sys.sleep(1)  # Short delay before retrying
    }
  }
  
  message("Failed to retrieve metadata for DOI after ", max_attempts, " attempts: ", doi)
  return(NULL)
}

# Create an empty list to store metadata
metadata_list <- list()
metadata_list <- lapply(doi_column, get_doi_metadata)

str(metadata_list[[1]])  # Check the structure of the first element

# Convert metadata_list into a structured data frame
metadata_df <- bind_rows(lapply(cleaned_metadata_list, function(x) {
  if (is.list(x) && !is.null(x)) {
    tryCatch({
      data.frame(
        doi = ifelse(!is.null(x$DOI), x$DOI, NA),
        title = ifelse(!is.null(x$title), x$title, NA),
        abstract = ifelse(!is.null(x$abstract), x$abstract, NA),  # Corrected abstract field
        author = ifelse(!is.null(x$author),
                        paste(x$author$family, collapse = ", "),
                        NA),
        published_date = ifelse(!is.null(x$`published-print`$`date-parts`),
                                paste(x$`published-print`$`date-parts`[[1]], collapse = "-"),
                                NA),
        journal = ifelse(!is.null(x$`short-container-title`), x$`short-container-title`, NA),
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        doi = NA, title = NA, abstract = NA, author = NA, published_date = NA, journal = NA
      )
    })
  } else {
    NULL
  }
}))


# Ensure metadata_list contains only list elements
cleaned_metadata_list <- Filter(function(x) is.list(x) && !is.null(x), metadata_list)


# Check rows with missing DOI, author, or title
na_doi <- metadata_df[is.na(metadata_df$doi), ]

# Print the rows with missing values
print("Rows with missing DOI:")
print(na_doi)

# Example: Check the API response for a specific DOI with a missing value
specific_doi <- doi_column[41]  # Replace with a specific DOI
response_data <- get_doi_metadata(specific_doi)
print(response_data)


