
######## Author: Yan Han with help of Gemini / Cursor
####### Created: Sep 8, 2025
####### Updated: Sep 8, 2025
##### Comment style: ##### (5#: like H1, Step) 
#####                 ### (3#: like H2, sub-step)
#####
##### Analyze who have cited works by UArizona authors. 
# an institution authors' and his/her co-authors nation and institutions 
# OpenAlex R Documentation: https://github.com/ropensci/openalexR
install.packages("dplyr")
install.packages("tidyverse")
install.packages("countrycode")

library(openalexR)
packageVersion("openalexR")
library(jsonlite)
library(dplyr)
library(tidyverse)

library(httr)
library(openxlsx)
library(writexl)

# free unused obj to manage memory
rm(list=ls())
gc()

options("max.print" = 100000)
options (openalexR.mailto="yhan@arizona.edu")
getwd()

# OpenAlex ID for the University of Arizona
ua_ror_id <- "03m2x1q45"

# --- Function to get recent works from an institution using openalexr ---
get_institution_works <- function(institution_ror, from_date, to_date) {
  message(paste("Fetching all works from ROR ID:", institution_ror, "between", from_date, "and", to_date))
  message("Note: This may take a moment as 'openalexr' retrieves all publications in the date range.")
  
  # Use oa_fetch to get works within a specific publication date range.
  # The openalexr package handles pagination automatically to get all results.
  works_df <- oa_fetch(
    entity = "works",
    institutions.ror = institution_ror,
    from_publication_date = from_date,
    to_publication_date = to_date,
    output = "tibble" # Ensure output is a tibble
  )
  
  # Check if any works were returned
  if (is.null(works_df) || nrow(works_df) == 0) {
    message("No works found for this institution in the specified date range.")
    return(NULL)
  }
  
  return(works_df)
}

# --- Function to fetch citing authors for a SINGLE work (passed as a row) ---
get_citing_authors <- function(work) {
  work_id <- work$id
  work_title <- work$display_name
  message(paste("Fetching citing works for:", work_title, "(ID:", work_id, ")"))
  
  if (is.null(work) || nrow(work) == 0) {
    # Return NULL if no citing works are found
    return(NULL)
  }
  
  # Process the downloaded data to extract author and institution information
  work %>%
    select(id, display_name, authorships) %>%
    unnest(authorships) %>%
    unnest(institutions) %>%
    select(
      citing_work_id = id,
      citing_work_title = display_name,
      author_name = display_name.author,
      author_orcid = orcid,
      institution_name = display_name.institutions,
      institution_country = country_code,
      institution_type = type
    ) %>%
    # Add the original work ID and title to know which paper was cited
    mutate(cited_source_work_id = work_id,
           cited_source_work_title = work_title)
}



#' Fetch Citing Works from OpenAlex API
#' This function retrieves a list of all work IDs from OpenAlex that cite a specified OpenAlex work ID. It handles API pagination using the cursor method.
#'
#' @param openalex_id The full OpenAlex ID of the work (e.g., "W4391067309").
#' @param mailto Your email address for polite API access.
#'
#' @return A character vector of OpenAlex work IDs. Returns an empty vector
#'   if no citing works are found or if an error occurs.
#'
get_citing_works <- function(openalex_id, mailto = "yhan@arizona.edu") {
  # --- 1. Setup API Request ---
  base_url <- "https://api.openalex.org/works"
  filter_value <- paste0("cites:", openalex_id)
  
  params <- list(
    filter = filter_value,
    mailto = mailto, 
    per_page = 200, # Max allowed per page
    cursor = "*"    # Initial cursor for the first page
  )
  
  all_work_ids <- c() # Initialize an empty vector to store IDs
  
  # --- 2. Loop Through Paginated Results ---
  while (!is.null(params$cursor)) {
    cat(paste("Fetching page with cursor:", params$cursor, "\n"))
    # Make the API GET request
    response <- GET(url = base_url, query = params)
    
    # Check for a successful response
    if (http_status(response)$category != "Success") {
      warning("API request failed: ", http_status(response)$reason)
      return(invisible(c())) # Return an empty vector on failure
    }
    
    # Parse the JSON content
    content <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(content)
    
    # --- 3. Extract IDs and Prepare for Next Loop ---
    # Extract the work IDs from the current page
    if (length(data$results) > 0 && "id" %in% names(data$results)) {
      all_work_ids <- c(all_work_ids, data$results$id)
    }
    
    # Get the cursor for the next page
    params$cursor <- data$meta$next_cursor
    
    # Be a good API citizen and wait a moment
    Sys.sleep(0.1)
  }
  
  # --- 4. Return Final List ---
  cat(paste("\nFinished! Found a total of", length(all_work_ids), "citing work IDs.\n"))
  return(all_work_ids)
}

# --- Example of How to Use the Function ---
work_id_to_check <- "W4391067309"

# Call the function and store the results
citing_ids <- get_citing_works(openalex_id = work_id_to_check, mailto = "yhan@arizona.edu" )

# View the first 20 results
head(citing_ids, 20)




# --- Main Workflow ---

# 1. Get a list of ALL works from the University of Arizona for a specific year.
cat("--- Step 1: Fetching publications from U of Arizona for 2024 ---\n")
ua_works_yyyy <- get_institution_works(
  institution_ror = ua_ror_id, 
  from_date = "2024-01-01", 
  to_date = "2024-12-31"
)

ua_works_2024 <-ua_works_yyyy

# 2. Select the most cited work from our new list as an example
get_citing_works("W4391067309")


if (!is.null(ua_works_2024) && nrow(ua_works_2024) > 0) {
  cat(paste("\nFound", nrow(ua_works_2024), "publications from 2024.\n"))
  
  # Find the work with the highest citation count in our list
  example_work <- ua_works_2024 %>%
    arrange(desc(cited_by_count)) %>%
    slice(1) # Select the top one
  
  example_work_id <- example_work$id
  example_work_title <- example_work$display_name
  example_work_cited_by_api <- example_work$cited_by_api_url
  example_work_ids <- example_work$ids
  
  
  cat(paste("Title:", example_work_title, "\n"))
  cat(paste("OpenAlex ID:", example_work_id, "\n"))
  cat(paste("OpenAlex cited_by_api:", example_work_cited_by_api, "\n"))
  cat(paste("OpenAlex ID:", example_work_ids, "\n"))
  
  
  # 3. Get the authors and institutions who cited our example work
  cat("\n--- Step 3: Finding authors and institutions that cited the example publication ---\n")
  citing_authors_df <- get_citing_authors(example_work_id)
  
  # 4. Display the results
  if (!is.null(citing_authors_df)) {
    cat("\n--- Results: List of Citing Authors and their Institutions ---\n")
    # Using distinct to show unique author/institution pairs for a given paper
    citing_authors_df %>% 
      distinct(citing_work_title, author_name, institution_name, .keep_all = TRUE) %>%
      print(n = 20)
  }
  
} else {
  message("Could not retrieve any works from the University of Arizona to analyze.")
}

