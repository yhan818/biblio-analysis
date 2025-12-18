install.packages("readxl")

library(openalexR)
library(tidyverse)
library(readxl)


source("my_functions.R")


search_author <- function(author_name, affiliation_ror) {
  # Step 1: Set up logging
  base_dir <- file.path("./", "output")
  if (!dir.exists(base_dir)) { dir.create(base_dir, recursive = TRUE) }
  safe_name <- gsub("[^[:alnum:]]", "_", author_name)
  log_file <- file.path(base_dir, paste0(safe_name, ".log"))
  
  # Step 2: Query OpenAlex using 'search' for better name matching
  author_results <- tryCatch({
    oa_fetch(
      entity = "authors",
      search = author_name 
    )
  }, error = function(e) {
    cat(paste0(Sys.time(), " Error: ", e$message, "\n"), file = log_file, append = TRUE)
    return(NULL)
  })
  
  # Step 3: Handle Column Name Variance
  if (!is.null(author_results) && nrow(author_results) > 0) {
    
    # Identify which institution column is present
    col_names <- names(author_results)
    inst_col <- intersect(col_names, c("last_known_institution", "last_known_institutions"))
    
    if (length(inst_col) == 0) {
      cat(paste0(author_name, ": Institution column not found in results.\n"), file = log_file, append = TRUE)
      return(NULL)
    }
    
    # Step 4: Filter by ROR
    matches <- sapply(seq_len(nrow(author_results)), function(i) {
      # Use the identified column name (singular or plural)
      inst_info <- author_results[[inst_col]][[i]]
      
      if (is.null(inst_info)) return(FALSE)
      
      # If inst_info is a dataframe/tibble, check the 'ror' column
      if (is.data.frame(inst_info) && "ror" %in% names(inst_info)) {
        return(any(grepl(affiliation_ror, inst_info$ror, ignore.case = TRUE)))
      }
      
      # If inst_info is a list, check the 'ror' element
      if (is.list(inst_info) && "ror" %in% names(inst_info)) {
        return(grepl(affiliation_ror, inst_info$ror, ignore.case = TRUE))
      }
      
      return(FALSE)
    })
    
    filtered_authors <- author_results[matches, ]
    
    if (nrow(filtered_authors) == 0) {
      cat(paste0(author_name, ": Found in OA, but ROR mismatch.\n"), file = log_file, append = TRUE)
      return(NULL)
    } else {
      return(filtered_authors)
    }
    
  } else {
    cat(paste0(author_name, ": No results for this name.\n"), file = log_file, append = TRUE)
    return(NULL)
  }
}




# Example usage:
u_arizona_ror <- "https://ror.org/03m2x1q45"
res <- search_author("Marek Rychlik", u_arizona_ror)

if (!is.null(res)) res |> show_authors() |> knitr::kable()


### For name like "Yan Han". easily get wrong matches. 
author_results <- tryCatch({
  oa_fetch(
    entity = "authors",
    orcid = "0000-0001-9518-2684"
  )
}, error = function(e) {
  err_msg <- paste0(Sys.time(), " search_author(), Error: ", author_name, ": ", e$message, "\n")
  cat(err_msg, file = log_file, append = TRUE)
  return(NULL)
})




# --- Step 1: Open the file and read it ---
# Ensure the file is in your current working directory
file_path <- "ua_asu_unm_grant_author.xlsx"
authors_df <- read_excel(file_path)

# --- Step 2: Combine Last Name and First Name ---
# We use backticks because the column names in your file have trailing spaces
# We force the columns to be named 'last' and 'first' based on their position
colnames(authors_df)[1] <- "last"
colnames(authors_df)[2] <- "first"

# Combine them into a full name
authors_df <- authors_df %>%
  mutate(full_name = paste(trimws(first), trimws(last)))

# Define the 'Target Author' as the first person in the list
target_author <- authors_df$full_name[1]
# The 'rest' of the authors to check against
other_authors <- authors_df$full_name[-1]

cat("Target Author for matching:", target_author, "\n")

# --- Step 3: Run a for loop to query openAlexR ---
# To be efficient and avoid redundant API calls, we first fetch all works 
# by the target author once, then check if 'other_authors' appear in them.

message("Fetching all works for target author... This may take a moment.")
target_works <- oa_fetch(
  entity = "works",
  author.search = target_author,
  verbose = FALSE
)

# Initialize a list to store found collaborations
results_list <- list()

for (auth in other_authors) {
  message(paste("Checking co-publications with:", auth))
  
  # Filter target_works to find rows where 'auth' is listed in the 'author' list-column
  collabs <- target_works %>%
    filter(map_lgl(author, ~ any(grepl(auth, .x$au_display_name, ignore.case = TRUE))))
  
  if (nrow(collabs) > 0) {
    collabs$matched_collaborator <- auth
    results_list[[auth]] <- collabs
    message(paste("  --> Found", nrow(collabs), "shared papers!"))
  }
}

# Combine all results into a single table
final_table <- bind_rows(results_list)

# --- Final Step: Display and Save ---
if (nrow(final_table) > 0) {
  # Select key columns for review
  summary_view <- final_table %>% 
    select(matched_collaborator, display_name, publication_year, doi)
  
  print(summary_view)
  
  # Save the results to a CSV
  write_csv(final_table, "target_author_collaborations.csv")
  message("Results saved to 'target_author_collaborations.csv'")
} else {
  print("No co-publications found between the target author and the rest of the list.")
}