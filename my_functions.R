#### my_functions.R
### Common functions

library(openxlsx)
library(writexl)
library(data.table)
library(dplyr)
library(rlang)

##########################################
############# Search Functions #######################
### Search if a publisher is in a DF
# Output the publisher 
# @ return: the indices of the publisher
search_publisher <- function(publisher_string, df) {
  # Find indices where the host_organization contains the publisher string (case insensitive)
  indices_with_string <- which(grepl(publisher_string, df$host_organization, ignore.case = TRUE))
  
  print(df[indices_with_string, ]$host_organization)
  print(df[indices_with_string, ]$id)
  return(indices_with_string)
}

# Example usage:
# publisher_string <- "Brill"
# result_indices <- search_publisher(publisher_string, works_cited_type_articles)

#### Function: search_work_publisher(): 
## Search a work's publisher and output the publisher
# @return: index of the DF
search_work_publisher <- function(search_string, df) {
  # Find indices where the host_organization contains the search string (case insensitive)
  indices_with_string <- which(sapply(df$id, function(x) !is.na(x) && search_string %in% x))
  
  print(df[indices_with_string, ]$host_organization)
  print(indices_with_string)
  return(indices_with_string)
}

# Example usage:
# search_string <- "https://openalex.org/W2944198613"
# result_indices <- search_work_publisher(search_string, works_published)

###############################################################
# Verify any cited work using the function search_references()
# Define the function to search for a string in the referenced_works column and print the output
##############################################3
search_references <- function(search_string, df) {
  indices_with_string <- which(sapply(df$referenced_works, function(x) search_string %in% x))
  print(indices_with_string)
  print(df[indices_with_string, ]$id)
}

# Example usage:
search_string <- "Emerald Publishing"
search_string <- "Brill"
# result_indices <- search_publisher(search_string, works_published)


#' Find IDs of Works Citing a Specific Work
#'
#' Searches the 'referenced_works' column of a dataframe to find which entries
#' cite a specific work (represented by a string 'work_b'). It returns the IDs
#' ('df$id') of those citing entries.
#'
#' @param work_b A string representing the exact cited work to search for
#'               within the reference lists in 'df$referenced_works'.
#' @param df The dataframe containing work information. Must include columns
#'           'id' and 'referenced_works'. 'referenced_works' is assumed
#'           to contain lists or vectors of citation strings.
#'
#' @return A vector containing the 'id' values from rows in 'df' where
#'         'work_b' was found in that row's 'referenced_works' list.
#'         Returns an empty vector of the appropriate type (e.g., character(0))
#'         if no citing works are found or if columns are missing.
#'
find_org_works <- function(work_cited, works_published) {
  # Assign input dataframe to local variable df
  df <- works_published
  
  # --- Input Checks ---
  # Check if df is actually a data frame
  if (!is.data.frame(df)) { # Added closing parenthesis here
    stop("Error: 'works_published' must be a data frame.") # Changed message slightly for clarity
  }
  
  # Check for required 'referenced_works' column
  if (!"referenced_works" %in% names(df)) {
    warning("Warning: Dataframe 'works_published' missing 'referenced_works' column. Returning empty vector.")
    # Determine appropriate empty vector type based on 'id' column if it exists
    if ("id" %in% names(df)) return(df$id[0]) else return(character(0))
  }
  
  # Check for required 'id' column
  if (!"id" %in% names(df)) {
    warning("Warning: Dataframe 'works_published' missing 'id' column. Returning empty vector.")
    return(character(0)) # Cannot return IDs if 'id' column is missing
  }
  
  # Check if work_cited is a single string
  if (!is.character(work_cited) || length(work_cited) != 1) {
    # Corrected variable name in the error message below
    stop("Error: 'work_cited' must be a single string.")
  }
  
  # --- Find Indices ---
  # Find indices where work_cited exists exactly within the referenced_works list/vector.
  # This handles cases where an element in referenced_works might be NULL.
  indices_with_string <- which(sapply(df$referenced_works, function(reference_list) {
    # Check if reference_list is not NULL before checking for work_cited within it
    !is.null(reference_list) && work_cited %in% reference_list
  }))
  
  # --- Retrieve and Return IDs ---
  # If no indices found, this correctly returns an empty vector df$id[integer(0)]
  citing_ids <- df$id[indices_with_string]
  
  return(citing_ids)
}
# --- Example Usage ---
work_cited <- "https://openalex.org/W2176010001"
# indices_with_string <- which(sapply(works_published$referenced_works, function(x) search_string %in% x))
#find_org_works(work_cited, works_published_2024)

#### 2022 Nature-SN
work_cited <- "https://openalex.org/W3217221255"
work_cited <- "https://openalex.org/W576036492"
work_cited<-"https://openalex.org/W4281394370"
#find_org_works(work_cited, works_published_2022)

work_cited<-"https://openalex.org/W1494075612"


#find_org_works(work_cited, works_published_2022)




##### Handling works "topic": OpenAlex's new topic has a hierarchical structure:
### domain-field-subfield-topic system (https://docs.google.com/document/d/1bDopkhuGieQ4F8gGNj7sEc8WSE8mvLZS/edit)
### Example: https://api.openalex.org/works/W2944198613 (search for primary_topic: )
## A work may have multiple domain-field-subfield-topic. Primary topic has a number "1" in "i", the 2nd has "2", and so on.
# The function adds 4 new cols: topic, subfield, field, and domain. 
# Default: level = 1, it is primary 
#         level = 2, it is secondry. Most works have 1 to 3 topic-subfield-field-domains, and do not have 4th topic. 


extract_topics_by_level <- function(data, level = 1) {
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  # Check for 'topics' column
  if (!("topics" %in% names(data))) {
    stop("The 'topics' column is missing from the input data frame.")
  }
  # Validate 'level'
  if (!is.numeric(level) || length(level) != 1 || level < 1 || level != as.integer(level)) {
    stop("'level' must be a positive integer.")
  }
  
  # --- Data Extraction and Transformation ---
  extracted_data <- data %>%
    select(id, title, topics) %>%
    # Rename the nested 'id' column *before* unnesting (as in original)
    mutate(topics = map(topics, ~{
      if (is.data.frame(.x) && "id" %in% names(.x)) {
        rename(.x, topic_id = id)
      } else {
        .x
      }
    })) %>%
    unnest(cols = c(topics)) %>%
    # Filter for the specific level
    filter(i == level) %>%
    # Select relevant info - assuming 'i' is the level index and 'display_name' is the value
    # We no longer select or rename a 'name' column. This column is gone in 2025-05
    select(id, title, level_index = i, topic_display_name = display_name) # Renaming for clarity
  
  # Handle the case where no rows match the level
  if (nrow(extracted_data) == 0) {
    warning(paste("No data found for level", level,
                  ". Returning original data frame with NA for new columns."))
    # Create the column that would have been added by the join with NA values
    # The column name will be dynamic based on the level
    col_name = paste0("level_", level, "_topic_display_name")
    data[[col_name]] <- NA_character_ # Add the column with NA values
    return(data)
  }
  
  # Pivot wider to create a column for this level's display name
  # Use level_index to create column names, adding a prefix for clarity
  extracted_data <- extracted_data %>%
    pivot_wider(id_cols = c(id, title),
                names_from = level_index, # Use the level index to name columns
                values_from = topic_display_name, # Use the display name as the value
                values_fn = ~paste(unique(.x), collapse = " - "),
                names_prefix = "level_") # Add a prefix like "level_1", "level_2", etc.
  
  # --- Left Join ---
  # Join back to original data using id and title
  final_data <- data %>%
    left_join(extracted_data, by = c("id", "title"))
  
  return(final_data)
}

extract_topics_by_level_pre_2025_04 <- function(data, level = 1) {
  
  # --- Input Validation ---
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (!("topics" %in% names(data))) {
    stop("The 'topics' column is missing from the input data frame.")
  }
  if (!is.numeric(level) || length(level) != 1 || level < 1 || level != as.integer(level)) {
    stop("'level' must be a positive integer.")
  }
  
  # --- Data Extraction ---
  extracted_data <- data %>%
    select(id, title, topics) %>%
    # Rename the nested 'id' column *before* unnesting
    mutate(topics = map(topics, ~{
      if (is.data.frame(.x) && "id" %in% names(.x)) {
        rename(.x, topic_id = id)
      } else {
        .x
      }
    })) %>%
    unnest(cols = c(topics)) %>%
    filter(i == level) %>%
    # Select the relevant info
    select(id, title, level_name = name, display_name)
  
  # Handle the case where no rows match the level
  if (nrow(extracted_data) == 0) {
    warning(paste("No data found for level", level,
                  ". Returning an empty data frame with appropriate columns."))
    # Create an empty data frame with the correct structure
    empty_df <- data %>%
      select(id, title) %>%  # Keep id and title
      mutate(topic = NA_character_,
             subfield = NA_character_,
             field = NA_character_,
             domain = NA_character_)
    return(empty_df)
  }
  # Pivot wider to create separate columns for each level_name
  extracted_data <- extracted_data %>%
    pivot_wider(id_cols = c(id, title), names_from = level_name, values_from = display_name, values_fn = ~paste(unique(.x), collapse = ", "))
  
  # --- Left Join ---
  final_data <- data %>%
    left_join(extracted_data, by = c("id", "title"))  # Join back to original data
  
  return(final_data)
}

# use: 
# works_cited_type_articles_publisher <- works_cited_type_articles_brill

# primary_topics <- extract_topics_by_level(works_cited_type_articles_publisher, 1)
# second_topics  <- extract_topics_by_level(primary_topics, 2)
# third_topics   <- extract_topics_by_level(works_cited_type_articles_publisher, 3)
# fourth_topics  <- extract_topics_by_level(works_cited_type_articles_publisher, 4)
# fifth_topics   <- extract_topics_by_level(works_cited_type_articles_publisher, 5)





################### Analyze top journals for each publisher ############
# Function to rank top cited journals
# Usage example:
#rank_top_cited_journals(publisher_nature, "so", 10)  # Top 10 cited journals

rank_top_cited_journals2 <- function(data, journal_col, top_n = 30, output_dir = "citations") {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame or tibble.")
  }
  if (!is.character(journal_col) || length(journal_col) != 1) {
    stop("Input 'journal_col' must be a single string.")
  }
  if (!(journal_col %in% names(data))) {
    stop("journal_col is not found")
  }
  
  top_cited_journals <- data %>%
    group_by(!!sym(journal_col)) %>%
    summarise(citation_count = n(), .groups = "drop") %>%
    arrange(desc(citation_count)) %>%
    rename("Journal Title" = !!sym(journal_col))
  
  # Print all rows if top_n is NULL or larger than number of rows
  if (is.null(top_n) || top_n >= nrow(top_cited_journals)) {
    print(as.data.frame(top_cited_journals))  # Convert to data.frame and print all
  } else {
    # Print only the requested top_n journals.
    print(head(as.data.frame(top_cited_journals), top_n))
    top_cited_journals <- top_cited_journals %>%
      slice(1:top_n) #keep top_n for writing to file
  }
  
  # --- File Output ---
  # Get the name of the input data frame
  df_name <- deparse(substitute(data))
  
  # Create the output file path
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  output_file <- file.path(output_dir, paste0(df_name, "_top_cited_journals.xlsx"))
  
  # Write to Excel
  tryCatch({
    write_xlsx(list("Top Cited Journals" = as.data.frame(top_cited_journals)), output_file)
    message(paste("Successfully wrote top cited journals to:", output_file))
  }, error = function(e) {
    message(paste("Error writing to Excel:", e))
    print(e)  # Print the full error object
  })
  
  return(top_cited_journals)
}


rank_top_cited_journals <- function(data,
                                    journal_col, # e.g., "so"
                                    # These parameters specify the names of the columns in the *original* 'data'
                                    # from which to pull ISSN and Host Organization information.
                                    source_issn_col = "issn_l",
                                    source_host_org_col = "host_organization",
                                    top_n = 30,
                                    output_dir = "citations") {
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame or tibble.")
  }
  if (!is.character(journal_col) || length(journal_col) != 1) {
    stop("Input 'journal_col' must be a single string.")
  }
  if (!(journal_col %in% names(data))) {
    stop(paste0("'", journal_col, "' (journal_col) not found in input 'data'."))
  }
  if (!(source_issn_col %in% names(data))) {
    stop(paste0("Source ISSN column '", source_issn_col, "' not found in input 'data'."))
  }
  if (!(source_host_org_col %in% names(data))) {
    stop(paste0("Source Host Organization column '", source_host_org_col, "' not found in input 'data'."))
  }
  
  # --- Original summarization logic to get top cited journals ---
  # This part remains as it was.
  top_cited_journals <- data %>%
    group_by(!!sym(journal_col)) %>%
    summarise(citation_count = n(), .groups = "drop") %>%
    arrange(desc(citation_count)) %>%
    rename("Journal Title" = !!sym(journal_col))
  
  # --- Add new columns "ISSN" and "host_organization" by looking up in original 'data' ---
  # Initialize the new columns in top_cited_journals_df
  top_cited_journals$ISSN <- NA_character_
  top_cited_journals$`host_organization` <- NA_character_ # Using backticks for the column name
  
  for (i in 1:nrow(top_cited_journals)) {
    current_journal_title <- top_cited_journals$"Journal Title"[i]
    
    # Find the first matching row in the original 'data'
    # data[[journal_col]] refers to the column in 'data' whose name is stored in the 'journal_col' variable (e.g., data$so)
    match_indices <- which(data[[journal_col]] == current_journal_title)
    
    if (length(match_indices) > 0) {
      first_match_index <- match_indices[1] # Take the first match
      
      # Assign values from the original 'data' using the source column names
      issn_value <- data[[source_issn_col]][first_match_index]
      host_org_value <- data[[source_host_org_col]][first_match_index]
      
      top_cited_journals$ISSN[i] <- if (is.null(issn_value) || length(issn_value) == 0) NA_character_ else as.character(issn_value)
      top_cited_journals$`host_organization`[i] <- if (is.null(host_org_value) || length(host_org_value) == 0) NA_character_ else as.character(host_org_value)
    }
  }
  
  # Reorder columns for better presentation if desired
  top_cited_journals <- top_cited_journals %>%
    select("Journal Title", "ISSN", "host_organization", "citation_count", everything())
  
  # --- Printing and Slicing Logic (operates on the now enriched top_cited_journals) ---
  df_to_print_and_write <- top_cited_journals # Start with the full enriched data
  
  if (is.null(top_n) || top_n >= nrow(df_to_print_and_write)) {
    print(as.data.frame(df_to_print_and_write))
  } else {
    print(head(as.data.frame(df_to_print_and_write), top_n))
    # Slice df_to_print_and_write if top_n is applicable for the file
    df_to_print_and_write <- df_to_print_and_write %>%
      slice(1:top_n)
  }
  
  # --- File Output ---
  # Get the name of the input data frame
  df_name <- deparse(substitute(data))
  
  # Create the output file path
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  # Modified filename to indicate enriched content
  output_file <- file.path(output_dir, paste0(df_name, "_top_cited_j.xlsx"))
  
  # Write to Excel using the (potentially sliced) enriched data frame
  tryCatch({
    write_xlsx(list("Top Cited Journals" = as.data.frame(df_to_print_and_write)), output_file)
    message(paste("Successfully wrote top cited journals to:", output_file))
  }, error = function(e) {
    message(paste("Error writing to Excel:", e$message))
  })
  
  # The function will return the enriched data frame, sliced if top_n was applied.
  # If you want to always return the full enriched data regardless of top_n for the file,
  # you would return 'top_cited_journals_df' instead of 'df_to_print_and_write'.
  # Based on the original code's structure, it returns the (potentially) sliced version.
  return(df_to_print_and_write)
}

# --- Example Usage ---
# Create a dummy dataset for demonstration
# dummy_data_journals <- tibble(
#   so = sample(c("Nature", "Science", "Cell", "Lancet", "JAMA", "Nature Comms"), 1000, replace = TRUE),
#   some_other_data = rnorm(1000),
#   issn_l = case_when( # This column will be used as 'source_issn_col'
#     so == "Nature" ~ "1476-4687",
#     so == "Science" ~ "1095-9203",
#     so == "Cell" ~ "0092-8674",
#     so == "Lancet" ~ "0140-6736",
#     so == "JAMA" ~ "1538-3598",
#     so == "Nature Comms" ~ "2041-1723",
#     TRUE ~ NA_character_
#   ),
#   host_organization = case_when( # This column will be used as 'source_host_org_col'
#     so == "Nature" ~ "Nature Portfolio",
#     so == "Science" ~ "AAAS",
#     so == "Cell" ~ "Cell Press",
#     so == "Lancet" ~ "Elsevier",
#     so == "JAMA" ~ "AMA",
#     so == "Nature Comms" ~ "Nature Portfolio", # Same host org for different journal
#     TRUE ~ NA_character_
#   ),
#   # Add some duplicate journal entries with potentially different (or NA) ISSN/Host Org
#   # to test the "first match" logic of the loop
#   issn_l = ifelse(runif(1000) < 0.1 & so == "Nature", "0000-000X (alt Nature ISSN)", issn_l),
#   host_organization = ifelse(runif(1000) < 0.05 & so == "Science", NA_character_, host_organization)
# )
#
# # Add a journal entry that will be in top N but has NA for ISSN initially in one entry
# dummy_data_journals <- bind_rows(dummy_data_journals,
#                                  tibble(so="Cell", some_other_data=1, issn_l=NA_character_, host_organization="Cell Press Test NA"))
# for(k in 1:20) dummy_data_journals <- bind_rows(dummy_data_journals,
#                                  tibble(so="Cell", some_other_data=k, issn_l="0092-8674", host_organization="Cell Press"))
#
#
# # Call the function using the default source column names "issn_l" and "host_organization"
# # results_loop <- rank_top_cited_journals(dummy_data_journals, "so", top_n = 5)
# # print("Returned data from function (loop version):")
# # print(results_loop)
#
# # Example if your source columns in 'dummy_data_journals' had different names:
# # dummy_data_alt_names <- dummy_data_journals %>%
# #   rename(Journal_Identifier_ISSN = issn_l, Main_Publisher = host_organization)
# #
# # results_loop_alt_names <- rank_top_cited_journals(
# #   dummy_data_alt_names,
# #   journal_col = "so",
# #   source_issn_col = "Journal_Identifier_ISSN", # Specify the actual name in your data
# #   source_host_org_col = "Main_Publisher",    # Specify the actual name in your data
# #   top_n = 3
# # )
# # print("Returned data (loop version, alt names):")
# # print(results_loop_alt_names)

 
compare_top_journals <- function(df_ua, df_ou, n) {
  #' Compares the top N journal titles between two data frames.
  #'
  #' Assumes both data frames have a 'Journal Title' column and are
  #' already sorted such that the top N rows represent the top N journals.
  #'
  #' @param df_ua Data frame containing UA's journal data, sorted by rank.
  #' @param df_ou Data frame containing the other u's journal data, sorted by rank.
  #' @param n The number of top journals to compare.
  #'
  #' @return A list containing three vectors of journal titles:
  #'   'common': Journals in the top N of both data frames.
  #'   'ua_unique': Journals in the top N of df_ua but not in the top N of df_ou.
  #'   'ou_unique': Journals in the top N of df_ou but not in the top N of df_ua.
  #' @export
  #'
  if (!"Journal Title" %in% colnames(df_ua)) {
    stop("Error: 'Journal Title' column not found in UA data frame.")
  }
  if (!"Journal Title" %in% colnames(df_ou)) {
    stop("Error: 'Journal Title' column not found in ou data frame.")
  }
  
  # Get the top N journal titles from each data frame
  # Ensure n does not exceed the number of rows in the dataframe
  n_ua <- min(n, nrow(df_ua))
  n_ou <- min(n, nrow(df_ou))
  
  ua_top_n_titles <- head(df_ua$`Journal Title`, n_ua)
  ou_top_n_titles <- head(df_ou$`Journal Title`, n_ou)
  
  # Convert to character vectors to ensure set operations work correctly
  ua_top_n_titles <- as.character(ua_top_n_titles)
  ou_top_n_titles <- as.character(ou_top_n_titles)
  
  # Find common and unique journals
  common_journals <- intersect(ua_top_n_titles, ou_top_n_titles)
  ua_unique_journals <- setdiff(ua_top_n_titles, ou_top_n_titles)
  ou_unique_journals <- setdiff(ou_top_n_titles, ua_top_n_titles)
  
  # Calculate counts
  count_common <- length(common_journals)
  count_ua_unique <- length(ua_unique_journals)
  count_ou_unique <- length(ou_unique_journals)
  
  return(list(
    common = common_journals,
    ua_unique = ua_unique_journals,
    ou_unique = ou_unique_journals,
    count_common = count_common,
    count_ua_unique = count_ua_unique,
    count_ou_unique = count_ou_unique
  ))
}




# Reformating Column "author" raw data >>> author regular name
###### $author data structure. 
# 1. works_cited: a data frame (or data.table), and its each row is a cited work.   
# 2. the "author" column: it is a "list". Each element of this list is the "author" info. 
#     each data frame has the au_display_name column. 
#     some elements are data frames and some are other data types. 
# 
# Apply the function to the author column

extract_author_names <- function(author_data) {
  message("DEBUG: Input author_data class: ", class(author_data))
  
  if (is.null(author_data)) {
    message("DEBUG: author_data is NULL.")
    return(NA_character_)
  }
  
  if (is.data.frame(author_data)) {
    message("DEBUG: author_data is a data frame.")
    author_df <- author_data
  } else if (is.list(author_data) && length(author_data) > 0 && is.data.frame(author_data[[1]])) {
    message("DEBUG: author_data is a list of data frame.")
    author_df <- author_data[[1]]
  } else {
    message("DEBUG: author_data is not a dataframe or list of dataframe.")
    return(NA_character_)
  }
  
  if (!is.data.frame(author_df)) {
    message("DEBUG: author_df is not a data frame.")
    return(NA_character_)
  }
  
  if (!("au_display_name" %in% names(author_df))) {
    message("DEBUG: author_df is missing au_display_name.")
    return(NA_character_)
  }
  
  message("DEBUG: au_display_name column exists.")
  message("DEBUG: Number of rows in author_df: ", nrow(author_df))
  
  if (nrow(author_df) > 0) {
    message("DEBUG: au_display_name values: ", paste(author_df$au_display_name, collapse = ", "))
    valid_names <- author_df$au_display_name[!is.na(author_df$au_display_name)]
    message("DEBUG: valid_names: ", paste(valid_names, collapse = ", "))
    if(length(valid_names) > 0){
      author_names <- paste(valid_names, collapse = "; ")
    } else {
      author_names <- NA_character_
    }
  } else {
    message("DEBUG: author_df is empty.")
    author_names <- NA_character_
  }
  
  message("DEBUG: Final author_names: ", author_names)
  return(author_names)
}



write_df_to_excel <- function(df, file_path_prefix = "citations/", max_chars = 32767) { # Updated default max_chars
  df_name <- deparse(substitute(df))
  file_name <- paste0(df_name, ".xlsx")
  file_path <- paste0(file_path_prefix, file_name)
  
  # Ensure the directory exists
  if (!dir.exists(file_path_prefix)) {
    dir.create(file_path_prefix, recursive = TRUE)
    message(paste("Created directory:", file_path_prefix))
  }
  
  # Define the truncation marker - keep it reasonably short
  trunc_marker <- " [...trunc]" # Example marker (11 chars)
  
  # Function to process a single value with depth tracking
  process_value <- function(x, max_chars, depth = 0, col_name = "") {
    indent <- paste(rep("  ", depth), collapse = "")
    
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    
    tryCatch({
      if (all(is.na(x))) {
        return(NA_character_)
      } else if (col_name == "author" && is.list(x) && length(x) > 0 && is.data.frame(x[[1]])) { # Added checks for structure
        message("\nDEBUG: Processing author data")
        author_df <- x[[1]]
        if (nrow(author_df) == 0) return(NA_character_) # Handle empty data frame
        
        row_strings <- character(nrow(author_df))
        for(i in 1:nrow(author_df)) {
          author_info <- c(
            author_df$au_id[i], author_df$au_display_name[i], author_df$au_orcid[i],
            author_df$author_position[i], author_df$is_corresponding[i], author_df$au_affiliation_raw[i],
            author_df$institution_id[i], author_df$institution_display_name[i], author_df$institution_ror[i],
            author_df$institution_country_code[i], author_df$institution_type[i], author_df$institution_lineage[i]
          )
          # Convert potential NULLs or NAs within author_info to "" or "NA" before pasting
          author_info <- sapply(author_info, function(val) ifelse(is.null(val) || is.na(val), "NA", as.character(val)))
          row_strings[i] <- paste(author_info, collapse = ": ")
        }
        full_string <- paste(row_strings, collapse = "; ")
        
        # --- ADDED TRUNCATION CHECK ---
        if (nchar(full_string) > max_chars) {
          allowed_len <- max_chars - nchar(trunc_marker)
          if (allowed_len < 0) allowed_len <- 0 # Safety check
          full_string <- paste0(substr(full_string, 1, allowed_len), trunc_marker)
        }
        message("DEBUG: Authors final string length: ", nchar(full_string))
        return(full_string)
        
      } else if (col_name == "topics" && is.list(x) && length(x) > 0 && is.data.frame(x[[1]])) { # Added checks for structure
        message("\nDEBUG: Processing topics data")
        topics_df <- x[[1]]
        if (nrow(topics_df) == 0) return(NA_character_) # Handle empty data frame
        
        row_strings <- character(nrow(topics_df))
        for(i in 1:nrow(topics_df)) {
          topic_values <- c(
            topics_df$i[i], topics_df$score[i], topics_df$name[i],
            topics_df$id[i], topics_df$display_name[i]
          )
          # Convert potential NULLs or NAs before pasting
          topic_values <- sapply(topic_values, function(val) ifelse(is.null(val) || is.na(val), "NA", as.character(val)))
          row_strings[i] <- paste(topic_values, collapse = ": ")
        }
        full_string <- paste(row_strings, collapse = "; ")
        
        # --- ADDED TRUNCATION CHECK ---
        if (nchar(full_string) > max_chars) {
          allowed_len <- max_chars - nchar(trunc_marker)
          if (allowed_len < 0) allowed_len <- 0 # Safety check
          full_string <- paste0(substr(full_string, 1, allowed_len), trunc_marker)
        }
        message("DEBUG: Topics final string length: ", nchar(full_string))
        return(full_string)
        
      } else if (is.data.frame(x)) {
        # message("DEBUG: Processing generic data.frame")
        if (nrow(x) == 0) return(NA_character_) # Handle empty data frame
        
        row_strings <- character(nrow(x))
        for(i in 1:nrow(x)) {
          # Convert row to character, handling potential NULL/NA
          row_values <- sapply(x[i,], function(val) ifelse(is.null(val) || is.na(val), "NA", as.character(val)))
          row_strings[i] <- paste(row_values, collapse = ": ")
        }
        full_string <- paste(row_strings, collapse = "; ")
        
        # --- ADDED TRUNCATION CHECK ---
        if (nchar(full_string) > max_chars) {
          allowed_len <- max_chars - nchar(trunc_marker)
          if (allowed_len < 0) allowed_len <- 0 # Safety check
          full_string <- paste0(substr(full_string, 1, allowed_len), trunc_marker)
        }
        # message("DEBUG: Generic DF final string length: ", nchar(full_string))
        return(full_string)
        
      } else if (is.list(x) && !is.data.frame(x)) {
        # message("DEBUG: Processing generic list")
        # Original list processing logic (already includes truncation)
        unlisted <- unlist(x)
        if (is.null(unlisted) || length(unlisted) == 0) {
          return(NA_character_)
        }
        unlisted <- unlisted[!is.null(unlisted) & !is.na(unlisted)]
        if (length(unlisted) == 0) {
          return(NA_character_)
        }
        full_string <- paste(unlisted, collapse = ": ")
        
        # Using the consistent truncation logic
        if (nchar(full_string) > max_chars) {
          allowed_len <- max_chars - nchar(trunc_marker)
          if (allowed_len < 0) allowed_len <- 0 # Safety check
          return(paste0(substr(full_string, 1, allowed_len), trunc_marker))
        }
        return(full_string) # Return unmodified if within limit
        
      } else {
        # message("DEBUG: Processing scalar value")
        # Original scalar processing logic (already includes truncation)
        char_val <- as.character(x) # Convert single value to character
        if (length(char_val) > 1) { # Should not happen often here but safer
          char_val <- paste(char_val, collapse = ": ")
        }
        
        # Using the consistent truncation logic
        if (nchar(char_val) > max_chars) {
          allowed_len <- max_chars - nchar(trunc_marker)
          if (allowed_len < 0) allowed_len <- 0 # Safety check
          return(paste0(substr(char_val, 1, allowed_len), trunc_marker))
        }
        return(char_val) # Return unmodified if within limit
      }
    }, error = function(e) {
      warning(paste("Error processing value in column '", col_name, "':", e$message))
      return(NA_character_) # Return NA on error
    })
  }
  
  # Convert data.table to data.frame if necessary
  if (inherits(df, "data.table")) {
    df <- as.data.frame(df)
  }
  
  # Create output dataframe (initialize with NA_character_ for safety)
  df_processed <- data.frame(matrix(NA_character_, nrow = nrow(df), ncol = ncol(df)))
  colnames(df_processed) <- colnames(df)
  
  # Process each cell using the updated process_value function
  # Using nested loops for clarity, apply could also be used but might be complex with column names
  for (j in seq_along(colnames(df))) {
    col_name <- colnames(df)[j]
    #message(paste("Processing column:", col_name))
    for (i in seq_len(nrow(df))) {
      # Access the element correctly, df[[j]][i] or df[[col_name]][i]
      cell_value <- df[[j]][[i]]
      df_processed[i, j] <- process_value(cell_value, max_chars, depth = 1, col_name = col_name)
    }
  }
  
  # Write the processed dataframe to Excel
  tryCatch({
    write_xlsx(df_processed, file_path)
    message(paste("Successfully wrote", df_name, "to", file_path))
  }, error = function(e) {
    # Provide more context on error
    message(paste("Error writing", df_name, "to Excel file:", file_path))
    message("Original error message:", e$message)
    # Consider printing offending row/column if possible, though identifying it post-processing is hard
    # You might add more detailed logging within the process_value function if needed
    print(e) # Print the full error object
  })
}

# --- Example Usage ---
# Create a dummy data frame with nested structures and long strings
# concept_long <- paste(rep("https://concept.url/", 2000), collapse=":") # Approx 40k chars
# topic_long <- paste(rep("T12345:0.99:Topic Name;", 1000), collapse="") # Approx 25k chars
# author_long_name <- paste(rep("Long Author Name ", 1000), collapse="") # Approx 17k chars
#
# dummy_authors <- data.frame(
#   au_id = "A123", au_display_name = author_long_name, au_orcid = "orcid",
#   author_position = "first", is_corresponding = TRUE, au_affiliation_raw = "Uni",
#   institution_id = "I456", institution_display_name = "University", institution_ror = "ror",
#   institution_country_code = "US", institution_type = "education", institution_lineage = "L1"
# )
#
# dummy_topics <- data.frame(
#    i = 1, score = 0.98, name = "Long Topic", id = "T987", display_name = topic_long
# )
#
# # Note: Store the complex data frame within a list for the cell
# df_test <- data.frame(id = 1:2)
# df_test$text_col <- c("Short string", paste(rep("LongText", 4000), collapse="")) # > 32k
# df_test$authors <- I(list(list(dummy_authors), list(dummy_authors))) # Wrap the list containing the df in I() or another list
# df_test$topics <- I(list(list(dummy_topics), list(dummy_topics)))
# df_test$concepts <- c("Short concept", concept_long)
#
# # Run the function
# write_df_to_excel(df_test)

write_df_to_excel2 <- function(df, file_path_prefix = "citations/", max_chars = 32000) {
  df_name <- deparse(substitute(df))
  file_name <- paste0(df_name, ".xlsx")
  file_path <- paste0(file_path_prefix, file_name)
  
  # Function to process a single value with depth tracking
  process_value <- function(x, max_chars, depth = 0, col_name = "") {
    indent <- paste(rep("  ", depth), collapse = "")
    
    if (is.null(x) || length(x) == 0) {
      return(NA_character_)
    }
    
    tryCatch({
      if (all(is.na(x))) {
        return(NA_character_)
      } else if (col_name == "author") {
        message("\nDEBUG: Processing author data")
        # Extract the data frame from the list
        author_df <- x[[1]]
        # message("DEBUG: Number of authors: ", nrow(author_df))
        
        # Process each author
        row_strings <- character(nrow(author_df))
        for(i in 1:nrow(author_df)) {
          # Get specific fields in desired order
          author_info <- c(
            author_df$au_id[i],
            author_df$au_display_name[i],
            author_df$au_orcid[i],
            author_df$author_position[i],
            author_df$is_corresponding[i],
            author_df$au_affiliation_raw[i],
            author_df$institution_id[i],
            author_df$institution_display_name[i],
            author_df$institution_ror[i],
            author_df$institution_country_code[i],
            author_df$institution_type[i],
            author_df$institution_lineage[i]
          )
          row_strings[i] <- paste(author_info, collapse = ": ")
          #message("DEBUG: Author ", i, " values: ", row_strings[i])
        }
        
        full_string <- paste(row_strings, collapse = "; ")
        #message("DEBUG: Authors final string: ", full_string)
        return(full_string)
        
      } else if (col_name == "topics") {
        #message("\nDEBUG: Processing topics data")
        # Extract the data frame from the list
        topics_df <- x[[1]]
        #message("DEBUG: Number of topics: ", nrow(topics_df))
        
        # Process each topic row
        row_strings <- character(nrow(topics_df))
        for(i in 1:nrow(topics_df)) {
          topic_values <- c(
            topics_df$i[i],
            topics_df$score[i],
            topics_df$name[i],
            topics_df$id[i],
            topics_df$display_name[i]
          )
          row_strings[i] <- paste(topic_values, collapse = ": ")
          #message("DEBUG: Topic ", i, " values: ", row_strings[i])
        }
        
        full_string <- paste(row_strings, collapse = "; ")
        #message("DEBUG: Topics final string: ", full_string)
        return(full_string)
        
      } else if (is.data.frame(x)) {
        row_strings <- character(nrow(x))
        for(i in 1:nrow(x)) {
          row_values <- as.character(unlist(x[i,]))
          row_strings[i] <- paste(row_values, collapse = ": ")
        }
        full_string <- paste(row_strings, collapse = "; ")
        return(full_string)
        
      } else if (is.list(x) && !is.data.frame(x)) {
        unlisted <- unlist(x)
        if (is.null(unlisted) || length(unlisted) == 0) {
          return(NA_character_)
        }
        unlisted <- unlisted[!is.null(unlisted) & !is.na(unlisted)]
        if (length(unlisted) == 0) {
          return(NA_character_)
        }
        full_string <- paste(unlisted, collapse = ": ")
        if (nchar(full_string) > max_chars) {
          return(paste0(substr(full_string, 1, max_chars), " [truncated...]"))
        }
        return(full_string)
      } else {
        char_val <- as.character(x)
        if (length(char_val) > 1) {
          char_val <- paste(char_val, collapse = ": ")
        }
        if (nchar(char_val) > max_chars) {
          return(paste0(substr(char_val, 1, max_chars), " [truncated...]"))
        }
        return(char_val)
      }
    }, error = function(e) {
      warning(paste("Error processing value:", e$message))
      return(NA_character_)
    })
  }
  
  # Convert data.table to data.frame if necessary
  if (inherits(df, "data.table")) {
    df <- as.data.frame(df)
  }
  
  # Create output dataframe
  df_processed <- data.frame(matrix(nrow = nrow(df), ncol = ncol(df)))
  colnames(df_processed) <- colnames(df)
  
  # Process each row
  for (i in seq_len(nrow(df))) {
    #message(sprintf("\nProcessing main row %d:", i))
    current_row <- df[i, , drop = FALSE]  # Keep as dataframe
    processed_row <- sapply(names(current_row), function(col) {
      result <- process_value(current_row[[col]], max_chars, depth = 1, col_name = col)
      #message("DEBUG: Final result for column ", col, ": ", result)
      return(result)
    })
    df_processed[i,] <- processed_row
  }
  
  tryCatch({
    write_xlsx(df_processed, file_path)
    message(paste("Successfully wrote", df_name, "to", file_path))
  }, error = function(e) {
    message(paste("Error writing", df_name, "to Excel:", e))
    print(e)
  })
}