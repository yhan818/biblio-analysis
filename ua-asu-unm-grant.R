install.packages("readxl")

library(openalexR)
library(tidyverse)
library(readxl)
library(knitr)

source("my_functions.R")


# Initialize lists to track results
found_list <- list()
not_found_list <- list()

# Example usage:
u_arizona_ror <- "https://ror.org/03m2x1q45"
res <- search_author("Terry Badger", u_arizona_ror)
if (!is.null(res)) res |> show_authors() |> knitr::kable()

### For name like "Yan Han". easily get wrong matches. 
# author_results <- tryCatch({
#   oa_fetch(
#     entity = "authors",
#     orcid = "0000-0001-9518-2684"
#   )
# }, error = function(e) {
#   err_msg <- paste0(Sys.time(), " search_author(), Error: ", author_name, ": ", e$message, "\n")
#   cat(err_msg, file = log_file, append = TRUE)
#   return(NULL)
# })





# --- Test Setup ---

# 1. Create a master list of all verified OpenAlex IDs in the group
all_group_ids <- sapply(found_list, function(x) x$id)
verified_names <- names(found_list)

all_collabs <- list()


# --- Test Setup ---
focal_id <- "https://openalex.org/A5049047999"
focal_name <- "Robert Hanson" 

# Helper function to ensure IDs match regardless of URL prefix
# 1. Prepare Group IDs (Clean them for comparison)
all_group_ids_clean <- clean_id(all_group_ids)
focal_id_clean <- clean_id(focal_id)
other_group_ids_clean <- all_group_ids_clean[all_group_ids_clean != focal_id_clean]

message("Targeting ", length(other_group_ids_clean), " other authors in the group.")



# --- Fetching Works ---
# --- Prerequisites ---
# Ensure 'all_group_ids' exists from your Step 1 loop.
# Ensure 'all_group_ids' exists from your Step 1 loop.

focal_id <- "https://openalex.org/A5049047999"
focal_name <- "Robert Hanson"
  five_years_ago <- "2021-01-01"

# 1) Works identified (Last 5 years)
works <- oa_fetch(
  entity = "works", 
  author.id = focal_id, 
  from_publication_date = five_years_ago, 
  verbose = FALSE
)

if (!is.null(works) && nrow(works) > 0) {
  
  # Prepare the matching list (excluding the focal author)
  other_group_ids_clean <- clean_id(all_group_ids[all_group_ids != focal_id])
  
  # 2) Get unique collaborators & 3) Match with list
  # We do this paper-by-paper to build a clean lookup table
  collab_lookup <- map_df(seq_len(nrow(works)), function(i) {
    auth_df <- works$authorships[[i]]
    
    # Extract arrays of IDs and Names from this paper
    # (Handles cases where IDs are nested or top-level)
    paper_ids <- if ("id" %in% names(auth_df)) auth_df$id else auth_df$author$id
    paper_names <- if ("display_name" %in% names(auth_df)) auth_df$display_name else auth_df$author$display_name
    
    # 3) Match array with collaborator_list
    match_idx <- which(clean_id(paper_ids) %in% other_group_ids_clean)
    
    if (length(match_idx) > 0) {
      # 4) Prepare collaborator name and id output
      tibble(
        work_id = works$id[i],
        collab_names = paste(unique(paper_names[match_idx]), collapse = "; "),
        collab_ids   = paste(unique(paper_ids[match_idx]), collapse = "; "),
        collab_count = length(unique(paper_ids[match_idx]))
      )
    } else {
      NULL
    }
  })
  
  # --- Combine and Output ---
  if (nrow(collab_lookup) > 0) {
    
    # Flatten the works metadata
    flat_works <- show_works(works)
    
    # Join the collaborator info back to the flattened works
    # This prevents the "size mismatch" error
    final_output <- flat_works %>%
      inner_join(collab_lookup, by = c("id" = "work_id")) %>%
      mutate(focal_author = focal_name)
    
    # Output count of papers
    cat("\nTotal papers with group collaborations:", nrow(final_output), "\n")
    
    # Output table with names and IDs
    final_output %>%
      select(focal_author, collab_count, collab_names, collab_ids, display_name) %>%
      knitr::kable() %>%
      print()
    
  } else {
    message("No group members found in papers from the last 5 years.")
  }
}







################################ The above shall go to myfunctions.

# [Not Found] Henry Tseng at ASU (Jui-Heng Tseng)
# [Not Found] Ken Buetow at ASU (Kennith Buetow)
# [Not Found] Bill Shuttleworth at UNM (William )


[Not Found] Hossein Ardehali at UA
[Not Found] Haijiang Cai at UA
[Not Found] Michael Daines at UA

# [Not Found] Tatiana Kalin at UA (Cincinnati https://api.openalex.org/a5078276804 )
# [Not Found] Moulun Luo at UA

# [Not Found] Mary laura Thomas at ASU (Mary Laura Lind)
# !!! [Not Found] Sampath Rangasamy at ASU (Arizona Research Center?, Phoenix, DO Check openAlex manually!!!)


[Not Found] Olga Ponomarova at UNM (https://orcid.org/0000-0001-6331-9949 ). Handle differently
[Not Found] Amy Gardiner at UNM (https://orcid.org/0000-0002-8179-4919)
[Not Found] Finny Swamidoss at UNM



#### Hanlding new added authors
# Load necessary libraries
library(tidyr)

df <- read_excel("New list for faculty collaborations.xlsx", col_names = FALSE)
df_split <- df %>%
  # Separate the first column (...1) into Last Name and First Name at the comma
  separate(col = ...1, into = c("Last Name", "First Name"), sep = ",", extra = "merge") %>%
  mutate(
    `Last Name` = trimws(`Last Name`),
    `First Name` = trimws(`First Name`),
    Institution = trimws(...4) # Using the 4th column for Institution
  ) %>%
  # Select only the three requested columns
  select(`Last Name`, `First Name`, Institution)

# 3. Output to CSV
write.csv(df_split, "Faculty_Collaborations_Split.csv", row.names = FALSE)
print("The 3-column CSV 'Faculty_Collaborations_Split.csv' has been created.")

### compare with the original list

library(dplyr)
library(stringr)
library(fuzzyjoin)

# 1. Clean Faculty List (Remove punctuation)
faculty_prep <- df %>%
  separate(col = `...1`, into = c("Last_Name", "First_Name"), sep = ",", extra = "merge") %>%
  mutate(
    # Remove periods and commas for a cleaner match
    clean_last  = str_replace_all(str_to_upper(trimws(Last_Name)), "[[:punct:]]", ""),
    clean_first = str_replace_all(str_to_upper(trimws(First_Name)), "[[:punct:]]", ""),
    match_key   = paste(clean_last, clean_first) # No comma in key
  )

# 2. Clean your authors_df
authors_prep <- authors_df %>%
  mutate(
    # Replace 'name' with your actual column name
    # Remove punctuation and standardize to uppercase
    author_match_key = str_replace_all(str_to_upper(trimws(name)), "[[:punct:]]", "")
  )

# 3. Perform Ultra-Fuzzy Match
# We increase max_dist to 10 to catch "Last, First Middle" vs "Last, First"
matched_results <- stringdist_inner_join(
  faculty_prep, 
  authors_prep, 
  by = c("match_key" = "author_match_key"),
  max_dist = 10,  
  method = "lv"
)

# 4. View Matches
final_matches <- matched_results %>%
  select(
    Faculty_Name = match_key, 
    Author_DF_Name = author_match_key
  ) %>%
  distinct()

print(final_matches)

########################
#### Latest code: 2026-01-15
library(openalexR)
library(tidyverse)
library(knitr)

# --- 1. The search_author Function ---
# --- 1. The search_author Function ---
# Moved to my_functions.R

# --- 2. ROR Definitions ---
ua_ror        <- "03m2x1q45"
asu_ror       <- "03efmqc40"
unm_main_ror  <- "05fs6jp91"
unm_hos_ror   <- "04skph061"
niddk_ror     <- "00adh9b73"

# --- 3. Load the CSV ---
csv_file <- "ua_asu_unm_grant_authors2.csv"
authors_raw <- read_csv(csv_file)

# Standardize column names
colnames(authors_raw) <- trimws(colnames(authors_raw))

authors_df <- authors_raw %>%
  mutate(full_name = paste(trimws(`First Name`), trimws(`Last Name`)))

# --- 4. Step 1: Resolve IDs and Categorize ---
message("\n>>> RESOLVING AUTHORS ON OPENALEX...")

found_list <- list()
not_found_list <- list()

for (i in 1:nrow(authors_df)) {
  name <- authors_df$full_name[i]
  inst <- trimws(authors_df$Institution[i])
  
  primary_ror <- case_when(
    inst == "UA"    ~ ua_ror,
    inst == "ASU"   ~ asu_ror,
    inst == "UNM"   ~ unm_main_ror,
    inst == "NIDDK" ~ niddk_ror,
    TRUE            ~ as.character(NA)
  )
  
  res <- search_author(name, primary_ror)
  
  # UNM Fallback Logic
  if (is.null(res) && inst == "UNM") {
    message("  [Fallback] ", name, " not found at UNM Main. Trying UNM Hospital...")
    res <- search_author(name, unm_hos_ror)
  }
  
  if (!is.null(res)) {
    match <- res[1, ]
    match$csv_institution <- inst
    found_list[[name]] <- match
  } else {
    not_found_list[[name]] <- authors_df[i, ]
  }
}

# --- 5. PRINT SUMMARIES AND SAVE RESULTS TO CSV ---

# Found Table
if (length(found_list) > 0) {
  found_df <- bind_rows(found_list)
  cat("\n========================================================\n")
  cat("✅ AUTHORS IDENTIFIED IN OPENALEX\n")
  cat("========================================================\n")
  found_df %>%
    select(display_name, id, works_count, csv_institution) %>%
    kable() %>%
    print()
  
  # Save identified authors to CSV
  write_csv(found_df, "authors_identified.csv")
  message("Success: Identified authors saved to 'authors_identified.csv'")
}

# Not Found Table
if (length(not_found_list) > 0) {
  not_found_df <- bind_rows(not_found_list)
  cat("\n========================================================\n")
  cat("❌ AUTHORS NOT FOUND (Manual Check Needed)\n")
  cat("========================================================\n")
  not_found_df %>%
    select(`First Name`, `Last Name`, Institution) %>%
    kable() %>%
    print()
  
  # Save not found authors to CSV
  write_csv(not_found_df, "authors_not_found.csv")
  message("Warning: Not found authors saved to 'authors_not_found.csv'")
}

#######################################################
####################### 2026-01-26
####################################
########################################################
# --- Section 6: Robust Collaboration Search using Authorships ---



if (length(found_list) < 2) {
  stop("\nNot enough authors were identified to perform a cross-reference search.")
}

message("\n>>> CROSS-REFERENCING CO-PUBLICATIONS...")

# 1. Create a master list of all verified OpenAlex IDs in the group
all_group_ids <- sapply(found_list, function(x) x$id)
verified_names <- names(found_list)

all_collabs <- list()

# 2. Loop through each focal author to scan their works
for (focal_name in verified_names) {
  focal_id <- found_list[[focal_name]]$id
  message("Scanning bibliography for: ", focal_name)
  
  # Fetch all works for the focal author (Last 5 Years)
  five_years_ago <- "2021-01-01"
  
  works <- oa_fetch(
    entity = "works", 
    author.id = focal_id, 
    from_publication_date = five_years_ago,
    verbose = FALSE
  )
  
  if (!is.null(works) && nrow(works) > 0) {
    
    # Identify IDs in the group excluding the current focal author
    other_group_ids <- all_group_ids[all_group_ids != focal_id]
    
    # 3. Filter works where the 'authorships' data frame contains a matching ID
    # We use map_lgl to look inside the list-column for each row
    matches <- works %>%
      filter(map_lgl(authorships, function(auth_df) {
        if (is.null(auth_df) || nrow(auth_df) == 0) return(FALSE)
        
        # Check if any ID in the 'id' column of the authorship DF 
        # is present in our list of other group IDs
        any(auth_df$id %in% other_group_ids)
      }))
    
    # 4. If co-publications are found, extract the names of the matched collaborators
    if (nrow(matches) > 0) {
      matches <- matches %>%
        mutate(
          focal_author = focal_name,
          # Extract the display names of the specific collaborators from the group
          csv_collaborators = map_chr(authorships, function(auth_df) {
            matched_names <- auth_df$display_name[auth_df$id %in% other_group_ids]
            paste(unique(matched_names), collapse = "; ")
          })
        )
      
      all_collabs[[focal_name]] <- matches
      message("  --> Found ", nrow(matches), " group co-publications.")
    }
  }
}

colnames(final_report)

if (length(all_collabs) > 0) {
  final_report <- bind_rows(all_collabs) %>% 
    distinct(id, .keep_all = TRUE)
  
  # Display to console (unchanged)
  final_report %>%
    select(`Focal Author` = focal_author, 
           `Collaborator(s)` = csv_collaborators, 
           Title = display_name, 
           Year = publication_year, 
           DOI = doi) %>%
    knitr::kable(caption = "Internal Collaborations Identified") %>%
    print()
  
  # FIX: Flatten lists AND Truncate long strings
  final_report_export <- final_report %>%
    # 1. Flatten lists to text
    mutate(across(where(is.list), ~ sapply(., function(x) paste(unlist(x), collapse = "; ")))) %>%
    # 2. Truncate to 32,000 chars to satisfy Excel limit
    mutate(across(where(is.character), ~ substring(., 1, 32000)))
  
  writexl::write_xlsx(final_report_export, "found_collaborations_summary.xlsx")
  message("\nResults saved to 'found_collaborations_summary.xlsx'")
  
} else {
  message("\nNo internal collaborations were detected among the identified authors.")
}

# Select only the clean columns for Excel
clean_export <- final_report %>%
  select(`Focal Author` = focal_author, 
         `Collaborator(s)` = csv_collaborators, 
         Title = display_name, 
         Year = publication_year, 
         DOI = doi)

writexl::write_xlsx(clean_export, "found_collaborations_short_summary.xlsx")















