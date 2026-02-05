install.packages("readxl")

library(readxl)
source("my_functions.R")


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

########################
#### Latest code: 2026-01-15
library(openalexR)
library(tidyverse)
library(knitr)

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

#######################
# --- Manual Additions ---
# Example for one author
manual_id   <- "https://openalex.org/A5078276804" # Replace with actual ID
manual_name <- "Tatiana Kalin"                     # Must match the name in your CSV exactly
manual_inst <- "UA"                                # "UA", "ASU", "UNM"

manual_id   <- "https://openalex.org/A5078276804" # Replace with actual ID
manual_name <- "Tatiana Kalin"                     # Must match the name in your CSV exactly
manual_inst <- "UA"                                # "UA", "ASU", "UNM"


# Fetch the author data 
manual_author <- oa_fetch(entity = "authors", identifier = manual_id)

if (!is.null(manual_author) && nrow(manual_author) > 0) {
  # Add the required institution column
  manual_match <- manual_author[1, ]
  manual_match$csv_institution <- manual_inst
  
  # Add to found_list
  found_list[[manual_name]] <- manual_match
  
  # Optional: Remove from not_found_list so it doesn't show up there
  not_found_list[[manual_name]] <- NULL
  
  message("Manually added: ", manual_name)
}

#######################################################
####################### 2026-01-26
# Step 2:
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















