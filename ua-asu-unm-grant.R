
###### Use Claude Sonnet 5 ($$$) and Opus 5 ($$$$$)
### 2026-08-15:  Opus 5 has issues. often not output all the code: Gave up
### 2026-08-15: Use Sonnet 5 instead.

options(openalexR.apikey = Sys.getenv("OPENALEXR_APIKEY"))
PATH <- "/home/yhan/Documents/biblio-analysis"

setwd(PATH)
getwd()
print(here())
source("my_functions.R")

########################
#### 1st code: 2026-01-15, updated: 2026-02-13; updated: 2026-08-11
 
# --- 2. ROR Definitions ---
ua_ror        <- "03m2x1q45"
asu_ror       <- "03efmqc40"
unm_main_ror  <- "05fs6jp91"
unm_hos_ror   <- "04skph061"
niddk_ror     <- "00adh9b73"

# --- 3. Load the CSV ---
#csv_file <- "ua_asu_unm_grant_authors_fin.csv"
authors_raw <- read_csv(csv_file)

### 2026-08 READ XSLX
# Read the Excel file
library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

# Read the Excel file
data <- read_excel("FY27_SWDRC.xlsx")

# Process the Name column
data_processed <- data %>%
  # Remove anything in parentheses
  mutate(Name = str_trim(str_remove(Name, "\\s*\\([^)]*\\)"))) %>%
  # Remove multiple titles
  mutate(Name = str_trim(str_remove_all(Name, regex("(Dr\\.?|Prof\\.?|Mr\\.?|Mrs\\.?|Ms\\.?|Sir|Dame|Jr\\.?|Sr\\.?)\\s+", ignore_case = TRUE)))) %>%
  # Split by comma (Last_Name, First_Name format)
  separate_wider_delim(
    Name,
    delim = ",",
    names = c("Last_Name", "First_Name"),
    too_few = "align_start"
  ) %>%
  mutate(
    First_Name = str_trim(First_Name),
    Last_Name = str_trim(Last_Name)
  ) %>%
  select(First_Name, Last_Name, Institution, College, everything())

# Fix the Institution mapping to handle "NIDDK" with variations
authors_df <- data_processed %>%
  mutate(full_name = paste(trimws(First_Name), trimws(Last_Name))) %>%
  # Standardize Institution - treat any NIDDK variant as "NIDDK"
  mutate(Institution = case_when(
    str_detect(Institution, regex("NIDDK", ignore_case = TRUE)) ~ "NIDDK",
    TRUE ~ Institution
  ))

head(data_processed)
library(dplyr)
library(stringr)

# Quick test with sample values
inst <- c("UA", "UA - PHX", "UA- PHX", "ASU", "NIDDK")

primary_ror <- case_when(
  str_detect(inst, regex("^UA", ignore_case = TRUE)) ~ "UA_ROR_PLACEHOLDER",
  inst == "ASU"   ~ "ASU_ROR_PLACEHOLDER",
  inst == "UNM"   ~ "UNM_ROR_PLACEHOLDER",
  inst == "NIDDK" ~ "NIDDK_ROR_PLACEHOLDER",
  TRUE            ~ as.character(NA)
)

print(data.frame(inst, primary_ror))

#authors_df <- authors_raw %>% mutate(full_name = paste(trimws(First_Name), trimws(Last_Name)))

# --- 4. Step 1: Resolve IDs and Categorize ---
message("\n>>> RESOLVING AUTHORS ON OPENALEX...")

found_list <- list()
not_found_list <- list()


for (i in 1:nrow(authors_df)) {
  name <- authors_df$full_name[i]
  inst <- trimws(authors_df$Institution[i])
  
  primary_ror <- case_when(
    str_detect(inst, regex("^UA", ignore_case = TRUE)) ~ ua_ror,
    inst == "ASU"   ~ asu_ror,
    inst == "UNM"   ~ unm_main_ror,
    inst == "NIDDK" ~ niddk_ror,
    TRUE            ~ as.character(NA)
  )
  
  res <- search_author(name, primary_ror)
  
  # Fallback: try with just first word of first name (drop middle name)
  if (is.null(res)) {
    short_name <- paste(word(authors_df$First_Name[i], 1), authors_df$Last_Name[i])
    if (short_name != name) {
      message("  [Retry] Trying shortened name: ", short_name)
      res <- search_author(short_name, primary_ror)
    }
  }
  
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
    select(`First_Name`, `Last_Name`, Institution) %>%
    kable() %>%
    print()
  
  # Save not found authors to CSV
  write_csv(not_found_df, "authors_not_found.csv")
  message("Warning: Not found authors saved to 'authors_not_found.csv'")
}

========================================================
  ❌ AUTHORS NOT FOUND (Manual Check Needed)
========================================================
  
  
  |First_Name   |Last_Name  |Institution |
  |:------------|:----------|:-----------|
  |FRANK C      |BROSIUS    |ASU         |
  |ELIZABETH A. |REIFSNIDER |ASU         |
  |RONALDIP     |BANERJEE   |UA          |
  |JEAN M.      |WILSON     |UA          |
  |MELISSA      |CHAMBERS   |UA - PHX    |


candidates_brosius <- oa_fetch(entity = "authors", search = "Frank Brosius", verbose = FALSE)
print(candidates_brosius[, c("display_name", "id", "works_count", "last_known_institutions")])

candidates_reifsnider <- oa_fetch(entity = "authors", search = "Elizabeth Reifsnider", verbose = FALSE)
print(candidates_reifsnider[, c("display_name", "id", "works_count", "last_known_institutions")])

candidates_wilson <- oa_fetch(entity = "authors", search = "Jean Wilson", verbose = FALSE)
print(candidates_wilson[, c("display_name", "id", "works_count", "last_known_institutions")])

candidates_chambers <- oa_fetch(entity = "authors", search = "Melissa Chambers", verbose = FALSE)
print(candidates_chambers[, c("display_name", "id", "works_count", "last_known_institutions")])

## 2026-08-15: Manual add

manual_authors_batch <- tribble(
  ~id, ~name, ~inst,
  # Fixed key-matching entries:
  "https://openalex.org/A5003676386", "FRANK C BROSIUS", "ASU",
  "https://openalex.org/a5066121781", "RONALDIP BANERJEE", "UA",
  
  # Fill in after running oa_fetch searches above:
  "https://openalex.org/aXXXXXXXXXX", "ELIZABETH A. REIFSNIDER", "ASU",
  "https://openalex.org/aXXXXXXXXXX", "JEAN M. WILSON", "UA",
  "https://openalex.org/aXXXXXXXXXX", "MELISSA CHAMBERS", "UA - PHX"
)

  
###############################################################
####################### Code for 2026-01: DO NOT USE for late grant!!! 
### Not found
### Megan Camey
## |Finny       |Swamidoss     |UNM         |
## |Kathleen    |Rogers        |UA          | >> Ohio State?? 
# Reza Shekarriz , Albuquerque, or Shahid Beheshti University ??

# [Not Found] Henry Tseng at ASU (Jui-Heng Tseng)
# [Not Found] Ken Buetow at ASU (Kennith Buetow)
# [Not Found] Bill Shuttleworth at UNM (William )

[Not Found] Haijiang Cai at UA
[Not Found] Michael Daines at UA

# Manually added: [Not Found] Tatiana Kalin at UA (Cincinnati https://api.openalex.org/a5078276804 )
# [Not Found] Moulun Luo at UA

# [Not Found] Mary laura Thomas at ASU (Mary Laura Lind)
# !!! [Not Found] Sampath Rangasamy at ASU (Arizona Research Center?, Phoenix, DO Check openAlex manually!!!)

========================================================
  ❌ AUTHORS NOT FOUND (Manual Check Needed)
========================================================
  
  
  |First_Name |Last_Name     |Institution |
  |:----------|:-------------|:-----------|
  |James      |BIbb          |UA          |
  |Nipavan    |Chiamvimonvat |UA          | >>> UC Davis
|Michael    |Daines        |UA          |
  |Tatiana    |Kalin         |UA          | >> Cincinati 
|Moulun     |Luo           |UA          |
  |Yanqiao    |Zhang         |UA          |
  |Sampath    |Rangasamy     |ASU         |
  |Gaberiel   |Shaibi        |ASU         |
  |Vincent    |Pizziconi     |ASU         |
  |Nathan     |Zaidman       |UNM         |
  |Eliseo     |Castillo      |UNM         |
  |f          |Clark         |UNM         |
  |Michael    |Deyhle        |UNM         |
  |Amy        |Gardiner      |UNM         |
  |Finny      |Swamidoss     |UNM         |
  |Kathleen   |Rogers        |UA          |
  |Reza       |Shekarriz     |UNM         |
  
  [Not Found] Olga Ponomarova at UNM (https://orcid.org/0000-0001-6331-9949 ). Handle differently
Amy Gardiner at UNM (https://orcid.org/0000-0002-8179-4919)

# --- Manual Additions (Batch Processing) ---
manual_id   <- "https://openalex.org/a5033254684"  # 
manual_name <- "Amy Gardiner"             # Must match the name in your CSV exactly
manual_inst <- "UNM"                                # "UA", "ASU", "UNM"

manual_author <- oa_fetch(entity = "authors", identifier = manual_id)
if (!is.null(manual_author) && nrow(manual_author) > 0) {
  # Add the required institution column
  manual_match <- manual_author[1, ]
  manual_match$csv_institution <- manual_inst
  found_list[[manual_name]] <- manual_match
  not_found_list[[manual_name]] <- NULL
  message("Manually added: ", manual_name)
}

#### 2026-01: First Grant
# Define manual entries here. Add new lines as needed.
manual_authors_batch <- tribble(
  ~id, ~name, ~inst,
  "https://openalex.org/a5012045039", "James Bibb", "UA",
  "https://openalex.org/A5031817215", "Nipavan Chiamvimonvat", "UA",
  "https://openalex.org/a5006730507", "Michael Daines", "UA",
  "https://openalex.org/A5078276804", "Tatiana Kalin", "UA",
  "https://openalex.org/a5055276714", "Julie Ledford", "UA",
  "https://openalex.org/A5100695723", "Moulun Luo", "UA",
  "https://openalex.org/A5045587465", "Liya Yin", "UA",
  "https://openalex.org/a5066121781", "Banerjee Ronaldip", "UA",
  "https://openalex.org/a5002109010", "Sampath Rangasamy", "ASU",
  "https://openalex.org/a5020644260", "Vincent Pizziconi", "ASU",
  "https://openalex.org/a5018559401", "Eliseo Castillo", "UNM",
  "https://openalex.org/a5068225719", "Michael Deyhle", "UNM",
  "https://openalex.org/a5033254684", "Amy Gardiner", "UNM",
  "https://openalex.org/a5110488400", "Marylaura Thomas", "ASU"
    # Add more below:
    # "ID", "Name", "Institution"
)

# NOT FOUND by Yan 
# Finny      |Swamidoss     |UNM         |
# |Kathleen   |Rogers        |UA          |
#  |Reza       |Shekarriz     |UNM 
# |Megan      |Camey         |UA          |
  

message(paste("\nProcessing", nrow(manual_authors_batch), "manual additions..."))

for (i in 1:nrow(manual_authors_batch)) {
  m_id   <- manual_authors_batch$id[i]
  m_name <- manual_authors_batch$name[i]
  m_inst <- manual_authors_batch$inst[i]
  
  # Fetch the author data 
  # Check if ID looks valid (basic check)
  if (!is.na(m_id) && m_id != "") {
      tryCatch({
        Sys.sleep(1)
        manual_author <- oa_fetch(entity = "authors", identifier = m_id)
        
        if (!is.null(manual_author) && nrow(manual_author) > 0) {
          # Add the required institution column
          manual_match <- manual_author[1, ]
          manual_match$csv_institution <- m_inst
          
          # Add to found_list
          found_list[[m_name]] <- manual_match
          
          # Remove from not_found_list
          not_found_list[[m_name]] <- NULL
          
          message("Manually added: ", m_name)
        } else {
          message("Failed to fetch manual author: ", m_name, " (ID: ", m_id, ")")
        }
      }, error = function(e) {
        message("Error fetching ", m_name, ": ", e$message)
      })
  }
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
    type= "article",
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

# --- 7. Count Total Collaborations per Author ---
if (exists("final_report") && nrow(final_report) > 0) {
  
  message("\n>>> COUNTING COLLABORATIONS PER AUTHOR...")
  
  # Initialize counts
  # We want to count how many papers each 'found' author is involved in within final_report
  
  # 1. Get the mapping of ID -> Name
  # found_list is a list of 1-row dataframes
  if (exists("found_list") && length(found_list) > 0) {
      if (requireNamespace("purrr", quietly = TRUE)) {
        group_id_map <- setNames(names(found_list), purrr::map_chr(found_list, "id"))
      } else {
        # Fallback without purrr
        group_id_map <- setNames(names(found_list), sapply(found_list, function(x) x$id))
      }
      
      # 2. Extract author IDs involved in each paper
      all_ids_in_papers <- final_report$authorships %>%
        lapply(function(df) {
          if (is.null(df) || nrow(df) == 0) return(character(0))
          # Return IDs that match our group
          df$id[df$id %in% names(group_id_map)]
        }) 
      
      # Flatten to a long format: (PaperIndex, AuthorID)
      # We just need counts per author
      all_ids_flat <- unlist(all_ids_in_papers)
      
      if (length(all_ids_flat) > 0) {
          author_counts_df <- data.frame(ID = all_ids_flat, stringsAsFactors = FALSE) %>%
            count(ID, name = "Collaborations") %>%
            mutate(Name = group_id_map[ID]) %>%
            select(Name, Collaborations) %>%
            arrange(desc(Collaborations))
          
          # Print to console
          print(author_counts_df)
          
          # Save to CSV
          readr::write_csv(author_counts_df, "author_collaboration_counts.csv")
          message("Results saved to 'author_collaboration_counts.csv'")
      } else {
        message("No group authors found in the final report authorships (unexpected).")
      }
      
  } else {
      message("found_list is missing. Cannot map IDs to names.")
  }

} else {
  message("No final_report found or it is empty. Cannot count collaborations.")
}




############################# Testing Claude Haiku 4.5
library(openalexR)
library(dplyr)
library(knitr)
 

# Create a dataframe with the found authors
found_authors <- tribble(
  ~display_name, ~id, ~works_count, ~csv_institution,
  "Matthew P. Buman", "https://openalex.org/A5000559212", 312, "ASU",
  "Ellen P. Green", "https://openalex.org/A5047947143", 19, "ASU",
  "Rodney P. Joseph", "https://openalex.org/A5053124087", 71, "ASU",
  "Christos S. Katsanos", "https://openalex.org/A5074138380", 109, "ASU",
  "Min‐Hyun Kim", "https://openalex.org/A5024956976", 38, "ASU",
  "Rosa Krajmalnik‐Brown", "https://openalex.org/A5035488966", 213, "ASU",
  "Joshua LaBaer", "https://openalex.org/A5046993686", 418, "ASU",
  "Linda J. Luecken", "https://openalex.org/A5073812178", 149, "ASU",
  "Miyeko Mana", "https://openalex.org/A5028420772", 69, "ASU",
  "Eyitayo Omolara Owolabi", "https://openalex.org/A5006639752", 79, "ASU",
  "Adewale L. Oyeyemi", "https://openalex.org/A5066047427", 175, "ASU",
  "Marisol Pérez", "https://openalex.org/A5013609965", 129, "ASU",
  "Bing Si", "https://openalex.org/A5053304698", 39, "ASU",
  "Taichi A. Suzuki", "https://openalex.org/A5008343733", 53, "ASU",
  "Corrie M. Whisner", "https://openalex.org/A5003624984", 132, "ASU",
  "Leslie J. Baier", "https://openalex.org/A5041569085", 219, "NIDDK",
  "Robert L. Hanson", "https://openalex.org/A5049047999", 582, "NIDDK",
  "Halimatou Alaofè", "https://openalex.org/A5078896557", 75, "UA",
  "Wei Zhou", "https://openalex.org/A5100640843", 170, "UA"
)

# Verify each author by extracting the A-number and fetching from OpenAlex
verification_results <- found_authors %>%
  mutate(
    a_number = str_extract(id, "A\\d+"),
    api_url = paste0("https://api.openalex.org/authors/", a_number)
  ) %>%
  rowwise() %>%
  mutate(
    verification = tryCatch({
      response <- httr::GET(api_url)
      if (httr::status_code(response) == 200) {
        author_data <- jsonlite::fromJSON(httr::content(response, "text"))
        paste0(
          "✓ VERIFIED | Name: ", author_data$display_name, 
          " | Works: ", author_data$works_count,
          " | Last Known Affiliation: ", 
          ifelse(!is.null(author_data$last_known_institution$display_name),
                 author_data$last_known_institution$display_name, "Not listed")
        )
      } else {
        "✗ NOT FOUND (404)"
      }
    }, error = function(e) paste0("✗ ERROR: ", e$message))
  ) %>%
  ungroup() %>%
  select(display_name, a_number, verification, csv_institution)

# Display results
kable(verification_results, format = "markdown")

# Summary
cat("\n\n=== VERIFICATION SUMMARY ===\n")
cat("Total authors checked:", nrow(verification_results), "\n")
cat("Verified:", sum(str_detect(verification_results$verification, "^✓")), "\n")
cat("Not found:", sum(str_detect(verification_results$verification, "^✗ NOT FOUND")), "\n")
cat("Errors:", sum(str_detect(verification_results$verification, "^✗ ERROR")), "\n")

# Save detailed results
write_csv(verification_results, "author_verification_results.csv")













