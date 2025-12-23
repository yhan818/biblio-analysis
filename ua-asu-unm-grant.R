install.packages("readxl")

library(openalexR)
library(tidyverse)
library(readxl)
library(knitr)

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
res <- search_author("Terry Badger", u_arizona_ror)
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


################################ The above shall go to myfunctions.

library(openalexR)
library(tidyverse)
library(knitr)

# First try
# --- 1. Your Specific ROR Mapping ---
ror_map <- c(
  "UA"    = "03m2x1q45",
  "ASU"   = "03efmqc40",
  "UNM"   = "05fs6jp91",
  "NIDDK" = "00adh9b73"
)

# --- 2. Load and Prepare Author Data ---
csv_file <- "ua_asu_unm_grant_authors.csv"
authors_df <- read_csv(csv_file)

# Standardize column names (removes trailing spaces)
colnames(authors_df) <- trimws(colnames(authors_df))

# Combine names and map the RORs from your list
authors_df <- authors_df %>%
  mutate(
    full_name = paste(trimws(`First Name`), trimws(`Last Name`)),
    target_ror = ror_map[trimws(Institution)]
  )

# --- 3. Step 1: Resolve OpenAlex IDs ---
# This ensures we are matching unique author IDs, not just names
message("Step 1: Resolving OpenAlex IDs for the group...")
author_metadata <- list()

for (i in 1:nrow(authors_df)) {
  name <- authors_df$full_name[i]
  ror  <- authors_df$target_ror[i]
  inst <- authors_df$Institution[i]
  
  if (is.na(ror)) {
    message("  [Skip] ", name, " - Institution '", inst, "' not in your ROR list.")
    next
  }
  
  # Calling your verified search_author function
  res <- search_author(name, ror)
  
  if (!is.null(res)) {
    author_metadata[[name]] <- res[1, ]
    message("  [Found] ", name, " (", inst, ")")
  } else {
    message("  [Not Found] ", name, " at ", inst)
  }
}


#################

# [Not Found] Henry Tseng at ASU (Jui-Heng Tseng)
# [Not Found] Ken Buetow at ASU (Kennith Buetow)
# [Not Found] Bill Shuttleworth at UNM (William )


[Not Found] Hossein Ardehali at UA
[Not Found] Haijiang Cai at UA
[Not Found] Michael Daines at UA
[Not Found] Tatiana Kalin at UA
[Not Found] Moulun Luo at UA



# [Not Found] Mary laura Thomas at ASU (Use Mary Laura Lind)
# !!! [Not Found] Sampath Rangasamy at ASU (Arizona Research Center?, Phoenix, DO Check openAlex manually!!!)


[Not Found] Olga Ponomarova at UNM (https://orcid.org/0000-0001-6331-9949 ). Handle differently
[Not Found] Amy Gardiner at UNM (https://orcid.org/0000-0002-8179-4919)

[Not Found] Finny Swamidoss at UNM



###### 2nd Try: 
# --- 1. Define Primary and Fallback RORs ---
ua_ror    <- "03m2x1q45"
asu_ror   <- "03efmqc40"
niddk_ror <- "00adh9b73"

# UNM has two possibilities
unm_main_ror  <- "05fs6jp91" 
unm_hos_ror <- "04skph061" 

# --- 1. ROR Definitions ---
ua_ror        <- "03m2x1q45"
asu_ror       <- "03efmqc40"
unm_hsc_ror   <- "05fs6jp91"
unm_main_ror  <- "01jc9as71"
niddk_ror     <- "00adh9b73"


library(openalexR)
library(tidyverse)
library(knitr)

# --- 1. The search_author Function ---
search_author <- function(author_name, affiliation_ror) {
  base_dir <- file.path("./", "output")
  if (!dir.exists(base_dir)) { dir.create(base_dir, recursive = TRUE) }
  safe_name <- gsub("[^[:alnum:]]", "_", author_name)
  log_file <- file.path(base_dir, paste0(safe_name, ".log"))
  
  author_results <- tryCatch({
    # Using the updated 'authors' entity
    oa_fetch(entity = "authors", search = author_name)
  }, error = function(e) {
    cat(paste0(Sys.time(), " Error: ", e$message, "\n"), file = log_file, append = TRUE)
    return(NULL)
  })
  
  if (!is.null(author_results) && nrow(author_results) > 0) {
    # Check for singular or plural institution column name
    inst_col <- intersect(names(author_results), c("last_known_institution", "last_known_institutions"))
    
    if (length(inst_col) == 0) return(NULL)
    
    matches <- sapply(seq_len(nrow(author_results)), function(i) {
      inst_info <- author_results[[inst_col]][[i]]
      if (is.null(inst_info)) return(FALSE)
      
      # Handle if inst_info is a data frame or a list
      if (is.data.frame(inst_info) && "ror" %in% names(inst_info)) {
        return(any(grepl(affiliation_ror, inst_info$ror, ignore.case = TRUE)))
      }
      if (is.list(inst_info) && "ror" %in% names(inst_info)) {
        return(grepl(affiliation_ror, inst_info$ror, ignore.case = TRUE))
      }
      return(FALSE)
    })
    
    filtered <- author_results[matches, ]
    return(if(nrow(filtered) > 0) filtered else NULL)
  }
  return(NULL)
}

# --- 2. ROR Definitions (Updated) ---
ua_ror        <- "03m2x1q45"
asu_ror       <- "03efmqc40"
unm_main_ror  <- "05fs6jp91"
unm_hos_ror   <- "04skph061"
niddk_ror     <- "00adh9b73"

# --- 3. Load the CSV ---
csv_file <- "ua_asu_unm_grant_authors.csv"
authors_raw <- read_csv(csv_file)

# Standardize column names (remove trailing spaces)
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
  
  # Map Primary ROR based on Institution column
  primary_ror <- case_when(
    inst == "UA"    ~ ua_ror,
    inst == "ASU"   ~ asu_ror,
    inst == "UNM"   ~ unm_main_ror,
    inst == "NIDDK" ~ niddk_ror,
    TRUE            ~ as.character(NA)
  )
  
  # Attempt search
  res <- search_author(name, primary_ror)
  
  # UNM Fallback to Hospital ROR if main returns nothing
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

# --- 5. PRINT SUMMARIES TO SCREEN ---

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
}

#####################3 CONTINUE WORKING ON THESE #############################
# --- 6. Step 2: Cross-Reference for Collaborations ---
if (length(found_list) < 2) {
  stop("\nNot enough authors found to check for co-publications.")
}

message("\n>>> CROSS-REFERENCING CO-PUBLICATIONS...")
verified_ids <- sapply(found_list, function(x) x$id)
all_collabs <- list()

for (focal_name in names(verified_ids)) {
  focal_id <- verified_ids[focal_name]
  message("Scanning bibliography for: ", focal_name)
  
  works <- oa_fetch(entity = "works", author.id = focal_id, verbose = FALSE)
  
  if (!is.null(works) && nrow(works) > 0) {
    other_ids <- verified_ids[verified_ids != focal_id]
    
    # Check if any author in the paper exists in our 'other_ids' list
    matches <- works %>%
      filter(map_lgl(author, ~ any(.x$au_id %in% other_ids)))
    
    if (nrow(matches) > 0) {
      matches <- matches %>%
        mutate(
          focal_author = focal_name,
          csv_collaborators = map_chr(author, function(a) {
            names_match <- a$au_display_name[a$au_id %in% other_ids]
            paste(unique(names_match), collapse = "; ")
          })
        )
      all_collabs[[focal_name]] <- matches
    }
  }
}

# --- 7. Print Final Collaboration Results ---
if (length(all_collabs) > 0) {
  final_report <- bind_rows(all_collabs) %>% distinct(id, .keep_all = TRUE)
  
  cat("\n========================================================\n")
  cat("🔗 INTERNAL CO-PUBLICATIONS FOUND\n")
  cat("========================================================\n")
  final_report %>%
    show_works() %>%
    select(`Focal Author` = focal_author, `Collaborator(s)` = csv_collaborators, 
           Title = display_name, Year = publication_year, DOI = doi) %>%
    kable() %>%
    print()
  
  write_csv(final_report, "found_collaborations_summary.csv")
} else {
  cat("\nNo internal collaborations were detected among these authors.\n")
}





####################3


# Create a mapping of IDs for the final cross-reference
group_ids <- sapply(author_metadata, function(x) x$id)
verified_names <- names(author_metadata)

# --- 4. Step 2: Cross-Reference Publications ---
message("\nStep 2: Checking for co-publications within the list...")
all_collaborations <- list()

for (current_name in verified_names) {
  target_id <- author_metadata[[current_name]]$id
  message("Searching works for: ", current_name)
  
  # Fetch author's works (entity = "works")
  works <- oa_fetch(entity = "works", author.id = target_id, verbose = FALSE)
  
  if (!is.null(works) && nrow(works) > 0) {
    # Everyone in the group EXCEPT the current author
    other_group_ids <- group_ids[group_ids != target_id]
    
    # Filter for papers where at least one other person from the CSV is an author
    co_authored_works <- works %>%
      filter(map_lgl(author, ~ any(.x$au_id %in% other_group_ids)))
    
    if (nrow(co_authored_works) > 0) {
      co_authored_works <- co_authored_works %>%
        mutate(
          focal_author = current_name,
          collaborators_from_csv = map_chr(author, function(auth_list) {
            # Find which names from our verified list appear in this paper
            matches <- auth_list$au_display_name[auth_list$au_id %in% other_group_ids]
            paste(unique(matches), collapse = "; ")
          })
        )
      all_collaborations[[current_name]] <- co_authored_works
    }
  }
}

# --- 5. Combine and Display ---
if (length(all_collaborations) > 0) {
  final_report <- bind_rows(all_collaborations)
  
  # Deduplicate: Since A&B and B&A are the same paper, we group by DOI
  unique_report <- final_report %>%
    distinct(id, .keep_all = TRUE) # 'id' here is the OpenAlex Work ID
  
  # Display formatted table
  unique_report %>%
    show_works() %>%
    select(collaborators_from_csv, display_name, publication_year, doi) %>%
    kable(caption = "Verified Group Collaborations")
  
  write_csv(unique_report, "verified_group_publications.csv")
} else {
  message("No internal collaborations found for this specific group.")
}