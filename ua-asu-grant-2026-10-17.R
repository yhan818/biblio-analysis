###############################################################################
# ua-asu-grant-2026-10-17.R  -- revised for openalexR >= 3.1.2 (2026-10)
# Purpose: resolve grant team authors in OpenAlex, download ALL their works
#          (articles, since 2021-01-01), and find internal co-publications.
###############################################################################

# ---- 0. Package version check ------------------------------------------------
required_version <- "3.1.2"
if (!requireNamespace("openalexR", quietly = TRUE) ||
    packageVersion("openalexR") < required_version) {
  install.packages("openalexR")
  stop("openalexR was installed/updated. Please RESTART R, then re-run this script.")
}

library(openalexR)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(readxl)
library(tibble)
library(knitr)
library(httr)
library(jsonlite)
library(writexl)
library(here)

message("openalexR version: ", packageVersion("openalexR"))

# ---- 1. Settings --------------------------------------------------------------
#options(openalexR.apikey = Sys.getenv("OPENALEXR_APIKEY"))
api_key <- Sys.getenv("OPENALEXR_APIKEY")
options(openalexR.apikey = api_key)
PATH <- "/home/yhan/Documents/biblio-analysis"
setwd(PATH)
getwd()
print(here())
source("my_functions.R")             # provides search_author()

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

FROM_DATE <- "2022-01-01"   # start of window
TO_DATE   <- "2025-12-31"   # end of window
WORK_TYPE            <- "article"
INCLUDE_EXPANSION    <- FALSE         # TRUE = also query expansion corpus (untested)
RUN_CANDIDATE_SEARCH <- FALSE         # TRUE = print candidate author lookups

# ---- 2. ROR definitions -------------------------------------------------------
ua_ror       <- "03m2x1q45"
asu_ror      <- "03efmqc40"
unm_main_ror <- "05fs6jp91"
unm_hos_ror  <- "04skph061"
niddk_ror    <- "00adh9b73"

# ---- 3. Load and clean the Excel author list ----------------------------------
data <- read_excel("DRC_list.xlsx")

data_processed <- data %>%
  mutate(Name = str_trim(str_remove(Name, "\\s*\\([^)]*\\)"))) %>%
  mutate(Name = str_trim(str_remove_all(
    Name,
    regex("(Dr\\.?|Prof\\.?|Mr\\.?|Mrs\\.?|Ms\\.?|Sir|Dame|Jr\\.?|Sr\\.?)\\s+",
          ignore_case = TRUE)))) %>%
  separate_wider_delim(Name, delim = ",",
                       names = c("Last_Name", "First_Name"),
                       too_few = "align_start") %>%
  mutate(First_Name = str_trim(First_Name),
         Last_Name  = str_trim(Last_Name)) %>%
  select(First_Name, Last_Name, Institution)

authors_df <- data_processed %>%
  mutate(
    First_Name      = str_squish(as.character(First_Name)),
    Last_Name       = str_squish(as.character(Last_Name)),
    Institution_raw = str_squish(as.character(Institution)),
    full_name       = str_squish(paste(First_Name, Last_Name)),
    Institution = case_when(
      str_detect(Institution_raw, regex("UNIVERSITY OF ARIZONA", ignore_case = TRUE)) ~ "UA",
      str_detect(Institution_raw, regex("ARIZONA STATE UNIVERSITY", ignore_case = TRUE)) ~ "ASU",
      str_detect(Institution_raw, regex("UNIVERSITY OF NEW MEXICO|\\bUNM\\b", ignore_case = TRUE)) ~ "UNM",
      str_detect(Institution_raw, regex("NIDDK|Phoenix Epidemiology.*Clinical Research Branch",
                                        ignore_case = TRUE)) ~ "NIDDK",
      TRUE ~ Institution_raw
    )
  ) %>%
  filter(!is.na(First_Name), !is.na(Last_Name),
         First_Name != "", Last_Name != "", full_name != "") %>%
  distinct(full_name, Institution, .keep_all = TRUE)

# ---- 4. Resolve authors on OpenAlex -------------------------------------------
message("\n>>> RESOLVING AUTHORS ON OPENALEX...")
found_list     <- list()
not_found_list <- list()

for (i in seq_len(nrow(authors_df))) {
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
  
  # Fallback: first word of first name only (drop middle name)
  if (is.null(res)) {
    short_name <- paste(word(authors_df$First_Name[i], 1), authors_df$Last_Name[i])
    if (short_name != name) {
      message("  [Retry] Trying shortened name: ", short_name)
      res <- search_author(short_name, primary_ror)
    }
  }
  
  # UNM fallback: try UNM Hospital
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

# ---- 4b. Optional: candidate lookups for manual matching ----------------------
if (RUN_CANDIDATE_SEARCH) {
  for (nm in c("Frank Brosius", "Elizabeth Reifsnider", "Jean Wilson", "Melissa M. Chambers")) {
    cand <- oa_fetch(entity = "authors", search = nm, verbose = FALSE)
    if (!is.null(cand)) {
      print(cand[, intersect(c("display_name", "id", "works_count", "last_known_institutions"),
                             names(cand))])
    }
  }
}
# ---- 4c. Manual additions (both batches, filtered to the current roster) ----

# Diagnostics: where did the 70 names go?
cat("\nRows in Excel file:            ", nrow(data), "\n")
cat("Unique authors after cleaning: ", nrow(authors_df), "\n")
cat("Found automatically:           ", length(found_list), "\n")
cat("Not found automatically:       ", length(not_found_list), "\n")
if (length(not_found_list) > 0) print(names(not_found_list))

# Name key: uppercase, drop punctuation and single-letter initials, sort words
name_key <- function(x) {
  toks <- str_split(str_squish(toupper(str_replace_all(x, "[^A-Za-z ]", " "))), " ")
  map_chr(toks, ~ paste(sort(.x[nchar(.x) > 1]), collapse = " "))
}

manual_all <- bind_rows(
  tribble(
    ~id,                                ~name,                     ~inst,
    "https://openalex.org/A5049313563", "FREDRIC E. WONDISFORD", "UA",
    "https://openalex.org/A5101914506", "YANQIAO ZHANG",         "UA",
    "https://openalex.org/A5000559212", "Matthew P. Burman",     "ASU",  # OpenAlex name: Buman
    "https://openalex.org/A5066121781", "RONALDIP BANERJEE",     "UA",
    "https://openalex.org/A5035488966", "Rose Krajmalnik-Brown", "ASU",  # OpenAlex name: Rosa
    "https://openalex.org/A5021883063", "JESSICA D. WAVER",      "ASU",
    "https://openalex.org/A5003676386", "FRANK C BROSIUS",       "ASU",
    "https://openalex.org/A5050987084", "ELIZABETH A. REIFSNIDER","ASU",
    "https://openalex.org/A5001871630", "MELISSA CHAMBERS",      "UA - PHX"
    # "https://openalex.org/A__________", "YI TAN",              "UA",   # add once confirmed
  ) %>% mutate(batch = "2026-08"),
)

roster <- authors_df %>% transmute(full_name, Institution, key = name_key(full_name))

manual_matched <- manual_all %>%
  mutate(key = name_key(name)) %>%
  distinct(key, .keep_all = TRUE) %>%            # 2026-08 listed first, so it wins
  left_join(roster, by = "key", relationship = "many-to-many")

unmatched <- manual_matched %>% filter(is.na(full_name))
if (nrow(unmatched) > 0) {
  message("\nManual entries NOT matched to anyone in DRC_list.xlsx (skipped):")
  print(unmatched %>% select(batch, name, id))
}

to_add <- manual_matched %>% filter(!is.na(full_name))
message("\nApplying ", nrow(to_add), " manual additions matched to the roster...")

for (i in seq_len(nrow(to_add))) {
  m_id       <- to_add$id[i]
  excel_name <- to_add$full_name[i]
  m_inst     <- coalesce(to_add$inst[i], to_add$Institution[i])
  
  existing <- found_list[[excel_name]]
  if (!is.null(existing) && toupper(basename(existing$id)) == toupper(basename(m_id))) {
    message("  Already found with same ID: ", excel_name)
    next
  }
  
  tryCatch({
    manual_author <- oa_fetch(entity = "authors", identifier = m_id)
    if (!is.null(manual_author) && nrow(manual_author) > 0) {
      manual_match <- manual_author[1, ]
      manual_match$csv_institution <- m_inst
      if (!is.null(existing)) message("  Replacing automatic match for ", excel_name,
                                      " (", existing$id, " -> ", m_id, ")")
      found_list[[excel_name]]     <- manual_match
      not_found_list[[excel_name]] <- NULL
      message("  Manually added: ", excel_name, " [", to_add$batch[i], "]")
    } else {
      message("  Failed to fetch: ", excel_name, " (", m_id, ")")
    }
  }, error = function(e) message("  Error fetching ", excel_name, ": ", conditionMessage(e)))
}

cat("\nAfter manual additions -> found:", length(found_list),
    "| still not found:", length(not_found_list), "\n")
if (length(not_found_list) > 0) print(names(not_found_list))


# ---- 5. Summaries and CSVs (after manual additions) ---------------------------
if (length(found_list) > 0) {
  found_df <- bind_rows(found_list)
  cat("\n========================================================\n")
  cat("✅ AUTHORS IDENTIFIED IN OPENALEX\n")
  cat("========================================================\n")
  found_df %>% select(display_name, id, works_count, csv_institution) %>% kable() %>% print()
  found_df %>%
    select(display_name, id, works_count, csv_institution) %>%
    write_csv("authors_identified.csv")
  message("Success: Identified authors saved to 'authors_identified.csv'")
}

if (length(not_found_list) > 0) {
  not_found_df <- bind_rows(not_found_list)
  cat("\n========================================================\n")
  cat("❌ AUTHORS NOT FOUND (Manual Check Needed)\n")
  cat("========================================================\n")
  not_found_df %>% select(First_Name, Last_Name, Institution) %>% kable() %>% print()
  write_csv(not_found_df, "authors_not_found.csv")
  message("Warning: Not found authors saved to 'authors_not_found.csv'")
}

# ---- 6. Helper functions for fetching works -----------------------------------
.chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])

# Build a works tibble from raw JSON list (used only if oa_fetch tibble fails)
build_works_from_list <- function(works_raw) {
  if (is.null(works_raw) || length(works_raw) == 0) return(NULL)
  tibble(
    id                  = map_chr(works_raw, ~ .chr(.x$id)),
    display_name        = map_chr(works_raw, ~ .chr(.x$display_name)),
    publication_year    = map_int(works_raw, ~ suppressWarnings(as.integer(.chr(.x$publication_year)))),
    publication_date    = map_chr(works_raw, ~ .chr(.x$publication_date)),
    doi                 = map_chr(works_raw, ~ .chr(.x$doi)),
    type                = map_chr(works_raw, ~ .chr(.x$type)),
    cited_by_count      = map_int(works_raw, ~ suppressWarnings(as.integer(.chr(.x$cited_by_count)))),
    source_display_name = map_chr(works_raw, ~ .chr(.x$primary_location$source$display_name)),
    authorships = map(works_raw, function(w) {
      if (length(w$authorships) == 0)
        return(tibble(id = character(), display_name = character()))
      tibble(
        id              = map_chr(w$authorships, ~ .chr(.x$author$id)),
        display_name    = map_chr(w$authorships, ~ .chr(.x$author$display_name)),
        orcid           = map_chr(w$authorships, ~ .chr(.x$author$orcid)),
        author_position = map_chr(w$authorships, ~ .chr(.x$author_position)),
        institutions    = map_chr(w$authorships, function(a)
          paste(map_chr(a$institutions %||% list(), ~ .chr(.x$display_name)), collapse = "; "))
      )
    })
  )
}

# Fetch all works for one author; fall back to raw JSON if conversion fails
fetch_author_works <- function(author_id, from_date = FROM_DATE, to_date = TO_DATE,
                               work_type = WORK_TYPE) {
  opts <- if (INCLUDE_EXPANSION) oa_options(corpus = "all") else NULL
  
  res <- tryCatch(
    oa_fetch(entity = "works", author.id = author_id,
             from_publication_date = from_date, to_publication_date = to_date,
             type = work_type, options = opts, verbose = FALSE),
    error = function(e) {
      message("  [Fallback] tibble conversion failed: ", conditionMessage(e))
      structure(list(), class = "oa_fail")
    }
  )
  
  if (inherits(res, "oa_fail")) {
    raw <- tryCatch(
      oa_fetch(entity = "works", author.id = author_id,
               from_publication_date = from_date, to_publication_date = to_date,
               type = work_type, options = opts, output = "list", verbose = FALSE),
      error = function(e) { message("  [Fallback] raw fetch failed: ", conditionMessage(e)); NULL }
    )
    res <- build_works_from_list(raw)
  }
  
  # Safety net: keep only works whose publication year is inside the window
  if (!is.null(res) && nrow(res) > 0) {
    yr_from <- as.integer(substr(from_date, 1, 4))
    yr_to   <- as.integer(substr(to_date, 1, 4))
    res <- res %>% filter(!is.na(publication_year),
                          publication_year >= yr_from, publication_year <= yr_to)
  }
  res
}

# Flatten any works tibble for Excel/CSV export
flatten_for_export <- function(df) {
  df %>%
    mutate(
      authors_names = map_chr(authorships, function(a)
        if (!is.data.frame(a) || nrow(a) == 0) "" else paste(a$display_name, collapse = "; ")),
      authors_ids = map_chr(authorships, function(a)
        if (!is.data.frame(a) || nrow(a) == 0) "" else paste(a$id, collapse = "; "))
    ) %>%
    select(-authorships) %>%
    mutate(across(where(is.list), ~ map_chr(., function(x) paste(unlist(x), collapse = "; ")))) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(everything(), ~ substring(., 1, 32000)))   # Excel cell limit
}

# ---- 7. Download ALL works and find internal co-publications ------------------
if (length(found_list) < 2) {
  stop("\nNot enough authors were identified to perform a cross-reference search.")
}

message("\n>>> DOWNLOADING WORKS AND CROSS-REFERENCING CO-PUBLICATIONS...")
all_group_ids  <- map_chr(found_list, ~ .x$id)
verified_names <- names(found_list)
all_works      <- list()
all_collabs    <- list()

for (focal_name in verified_names) {
  focal_id <- found_list[[focal_name]]$id
  message("Scanning bibliography for: ", focal_name)
  
  works <- fetch_author_works(focal_id)
  if (is.null(works) || nrow(works) == 0) {
    message("  --> No works returned.")
    next
  }
  
  works <- works %>% mutate(focal_author = focal_name, focal_author_id = focal_id)
  all_works[[focal_name]] <- works
  message("  --> ", nrow(works), " works downloaded.")
  
  other_group_ids <- all_group_ids[all_group_ids != focal_id]
  
  matches <- works %>%
    filter(map_lgl(authorships, function(auth_df) {
      if (!is.data.frame(auth_df) || nrow(auth_df) == 0) return(FALSE)
      any(auth_df$id %in% other_group_ids)
    }))
  
  if (nrow(matches) > 0) {
    matches <- matches %>%
      mutate(csv_collaborators = map_chr(authorships, function(auth_df) {
        paste(unique(auth_df$display_name[auth_df$id %in% other_group_ids]), collapse = "; ")
      }))
    all_collabs[[focal_name]] <- matches
    message("  --> Found ", nrow(matches), " group co-publications.")
  }
}

# ---- 7b. Save ALL works --------------------------------------------------------
if (length(all_works) > 0) {
  saveRDS(all_works, "all_works_by_author.rds")   # complete, with nested columns
  all_works_export <- map_dfr(all_works, flatten_for_export)
  write_xlsx(all_works_export, "all_works_by_author.xlsx")
  message("\nAll works saved: 'all_works_by_author.rds' and 'all_works_by_author.xlsx' (",
          nrow(all_works_export), " rows)")
}

# ---- 7c. Co-publication report ---------------------------------------------------
core_cols <- c("id", "display_name", "publication_year", "publication_date", "doi", "type",
               "cited_by_count", "source_display_name", "authorships",
               "focal_author", "csv_collaborators")

if (length(all_collabs) > 0) {
  final_report <- map_dfr(all_collabs, ~ select(.x, any_of(core_cols))) %>%
    distinct(id, .keep_all = TRUE)
  
  final_report %>%
    select(`Focal Author` = focal_author, `Collaborator(s)` = csv_collaborators,
           Title = display_name, Year = publication_year, DOI = doi) %>%
    kable(caption = "Internal Collaborations Identified") %>%
    print()
  
  saveRDS(all_collabs, "found_collaborations_full.rds")
  write_xlsx(map_dfr(all_collabs, flatten_for_export), "found_collaborations_summary.xlsx")
  write_xlsx(
    final_report %>%
      select(`Focal Author` = focal_author, `Collaborator(s)` = csv_collaborators,
             Title = display_name, Year = publication_year, DOI = doi),
    "found_collaborations_short_summary.xlsx"
  )
  message("Results saved to 'found_collaborations_summary.xlsx' and 'found_collaborations_short_summary.xlsx'")
} else {
  message("\nNo internal collaborations were detected among the identified authors.")
}

# ---- 8. Count collaborations per author -----------------------------------------
if (exists("final_report") && nrow(final_report) > 0) {
  message("\n>>> COUNTING COLLABORATIONS PER AUTHOR...")
  group_id_map <- setNames(names(found_list), map_chr(found_list, ~ .x$id))
  
  all_ids_flat <- final_report$authorships %>%
    map(function(df) {
      if (!is.data.frame(df) || nrow(df) == 0) return(character(0))
      df$id[df$id %in% names(group_id_map)]
    }) %>%
    unlist()
  
  if (length(all_ids_flat) > 0) {
    author_counts_df <- tibble(ID = all_ids_flat) %>%
      count(ID, name = "Collaborations") %>%
      mutate(Name = group_id_map[ID]) %>%
      select(Name, Collaborations) %>%
      arrange(desc(Collaborations))
    print(author_counts_df)
    write_csv(author_counts_df, "author_collaboration_counts.csv")
    message("Results saved to 'author_collaboration_counts.csv'")
  } else {
    message("No group authors found in the final report authorships (unexpected).")
  }
} else {
  message("No final_report found or it is empty. Cannot count collaborations.")
}

# ---- 9. Verify authors against the live API --------------------------------------
found_df <- bind_rows(found_list) %>% select(display_name, id, works_count, csv_institution)
key_param <- if (api_key != "") paste0("?api_key=", api_key) else ""

verification_results <- found_df %>%
  mutate(
    a_number = toupper(str_extract(id, regex("A\\d+", ignore_case = TRUE))),
    api_url  = paste0("https://api.openalex.org/authors/", a_number, key_param)
  ) %>%
  rowwise() %>%
  mutate(verification = tryCatch({
    response <- httr::GET(api_url)
    if (httr::status_code(response) == 200) {
      author_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      api_name <- (author_data$display_name %||% NA)[1]
      api_wc   <- (author_data$works_count  %||% NA)[1]
      api_inst <- tryCatch({
        inst_val <- author_data$last_known_institutions$display_name
        if (is.null(inst_val) || length(inst_val) == 0) "Not listed"
        else paste(inst_val, collapse = "; ")
      }, error = function(e) "Not listed")
      
      name_flag <- if (!is.na(api_name) && tolower(api_name) != tolower(display_name)) " ⚠️ NAME DIFFERS" else ""
      wc_flag   <- if (!is.na(api_wc) && api_wc != works_count)
        paste0(" ⚠️ WORKS_COUNT DIFFERS (CSV: ", works_count, ")") else ""
      
      paste0("✓ VERIFIED | Name: ", api_name, " | Works: ", api_wc,
             " | Last Known Affiliation: ", api_inst, name_flag, wc_flag)
    } else {
      paste0("✗ NOT FOUND (HTTP ", httr::status_code(response), ")")
    }
  }, error = function(e) paste0("✗ ERROR: ", conditionMessage(e)))) %>%
  ungroup() %>%
  select(display_name, a_number, csv_institution, verification)

print(kable(verification_results, format = "markdown"))

cat("\n\n=== VERIFICATION SUMMARY ===\n")
cat("Total authors checked:", nrow(verification_results), "\n")
cat("Verified:", sum(str_detect(verification_results$verification, "^✓")), "\n")
cat("Not found:", sum(str_detect(verification_results$verification, "^✗ NOT FOUND")), "\n")
cat("Errors:", sum(str_detect(verification_results$verification, "^✗ ERROR")), "\n")
cat("Name mismatches:", sum(str_detect(verification_results$verification, "NAME DIFFERS")), "\n")
cat("Works_count mismatches:", sum(str_detect(verification_results$verification, "WORKS_COUNT DIFFERS")), "\n")

write_csv(verification_results, "author_verification_full_report.csv")

flagged <- verification_results %>% filter(str_detect(verification, "⚠️|✗"))
if (nrow(flagged) > 0) {
  cat("\n\n=== ⚠️ ROWS NEEDING MANUAL REVIEW ===\n")
  print(kable(flagged, format = "markdown"))
  write_csv(flagged, "author_verification_flagged.csv")
} else {
  cat("\n✅ All authors verified cleanly — no mismatches detected.\n")
}

all_works_export %>% count(publication_year)
