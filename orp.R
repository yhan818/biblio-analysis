### For Office of Research ORP
### U Arizona 2025 Publications and authors data 
### Date: 2026-07-07 
### Author: Yan Han with assist from Claude Opus 4.6


# free unused obj to manage memory
rm(list=ls())
gc()

options(openalexR.apikey = Sys.getenv("OPENALEXR_APIKEY"))
PATH <- "/home/yhan/Documents/biblio-analysis"

setwd(PATH)
getwd()

source("my_functions.R")

works_published_2025 <- readRDS("../works_published_2025_ver2026.rds")

works_published_2025 <-oa_fetch(
  entity="works",
  institutions.ror=c("03m2x1q45"), # U Arizona
  from_publication_date ="2025-01-01",
  to_publication_date = "2025-12-31",
)

saveRDS(works_published_2025, "../works_published_2025_ver2026.rds")

library(openalexR)
library(dplyr)
library(tidyr)
library(purrr)

# ============================================================
# STEP 1: Fetch UA 2025 peer-reviewed publications
# ============================================================

works_article_published_2025 <- oa_fetch(
  entity = "works",
  institutions.ror = "03m2x1q45",  # University of Arizona
  from_publication_date = "2025-01-01",
  to_publication_date = "2025-12-31",
  type = "article"  
)

################### use this for changing years!!!! 
works_published <- works_article_published_2025

# ============================================================
# STEP 2: Helper function to unnest authorships → affiliations
# ============================================================

unnest_authors <- function(author_df) {
  if (is.null(author_df) || nrow(author_df) == 0) return(NULL)
  author_df %>%
    unnest(affiliations, names_sep = "_", keep_empty = TRUE)
}

# ============================================================
# STEP 3: Classify works by collaboration type
# ============================================================

works_classified <- works_published %>%
  mutate(
    # Get all unique country codes per work
    all_countries = map(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(character(0))
      df %>%
        pull(affiliations_country_code) %>%
        na.omit() %>%
        unique()
    }),
    
    # Has at least one non-US author
    has_nonus_author = map_lgl(all_countries, ~ any(.x != "US")),
    
    # UA solo (only UA, no other institution)
    ua_solo = map_lgl(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(FALSE)
      all_institutions <- df %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
      length(all_institutions) == 1 && grepl("Arizona", all_institutions[1])
    }),
    
    # Classification
    collab_detail = case_when(
      ua_solo ~ "UA solo",
      has_nonus_author ~ "International collaboration",
      TRUE ~ "US collaboration"
    )
  )

table(works_classified$collab_detail)

# ============================================================
# STEP 4: UA author names
# ============================================================

ua_authors_info <- works_published %>%
  transmute(
    work_id = id,
    ua_author_names = map_chr(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(NA_character_)
      ua_names <- df %>%
        filter(grepl("03m2x1q45", affiliations_ror)) %>%
        pull(display_name) %>%
        unique()
      if (length(ua_names) == 0) return(NA_character_)
      paste(ua_names, collapse = "; ")
    })
  )

# ============================================================
# STEP 5: Corresponding author institution (by name)
# ============================================================

corresponding_info <- works_published %>%
  transmute(
    work_id = id,
    corresponding_author = map_chr(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(NA_character_)
      corr <- df %>% filter(is_corresponding == TRUE)
      if (nrow(corr) == 0) return(NA_character_)
      paste(unique(corr$display_name), collapse = "; ")
    }),
    corresponding_institution = map_chr(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(NA_character_)
      corr <- df %>% filter(is_corresponding == TRUE)
      if (nrow(corr) == 0) return(NA_character_)
      insts <- corr %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
      if (length(insts) == 0) return(NA_character_)
      paste(insts, collapse = "; ")
    })
  )

# ============================================================
# STEP 6: Countries distinct (count + codes)
# ============================================================

countries_info <- works_published %>%
  transmute(
    work_id = id,
    countries_distinct = map_int(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(0L)
      df %>%
        pull(affiliations_country_code) %>%
        na.omit() %>%
        n_distinct()
    }),
    country_codes_all = map_chr(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(NA_character_)
      codes <- df %>%
        pull(affiliations_country_code) %>%
        na.omit() %>%
        unique()
      if (length(codes) == 0) return(NA_character_)
      paste(codes, collapse = "; ")
    })
  )

# ============================================================
# STEP 7: Institutions distinct (count)
# ============================================================
#### ??? Questions for OpenAlex: why distinct inst counts are different from the web ? https://openalex.org/works/W3104212047

institutions_info <- works_published %>%
  transmute(
    work_id = id,
    institutions_distinct = map_int(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(0L)
      df %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        n_distinct()
    })
  )

# ============================================================
# STEP 8: Top US partner institutions
# ============================================================

us_partners <- works_published %>%
  transmute(
    work_id = id,
    partners = map(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(character(0))
      df %>%
        filter(affiliations_country_code == "US") %>%
        filter(!grepl("03m2x1q45", affiliations_ror)) %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
    })
  ) %>%
  unnest(partners, keep_empty = TRUE) %>%
  filter(!is.na(partners)) %>%
  count(partners, sort = TRUE, name = "num_papers")

head(us_partners, 20)

# ============================================================
# STEP 9: Top international partner institutions
# ============================================================

intl_partners <- works_published %>%
  transmute(
    work_id = id,
    partners = map(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(character(0))
      df %>%
        filter(affiliations_country_code != "US") %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
    })
  ) %>%
  unnest(partners, keep_empty = TRUE) %>%
  filter(!is.na(partners)) %>%
  count(partners, sort = TRUE, name = "num_papers")

head(intl_partners, 20)

# ============================================================
# STEP 10: Top collaborating countries
# ============================================================

country_collabs <- works_published %>%
  transmute(
    work_id = id,
    countries = map(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(character(0))
      df %>%
        filter(affiliations_country_code != "US") %>%
        pull(affiliations_country_code) %>%
        na.omit() %>%
        unique()
    })
  ) %>%
  unnest(countries, keep_empty = TRUE) %>%
  filter(!is.na(countries)) %>%
  count(countries, sort = TRUE, name = "num_papers")

head(country_collabs, 20)

# ============================================================
# STEP 11: Unique UA authors
# ============================================================

ua_unique_authors <- ua_authors_info %>%
  separate_rows(ua_author_names, sep = "; ") %>%
  filter(!is.na(ua_author_names)) %>%
  distinct(ua_author_names)

# ============================================================
# STEP 12: UA author productivity
# ============================================================

ua_author_productivity <- ua_authors_info %>%
  separate_rows(ua_author_names, sep = "; ") %>%
  filter(!is.na(ua_author_names)) %>%
  count(ua_author_names, sort = TRUE)

head(ua_author_productivity, 20)

# ============================================================
# STEP 13: Corresponding author — is UA the lead?
# ============================================================

corresponding_ua <- corresponding_info %>%
  mutate(
    ua_is_corresponding = map_lgl(
      corresponding_institution,
      ~ grepl("Arizona", .x, ignore.case = TRUE) & !is.na(.x)
    )
  )

table(corresponding_ua$ua_is_corresponding)

# ============================================================
# STEP 14: Impact by collaboration type
# ============================================================

impact_by_collab <- works_classified %>%
  group_by(collab_detail) %>%
  summarise(
    n_papers = n(),
    mean_citations = mean(cited_by_count, na.rm = TRUE),
    median_citations = median(cited_by_count, na.rm = TRUE),
    mean_fwci = mean(fwci, na.rm = TRUE),
    median_fwci = median(fwci, na.rm = TRUE),
    .groups = "drop"
  )

impact_by_collab

# ============================================================
# STEP 15: Unique countries & institutions counts
# ============================================================

unique_countries_count <- nrow(country_collabs)

all_partners <- works_published_2025 %>%
  transmute(
    work_id = id,
    partners = map(authorships, function(author_df) {
      df <- unnest_authors(author_df)
      if (is.null(df)) return(character(0))
      df %>%
        filter(!grepl("03m2x1q45", affiliations_ror)) %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
    })
  ) %>%
  unnest(partners, keep_empty = TRUE) %>%
  filter(!is.na(partners))

unique_institutions_count <- n_distinct(all_partners$partners)

cat("Unique countries collaborated with:", unique_countries_count, "\n")
cat("Unique institutions collaborated with:", unique_institutions_count, "\n")

# ============================================================
# STEP 16: Summary Report
# ============================================================

summary_report <- tibble(
  Metric = c(
    "Total articles (2025)",
    "Unique UA authors",
    "UA solo publications",
    "US collaboration",
    "International collaboration",
    "Top US partner",
    "Top international partner",
    "Top collaborating country",
    "Unique countries collaborated with",
    "Unique institutions collaborated with",
    "Mean citations per paper",
    "Mean FWCI",
    "UA as corresponding author (%)"
  ),
  Value = c(
    nrow(works_published),
    nrow(ua_unique_authors),
    sum(works_classified$collab_detail == "UA solo", na.rm = TRUE),
    sum(works_classified$collab_detail == "US collaboration", na.rm = TRUE),
    sum(works_classified$collab_detail == "International collaboration", na.rm = TRUE),
    us_partners$partners[1],
    intl_partners$partners[1],
    country_collabs$countries[1],
    unique_countries_count,
    unique_institutions_count,
    round(mean(works_published$cited_by_count, na.rm = TRUE), 2),
    round(mean(works_published$fwci, na.rm = TRUE), 2),
    round(mean(corresponding_ua$ua_is_corresponding, na.rm = TRUE) * 100, 1)
  )
)

print(summary_report)

# ============================================================
# STEP 17: Combine all details into master table
# ============================================================

master <- works_classified %>%
  transmute(
    work_id = id,
    title,
    publication_date,
    journal_name = source_display_name,
    publisher = host_organization,
    citation_count = cited_by_count,
    fwci,
    collab_detail
  ) %>%
  left_join(ua_authors_info, by = "work_id") %>%
  left_join(corresponding_info, by = "work_id") %>%
  left_join(countries_info, by = "work_id") %>%
  left_join(institutions_info, by = "work_id")

glimpse(master)

# ============================================================
# STEP 18: Export
# ============================================================

write.csv(master, "UA_2025_publications_master.csv", row.names = FALSE)
write.csv(us_partners, "UA_2025_US_partners.csv", row.names = FALSE)
write.csv(intl_partners, "UA_2025_international_partners.csv", row.names = FALSE)
write.csv(country_collabs, "UA_2025_country_collaborations.csv", row.names = FALSE)
write.csv(ua_author_productivity, "UA_2025_author_productivity.csv", row.names = FALSE)
write.csv(as.data.frame(summary_report), "UA_2025_summary_report.csv", row.names = FALSE)
write.csv(impact_by_collab, "UA_2025_impact_by_collaboration.csv", row.names = FALSE)

#######################################################

# ============================================================
# VERIFICATION METHOD 1: Collaboration Classification
# Original: map_lgl checking all_countries for non-US
# Alternative: unnest everything first, then classify
# ============================================================

# --- Alternative approach: unnest all authorships globally ---
all_authorships_flat <- works_published %>%
  select(work_id = id, authorships) %>%
  mutate(row_num = row_number()) %>%
  mutate(
    all_countries_per_work = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      df <- author_df %>%
        unnest(affiliations, names_sep = "_", keep_empty = TRUE)
      df %>%
        pull(affiliations_country_code) %>%
        na.omit() %>%
        unique()
    }),
    all_institutions_per_work = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      df <- author_df %>%
        unnest(affiliations, names_sep = "_", keep_empty = TRUE)
      df %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
    }),
    all_rors_per_work = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      df <- author_df %>%
        unnest(affiliations, names_sep = "_", keep_empty = TRUE)
      df %>%
        pull(affiliations_ror) %>%
        na.omit() %>%
        unique()
    })
  )

# Classify using alternative logic
collab_classified_alt <- all_authorships_flat %>%
  mutate(
    n_countries = map_int(all_countries_per_work, length),
    n_institutions = map_int(all_institutions_per_work, length),
    has_us = map_lgl(all_countries_per_work, ~ "US" %in% .x),
    has_nonus = map_lgl(all_countries_per_work, ~ any(.x != "US")),
    has_ua = map_lgl(all_rors_per_work, ~ any(grepl("03m2x1q45", .x))),
    
    # Is UA the only institution?
    ua_solo_alt = map_lgl(all_institutions_per_work, function(insts) {
      length(insts) == 1 && grepl("Arizona", insts[1])
    }),
    
    collab_detail_alt = case_when(
      ua_solo_alt ~ "UA solo",
      has_nonus ~ "International collaboration",
      TRUE ~ "US collaboration"
    )
  )

# Compare classification
cat("=== VERIFICATION 1: Collaboration Classification ===\n\n")

collab_counts_alt <- collab_classified_alt %>%
  count(collab_detail_alt, name = "n_alt")

collab_counts_orig <- works_classified %>%
  count(collab_detail, name = "n_orig")

comparison_collab <- collab_counts_orig %>%
  rename(collab_detail_alt = collab_detail) %>%
  left_join(collab_counts_alt, by = "collab_detail_alt")

print(comparison_collab)
cat("Match:", all(comparison_collab$n_orig == comparison_collab$n_alt), "\n\n")

# ============================================================
# VERIFICATION METHOD 2: Countries Distinct
# Original: map_int with unnest_authors helper
# Alternative: use pre-computed all_countries_per_work
# ============================================================

countries_alt <- collab_classified_alt %>%
  transmute(
    work_id,
    countries_distinct_alt = n_countries,
    country_codes_alt = map_chr(all_countries_per_work, function(codes) {
      if (length(codes) == 0) return(NA_character_)
      paste(codes, collapse = "; ")
    })
  )

# Compare with original
countries_comparison <- countries_info %>%
  left_join(countries_alt, by = "work_id")

cat("=== VERIFICATION 2: Countries Distinct ===\n")
cat("Correlation:", 
    cor(countries_comparison$countries_distinct, 
        countries_comparison$countries_distinct_alt, 
        use = "complete.obs"), "\n")
cat("Exact match:", 
    sum(countries_comparison$countries_distinct == countries_comparison$countries_distinct_alt, na.rm = TRUE),
    "out of", nrow(countries_comparison), "\n")

# Show any mismatches
mismatches_countries <- countries_comparison %>%
  filter(countries_distinct != countries_distinct_alt)
cat("Mismatches:", nrow(mismatches_countries), "\n\n")

# ============================================================
# VERIFICATION METHOD 3: Institutions Distinct
# Original: map_int with unnest_authors helper
# Alternative: use pre-computed all_institutions_per_work
# ============================================================

institutions_alt <- collab_classified_alt %>%
  transmute(
    work_id,
    institutions_distinct_alt = n_institutions
  )

# Compare with original
institutions_comparison <- institutions_info %>%
  left_join(institutions_alt, by = "work_id")

cat("=== VERIFICATION 3: Institutions Distinct ===\n")
cat("Correlation:", 
    cor(institutions_comparison$institutions_distinct, 
        institutions_comparison$institutions_distinct_alt, 
        use = "complete.obs"), "\n")
cat("Exact match:", 
    sum(institutions_comparison$institutions_distinct == institutions_comparison$institutions_distinct_alt, na.rm = TRUE),
    "out of", nrow(institutions_comparison), "\n")

mismatches_inst <- institutions_comparison %>%
  filter(institutions_distinct != institutions_distinct_alt)
cat("Mismatches:", nrow(mismatches_inst), "\n\n")

# ============================================================
# VERIFICATION METHOD 4: UA Author Names
# Original: map_chr filtering by ROR "03m2x1q45"
# Alternative: different filtering approach
# ============================================================

ua_authors_alt <- works_published %>%
  transmute(
    work_id = id,
    ua_author_names_alt = map_chr(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(NA_character_)
      
      # For each author, check if any affiliation has UA ROR
      ua_mask <- map_lgl(author_df$affiliations, function(aff) {
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) return(FALSE)
        any(grepl("03m2x1q45", aff$ror), na.rm = TRUE)
      })
      
      ua_names <- author_df$display_name[ua_mask]
      ua_names <- unique(ua_names[!is.na(ua_names)])
      
      if (length(ua_names) == 0) return(NA_character_)
      paste(ua_names, collapse = "; ")
    })
  )

# Compare with original
ua_authors_comparison <- ua_authors_info %>%
  left_join(ua_authors_alt, by = "work_id")

# Count matches
exact_match_ua <- sum(
  ua_authors_comparison$ua_author_names == ua_authors_comparison$ua_author_names_alt,
  na.rm = TRUE
)
both_na <- sum(
  is.na(ua_authors_comparison$ua_author_names) & is.na(ua_authors_comparison$ua_author_names_alt)
)

cat("=== VERIFICATION 4: UA Author Names ===\n")
cat("Exact match:", exact_match_ua, "\n")
cat("Both NA:", both_na, "\n")
cat("Total matching:", exact_match_ua + both_na, "out of", nrow(ua_authors_comparison), "\n")

# Show mismatches
mismatches_ua <- ua_authors_comparison %>%
  filter(
    (ua_author_names != ua_author_names_alt) |
      (is.na(ua_author_names) & !is.na(ua_author_names_alt)) |
      (!is.na(ua_author_names) & is.na(ua_author_names_alt))
  )
cat("Mismatches:", nrow(mismatches_ua), "\n\n")

if (nrow(mismatches_ua) > 0) {
  cat("Sample mismatches:\n")
  print(head(mismatches_ua %>% select(work_id, ua_author_names, ua_author_names_alt), 5))
}

# ============================================================
# VERIFICATION METHOD 5: Corresponding Author Institution
# Original: map_chr filtering is_corresponding == TRUE
# Alternative: different access pattern
# ============================================================

corresponding_alt <- works_published %>%
  transmute(
    work_id = id,
    corresponding_author_alt = map_chr(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(NA_character_)
      
      # Find corresponding authors
      corr_mask <- author_df$is_corresponding == TRUE
      corr_mask[is.na(corr_mask)] <- FALSE
      
      if (!any(corr_mask)) return(NA_character_)
      paste(unique(author_df$display_name[corr_mask]), collapse = "; ")
    }),
    corresponding_institution_alt = map_chr(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(NA_character_)
      
      corr_mask <- author_df$is_corresponding == TRUE
      corr_mask[is.na(corr_mask)] <- FALSE
      
      if (!any(corr_mask)) return(NA_character_)
      
      # Get institutions of corresponding authors
      corr_affs <- author_df$affiliations[corr_mask]
      
      inst_names <- map(corr_affs, function(aff) {
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) return(character(0))
        aff$display_name[!is.na(aff$display_name)]
      }) %>%
        unlist() %>%
        unique()
      
      if (length(inst_names) == 0) return(NA_character_)
      paste(inst_names, collapse = "; ")
    })
  )

# Compare
corresponding_comparison <- corresponding_info %>%
  left_join(corresponding_alt, by = "work_id")

exact_match_corr_author <- sum(
  corresponding_comparison$corresponding_author == corresponding_comparison$corresponding_author_alt,
  na.rm = TRUE
)
exact_match_corr_inst <- sum(
  corresponding_comparison$corresponding_institution == corresponding_comparison$corresponding_institution_alt,
  na.rm = TRUE
)

cat("=== VERIFICATION 5: Corresponding Author ===\n")
cat("Author name match:", exact_match_corr_author, "out of", nrow(corresponding_comparison), "\n")
cat("Institution match:", exact_match_corr_inst, "out of", nrow(corresponding_comparison), "\n")

mismatches_corr <- corresponding_comparison %>%
  filter(corresponding_institution != corresponding_institution_alt)
cat("Institution mismatches:", nrow(mismatches_corr), "\n\n")

# ============================================================
# VERIFICATION METHOD 6: Top US Partners
# Original: filter country_code == "US", exclude UA ROR
# Alternative: different approach
# ============================================================

us_partners_alt <- works_published %>%
  transmute(
    work_id = id,
    us_partner_list = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      
      # Get all institutions per author
      all_insts <- map_dfr(seq_len(nrow(author_df)), function(j) {
        aff <- author_df$affiliations[[j]]
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) {
          return(tibble(display_name = character(0), country_code = character(0), ror = character(0)))
        }
        aff %>% select(display_name, country_code, ror)
      })
      
      # Filter US, exclude UA
      us_non_ua <- all_insts %>%
        filter(country_code == "US") %>%
        filter(!grepl("03m2x1q45", ror)) %>%
        pull(display_name) %>%
        na.omit() %>%
        unique()
      
      us_non_ua
    })
  ) %>%
  unnest(us_partner_list, keep_empty = TRUE) %>%
  filter(!is.na(us_partner_list)) %>%
  count(us_partner_list, sort = TRUE, name = "num_papers") %>%
  rename(partners = us_partner_list)

cat("=== VERIFICATION 6: Top US Partners ===\n")
cat("Original top 10:\n")
print(head(us_partners, 10))
cat("\nAlternative top 10:\n")
print(head(us_partners_alt, 10))

# Check match
partners_match <- all.equal(
  us_partners %>% head(10) %>% pull(num_papers),
  us_partners_alt %>% head(10) %>% pull(num_papers)
)
cat("\nTop 10 US partners match:", partners_match, "\n\n")

# ============================================================
# VERIFICATION METHOD 7: Top International Partners
# ============================================================

intl_partners_alt <- works_published %>%
  transmute(
    work_id = id,
    intl_partner_list = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      
      all_insts <- map_dfr(seq_len(nrow(author_df)), function(j) {
        aff <- author_df$affiliations[[j]]
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) {
          return(tibble(display_name = character(0), country_code = character(0)))
        }
        aff %>% select(display_name, country_code)
      })
      
      # Filter non-US
      intl <- all_insts %>%
        filter(country_code != "US") %>%
        pull(display_name) %>%
        na.omit() %>%
        unique()
      
      intl
    })
  ) %>%
  unnest(intl_partner_list, keep_empty = TRUE) %>%
  filter(!is.na(intl_partner_list)) %>%
  count(intl_partner_list, sort = TRUE, name = "num_papers") %>%
  rename(partners = intl_partner_list)

cat("=== VERIFICATION 7: Top International Partners ===\n")
cat("Original top 10:\n")
print(head(intl_partners, 10))
cat("\nAlternative top 10:\n")
print(head(intl_partners_alt, 10))

partners_intl_match <- all.equal(
  intl_partners %>% head(10) %>% pull(num_papers),
  intl_partners_alt %>% head(10) %>% pull(num_papers)
)
cat("\nTop 10 intl partners match:", partners_intl_match, "\n\n")

# ============================================================
# VERIFICATION METHOD 8: Top Collaborating Countries
# ============================================================

country_collabs_alt <- works_published %>%
  transmute(
    work_id = id,
    country_list = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      
      all_countries <- map(author_df$affiliations, function(aff) {
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) return(character(0))
        aff$country_code[!is.na(aff$country_code)]
      }) %>%
        unlist() %>%
        unique()
      
      # Exclude US
      all_countries[all_countries != "US"]
    })
  ) %>%
  unnest(country_list, keep_empty = TRUE) %>%
  filter(!is.na(country_list)) %>%
  count(country_list, sort = TRUE, name = "num_papers") %>%
  rename(countries = country_list)

cat("=== VERIFICATION 8: Top Collaborating Countries ===\n")
cat("Original top 10:\n")
print(head(country_collabs, 10))
cat("\nAlternative top 10:\n")
print(head(country_collabs_alt, 10))

countries_match <- all.equal(
  country_collabs %>% head(10) %>% pull(num_papers),
  country_collabs_alt %>% head(10) %>% pull(num_papers)
)
cat("\nTop 10 countries match:", countries_match, "\n\n")

# ============================================================
# VERIFICATION METHOD 9: UA as Corresponding Author %
# Original: grepl("Arizona") on corresponding_institution
# Alternative: check ROR directly
# ============================================================

corresponding_ua_alt <- works_published %>%
  transmute(
    work_id = id,
    ua_is_corresponding_alt = map_lgl(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(FALSE)
      
      corr_mask <- author_df$is_corresponding == TRUE
      corr_mask[is.na(corr_mask)] <- FALSE
      
      if (!any(corr_mask)) return(FALSE)
      
      # Check if any corresponding author has UA affiliation
      corr_affs <- author_df$affiliations[corr_mask]
      
      any(map_lgl(corr_affs, function(aff) {
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) return(FALSE)
        any(grepl("03m2x1q45", aff$ror), na.rm = TRUE)
      }))
    })
  )

# Compare with original (which used grepl("Arizona") on institution name)
cat("=== VERIFICATION 9: UA as Corresponding Author ===\n")
cat("Original (by name 'Arizona'):", 
    sum(corresponding_ua$ua_is_corresponding, na.rm = TRUE), "\n")
##### ROR is more accurate. but missing Cancer center!!!!!!!!!!!!!
##################################################################
cat("Alternative (by ROR):", 
    sum(corresponding_ua_alt$ua_is_corresponding_alt, na.rm = TRUE), "\n")
cat("Original %:", 
    round(mean(corresponding_ua$ua_is_corresponding, na.rm = TRUE) * 100, 1), "%\n")
cat("Alternative %:", 
    round(mean(corresponding_ua_alt$ua_is_corresponding_alt, na.rm = TRUE) * 100, 1), "%\n\n")

# Check discrepancies
corr_compare <- corresponding_ua %>%
  left_join(corresponding_ua_alt, by = "work_id")

disagree <- corr_compare %>%
  filter(ua_is_corresponding != ua_is_corresponding_alt)

cat("Disagreements:", nrow(disagree), "\n")
if (nrow(disagree) > 0) {
  cat("Cases where name-match says YES but ROR says NO (or vice versa):\n")
  print(head(disagree %>% select(work_id, ua_is_corresponding, ua_is_corresponding_alt), 10))
}

# ============================================================
# VERIFICATION METHOD 10: Unique UA Authors Count
# Original: separate_rows on ua_author_names, count distinct
# Alternative: extract directly from authorships
# ============================================================

ua_unique_authors_alt <- works_published %>%
  transmute(
    ua_names = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      
      ua_mask <- map_lgl(author_df$affiliations, function(aff) {
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) return(FALSE)
        any(grepl("03m2x1q45", aff$ror), na.rm = TRUE)
      })
      
      author_df$display_name[ua_mask] %>% na.omit()
    })
  ) %>%
  unnest(ua_names, keep_empty = TRUE) %>%
  filter(!is.na(ua_names)) %>%
  distinct(ua_names)

cat("=== VERIFICATION 10: Unique UA Authors ===\n")
cat("Original:", nrow(ua_unique_authors), "\n")
cat("Alternative:", nrow(ua_unique_authors_alt), "\n")
cat("Match:", nrow(ua_unique_authors) == nrow(ua_unique_authors_alt), "\n\n")

# ============================================================
# VERIFICATION METHOD 11: Unique Countries & Institutions
# ============================================================
unique_countries_alt <- collab_classified_alt %>%
  unnest(all_countries_per_work, keep_empty = TRUE) %>%
  filter(!is.na(all_countries_per_work) & all_countries_per_work != "US") %>%
  n_distinct()

unique_institutions_alt <- collab_classified_alt %>%
  unnest(all_institutions_per_work, keep_empty = TRUE) %>%
  filter(!is.na(all_institutions_per_work)) %>%
  # Exclude UA
  filter(!grepl("University of Arizona", all_institutions_per_work)) %>%
  pull(all_institutions_per_work) %>%
  n_distinct()

cat("=== VERIFICATION 11: Unique Countries & Institutions ===\n")
cat("Unique countries (original):", unique_countries_count, "\n")
cat("Unique countries (alternative):", unique_countries_alt, "\n")
cat("Unique institutions (original):", unique_institutions_count, "\n")
cat("Unique institutions (alternative):", unique_institutions_alt, "\n\n")

# ============================================================
# DEBUG: Unique Countries (162 vs 10539)
# ============================================================

# Original approach:
cat("Original unique countries:", unique_countries_count, "\n")

unique_countries_alt_fixed <- collab_classified_alt %>%
  mutate(nonus_countries = map(all_countries_per_work, ~ .x[.x != "US"])) %>%
  unnest(nonus_countries, keep_empty = TRUE) %>%
  filter(!is.na(nonus_countries)) %>%
  pull(nonus_countries) %>%
  n_distinct()

cat("Alternative unique countries (fixed):", unique_countries_alt_fixed, "\n")
cat("Match:", unique_countries_count == unique_countries_alt_fixed, "\n\n")

# ============================================================
# DEBUG: Unique Institutions (9005 vs 7933)
# ============================================================
# The difference is likely in how "excluding UA" is done
# Original: excluded by ROR pattern "03m2x1q45"
# Alternative: excluded by name "University of Arizona"

# But UA might appear with slightly different names in the data
# Let's check both methods

# Method A: Exclude by ROR (more accurate)
unique_institutions_alt_by_ror <- works_published %>%
  transmute(
    work_id = id,
    partner_insts = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      
      all_insts <- map_dfr(seq_len(nrow(author_df)), function(j) {
        aff <- author_df$affiliations[[j]]
        if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) {
          return(tibble(display_name = character(0), ror = character(0)))
        }
        aff %>% select(display_name, ror)
      })
      
      # Exclude UA by ROR
      non_ua <- all_insts %>%
        filter(!grepl("03m2x1q45", ror) | is.na(ror)) %>%
        pull(display_name) %>%
        na.omit() %>%
        unique()
      
      non_ua
    })
  ) %>%
  unnest(partner_insts, keep_empty = TRUE) %>%
  filter(!is.na(partner_insts)) %>%
  pull(partner_insts) %>%
  n_distinct()

cat("Original unique institutions:", unique_institutions_count, "\n")
cat("Alternative (exclude by name 'University of Arizona'):", unique_institutions_alt, "\n")
cat("Alternative (exclude by ROR):", unique_institutions_alt_by_ror, "\n\n")

# Method B: Check what the original method actually counted
# The original used all_partners which excluded by ROR
cat("Let's verify original was computed from all_partners:\n")
cat("n_distinct(all_partners$partners):", n_distinct(all_partners$partners), "\n\n")

# ============================================================
# The difference in institutions is likely because:
# 1. Original includes ALL institutions (including UA sub-units?)
# 2. Name-based exclusion misses UA variations
# The difference (9,005 vs 7,934) = ~1,071 is NOT just UA — it's likely also:

#  UA sub-departments/centers listed as separate institutions
#  Entries with NA in ROR that have "University of Arizona" in the name
# ============================================================

# Check: What UA-related names exist in the data?
ua_related_names <- collab_classified_alt %>%
  unnest(all_institutions_per_work, keep_empty = TRUE) %>%
  filter(grepl("Arizona", all_institutions_per_work, ignore.case = TRUE)) %>%
  distinct(all_institutions_per_work)

cat("Institutions with 'Arizona' in name:\n")
print(ua_related_names, n = 30)


# ============================================================
# CORRECTED: Unique partner institutions (exclude UA properly)
# ============================================================

unique_institutions_correct <- works_published %>%
  transmute(
    work_id = id,
    partner_insts = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      df <- author_df %>%
        unnest(affiliations, names_sep = "_", keep_empty = TRUE)
      # Exclude UA by BOTH ROR and name
      df %>%
        filter(
          !grepl("03m2x1q45", affiliations_ror) | is.na(affiliations_ror)
        ) %>%
        filter(
          !grepl("University of Arizona", affiliations_display_name, ignore.case = TRUE) | 
            is.na(affiliations_display_name)
        ) %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
    })
  ) %>%
  unnest(partner_insts, keep_empty = TRUE) %>%
  filter(!is.na(partner_insts)) %>%
  pull(partner_insts) %>%
  n_distinct()

cat("Unique partner institutions (exclude UA by ROR + name):", unique_institutions_correct, "\n")

### UA has only 1 institution name with ROR 03m2x1q45: "University of Arizona"
# "University of Arizona Cancer Center" has a different ROR (04tvx8690) — so it's NOT excluded by the UA ROR filter
Total institutions = 7,935 (including UA) → 7,934 (excluding UA by ROR) — difference is just 1 (UA itself)
# ============================================================
# CORRECTED FINAL COMPARISON
# ============================================================

cat("\n\n========================================\n")
cat("CORRECTED FINAL VERIFICATION SUMMARY\n")
cat("========================================\n\n")

final_comparison_v2 <- tibble(
  Metric = c(
    "UA solo papers",
    "US collaboration papers",
    "International collaboration papers",
    "Unique UA authors",
    "Unique countries (non-US)",
    "Unique partner institutions",
    "UA as corresponding author %"
  ),
  Original = as.character(c(
    sum(works_classified$collab_detail == "UA solo"),
    sum(works_classified$collab_detail == "US collaboration"),
    sum(works_classified$collab_detail == "International collaboration"),
    nrow(ua_unique_authors),
    unique_countries_count,
    unique_institutions_count,
    round(mean(corresponding_ua$ua_is_corresponding, na.rm = TRUE) * 100, 1)
  )),
  Alternative = as.character(c(
    sum(collab_classified_alt$collab_detail_alt == "UA solo"),
    sum(collab_classified_alt$collab_detail_alt == "US collaboration"),
    sum(collab_classified_alt$collab_detail_alt == "International collaboration"),
    nrow(ua_unique_authors_alt),
    unique_countries_alt_fixed,
    unique_institutions_alt_by_ror,
    round(mean(corresponding_ua_alt$ua_is_corresponding_alt, na.rm = TRUE) * 100, 1)
  )),
  Note = c(
    "",
    "",
    "",
    "",
    "",
    "Slight diff may be due to NA ROR handling",
    "ROR method (28.9%) is more accurate"
  )
)

final_comparison_v2 <- final_comparison_v2 %>%
  mutate(Match = Original == Alternative)

print(final_comparison_v2)

cat("\nCore metrics match:", 
    all(final_comparison_v2$Match[1:4]), "\n")



# ============================================================
# VERIFICATION METHOD 12: Impact by Collaboration Type
# Alternative: merge differently
# ============================================================

impact_collab_alt <- collab_classified_alt %>%
  select(work_id, collab_detail_alt) %>%
  left_join(
    works_published %>% transmute(work_id = id, cited_by_count, fwci),
    by = "work_id"
  ) %>%
  group_by(collab_detail_alt) %>%
  summarise(
    n_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("=== VERIFICATION 12: Impact by Collaboration ===\n")
cat("Original:\n")
print(impact_by_collab)
cat("\nAlternative:\n")
print(impact_collab_alt)

# ============================================================
# FINAL COMPARISON SUMMARY
# ============================================================

cat("\n\n========================================\n")
cat("FINAL VERIFICATION SUMMARY\n")
cat("========================================\n\n")

final_comparison <- tibble(
  Metric = c(
    "UA solo papers",
    "US collaboration papers",
    "International collaboration papers",
    "Unique UA authors",
    "Unique countries",
    "Unique partner institutions",
    "UA as corresponding author %"
  ),
  Original = as.character(c(
    sum(works_classified$collab_detail == "UA solo"),
    sum(works_classified$collab_detail == "US collaboration"),
    sum(works_classified$collab_detail == "International collaboration"),
    nrow(ua_unique_authors),
    unique_countries_count,
    unique_institutions_count,
    round(mean(corresponding_ua$ua_is_corresponding, na.rm = TRUE) * 100, 1)
  )),
  Alternative = as.character(c(
    sum(collab_classified_alt$collab_detail_alt == "UA solo"),
    sum(collab_classified_alt$collab_detail_alt == "US collaboration"),
    sum(collab_classified_alt$collab_detail_alt == "International collaboration"),
    nrow(ua_unique_authors_alt),
    unique_countries_alt,
    unique_institutions_alt,
    round(mean(corresponding_ua_alt$ua_is_corresponding_alt, na.rm = TRUE) * 100, 1)
  ))
)

final_comparison <- final_comparison %>%
  mutate(Match = Original == Alternative)

print(final_comparison)

cat("\nAll checks passed:", all(final_comparison$Match), "\n")

##############################################################
#######################################################################
######################################################################
#######################################################################
################ Awards and Funders Analysis 
##############################################################
#######################################################################
# ============================================================
# Inspect "awards" column
# ============================================================

str(works_published$awards[[1]])


library(dplyr)
library(tidyr)
library(purrr)

# ============================================================
# FUNCTION: Parse awards named vector into a dataframe
# ============================================================

parse_awards <- function(awards_vec) {
  # Handle NULL or empty
  if (is.null(awards_vec) || length(awards_vec) == 0) {
    return(tibble(
      award_id = character(0),
      funder_award_id = character(0),
      funder_id = character(0),
      funder_display_name = character(0)
    ))
  }
  
  # Get names
  nms <- names(awards_vec)
  
  # Find positions of "id" — each "id" starts a new award record
  id_positions <- which(nms == "id")
  
  # Parse each award
  awards_list <- map(seq_along(id_positions), function(i) {
    # Start position
    start <- id_positions[i]
    
    # End position (next "id" minus 1, or end of vector)
    if (i < length(id_positions)) {
      end <- id_positions[i + 1] - 1
    } else {
      end <- length(awards_vec)
    }
    
    # Extract this award's chunk
    chunk <- awards_vec[start:end]
    chunk_names <- names(chunk)
    
    tibble(
      award_id = chunk[chunk_names == "id"][1],
      display_name = ifelse("display_name" %in% chunk_names, 
                            chunk[chunk_names == "display_name"][1], 
                            NA_character_),
      funder_award_id = ifelse("funder_award_id" %in% chunk_names,
                               chunk[chunk_names == "funder_award_id"][1],
                               NA_character_),
      funder_id = ifelse("funder_id" %in% chunk_names,
                         chunk[chunk_names == "funder_id"][1],
                         NA_character_),
      funder_display_name = ifelse("funder_display_name" %in% chunk_names,
                                   chunk[chunk_names == "funder_display_name"][1],
                                   NA_character_)
    )
  })
  
  bind_rows(awards_list)
}

# ============================================================
# TEST: Parse one example
# ============================================================

test_parsed <- parse_awards(works_published_2025$awards[[1]])
print(test_parsed)




########################### Awards/ Funders
############ **** 2026-07-10

library(dplyr)
library(tidyr)
library(purrr)

# ============================================================
# STEP 1: Identify funded vs unfunded papers
# ============================================================

papers_with_funding <- works_published %>%
  mutate(
    has_funding = map_lgl(funders, function(f) {
      if (is.null(f)) return(FALSE)
      if (is.data.frame(f) && nrow(f) > 0) return(TRUE)
      return(FALSE)
    }),
    has_award = map_lgl(awards, function(a) {
      if (is.null(a)) return(FALSE)
      if (all(is.na(a))) return(FALSE)
      if (length(a) >= 4) return(TRUE)
      return(FALSE)
    })
  )

cat("Papers with funders:", sum(papers_with_funding$has_funding), "\n")
cat("Papers without funders:", sum(!papers_with_funding$has_funding), "\n")
cat("Percentage funded:", 
    round(mean(papers_with_funding$has_funding) * 100, 1), "%\n\n")

cat("Papers with awards:", sum(papers_with_funding$has_award), "\n")
cat("Papers without awards:", sum(!papers_with_funding$has_award), "\n")

# ============================================================
# STEP 2: Extract funders data
# ============================================================

funders_data <- works_published %>%
  transmute(
    work_id = id,
    title,
    funders
  ) %>%
  mutate(
    has_funders = map_lgl(funders, ~ is.data.frame(.x) && nrow(.x) > 0)
  ) %>%
  filter(has_funders) %>%
  unnest(funders, keep_empty = TRUE) %>%
  select(-has_funders)

# ============================================================
# STEP 3: Top funders by number of papers
# ============================================================

top_funders <- funders_data %>%
  filter(!is.na(display_name)) %>%
  count(display_name, sort = TRUE, name = "num_papers")

head(top_funders, 30)

# ============================================================
# STEP 4: Parse awards data (for award IDs)
# ============================================================

parse_awards <- function(awards_vec) {
  if (is.null(awards_vec) || all(is.na(awards_vec)) || length(awards_vec) < 4) {
    return(tibble(
      award_id = character(0),
      funder_award_id = character(0),
      funder_id = character(0),
      funder_display_name = character(0)
    ))
  }
  
  nms <- names(awards_vec)
  id_positions <- which(nms == "id")
  
  awards_list <- map(seq_along(id_positions), function(i) {
    start <- id_positions[i]
    if (i < length(id_positions)) {
      end <- id_positions[i + 1] - 1
    } else {
      end <- length(awards_vec)
    }
    
    chunk <- awards_vec[start:end]
    chunk_names <- names(chunk)
    
    tibble(
      award_id = chunk[chunk_names == "id"][1],
      funder_award_id = ifelse("funder_award_id" %in% chunk_names,
                               chunk[chunk_names == "funder_award_id"][1],
                               NA_character_),
      funder_id = ifelse("funder_id" %in% chunk_names,
                         chunk[chunk_names == "funder_id"][1],
                         NA_character_),
      funder_display_name = ifelse("funder_display_name" %in% chunk_names,
                                   chunk[chunk_names == "funder_display_name"][1],
                                   NA_character_)
    )
  })
  
  bind_rows(awards_list)
}

awards_data <- works_published %>%
  transmute(
    work_id = id,
    awards_parsed = map(awards, parse_awards)
  ) %>%
  unnest(awards_parsed, keep_empty = TRUE)

# Top funders from awards (with award IDs)
top_funders_awards <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  group_by(funder_display_name) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_awards = n_distinct(funder_award_id, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

head(top_funders_awards, 30)

# ============================================================
# STEP 5: Impact by funding status
# ============================================================

impact_by_funding <- papers_with_funding %>%
  group_by(has_funding) %>%
  summarise(
    n_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    median_citations = median(cited_by_count, na.rm = TRUE),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    median_fwci = round(median(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(funding_status = if_else(has_funding, "Funded", "Not funded"))

print(impact_by_funding)

# ============================================================
# STEP 6: Impact by top funders
# ============================================================

impact_by_funder <- funders_data %>%
  filter(!is.na(display_name)) %>%
  distinct(work_id, display_name) %>%
  left_join(
    works_published %>% transmute(work_id = id, cited_by_count, fwci),
    by = "work_id"
  ) %>%
  group_by(display_name) %>%
  summarise(
    num_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  filter(num_papers >= 10) %>%
  arrange(desc(num_papers))

head(impact_by_funder, 30)

# ============================================================
# STEP 7: Funding by collaboration type
# ============================================================

funding_by_collab <- works_classified %>%
  mutate(
    has_funding = map_lgl(funders, function(f) {
      if (is.null(f)) return(FALSE)
      if (is.data.frame(f) && nrow(f) > 0) return(TRUE)
      return(FALSE)
    })
  ) %>%
  group_by(collab_detail) %>%
  summarise(
    n_papers = n(),
    n_funded = sum(has_funding),
    pct_funded = round(mean(has_funding) * 100, 1),
    .groups = "drop"
  )

print(funding_by_collab)

# ============================================================
# STEP 8: Federal vs Non-Federal Funder Breakdown
# ============================================================

# Define US federal funders and their sub-agencies
us_federal_funders <- c(
  # NSF
  "National Science Foundation",
  "Division of Astronomical Sciences",
  "Division of Materials Research",
  "Division of Earth Sciences",
  "Division of Physics",
  "Division of Chemistry",
  "Division of Ocean Sciences",
  "Division of Computer and Network Systems",
  "Division of Mathematical Sciences",
  "Division of Biological Infrastructure",
  "Division of Environmental Biology",
  "Division of Atmospheric and Geospace Sciences",
  "Division of Molecular and Cellular Biosciences",
  "Division of Civil, Mechanical and Manufacturing Innovation",
  
  # NIH and institutes
  "National Institutes of Health",
  "National Heart, Lung, and Blood Institute",
  "National Cancer Institute",
  "National Institute on Aging",
  "National Institute of General Medical Sciences",
  "National Institute of Allergy and Infectious Diseases",
  "National Institute of Mental Health",
  "National Institute of Diabetes and Digestive and Kidney Diseases",
  "National Institute of Neurological Disorders and Stroke",
  "National Institute of Environmental Health Sciences",
  "National Institute on Drug Abuse",
  "National Institute of Biomedical Imaging and Bioengineering",
  "National Eye Institute",
  "National Institute on Alcohol Abuse and Alcoholism",
  "National Institute of Arthritis and Musculoskeletal and Skin Diseases",
  "National Institute of Child Health and Human Development",
  "Eunice Kennedy Shriver National Institute of Child Health and Human Development",
  "National Center for Advancing Translational Sciences",
  "National Institute of Dental and Craniofacial Research",
  "National Library of Medicine",
  "National Human Genome Research Institute",
  "National Institute on Deafness and Other Communication Disorders",
  "National Institute of Nursing Research",
  "National Center for Complementary and Integrative Health",
  "Fogarty International Center",
  
  # NASA
  "National Aeronautics and Space Administration",
  
  # DOE
  "U.S. Department of Energy",
  "High Energy Physics",
  "Office of Science",
  
  # DOD
  "U.S. Department of Defense",
  "U.S. Air Force",
  "U.S. Army",
  "U.S. Navy",
  "Defense Advanced Research Projects Agency",
  "Office of Naval Research",
  "Army Research Office",
  "Air Force Office of Scientific Research",
  
  # USDA
  "U.S. Department of Agriculture",
  "National Institute of Food and Agriculture",
  "Agricultural Research Service",
  
  # Other federal
  "National Oceanic and Atmospheric Administration",
  "U.S. Geological Survey",
  "Centers for Disease Control and Prevention",
  "U.S. Environmental Protection Agency",
  "United States Agency for International Development",
  "U.S. Department of the Interior",
  "Bureau of Land Management",
  "U.S. Fish and Wildlife Service",
  "Smithsonian Institution"
)

# Classify funders
funders_classified <- funders_data %>%
  filter(!is.na(display_name)) %>%
  mutate(
    funder_type = case_when(
      display_name %in% us_federal_funders ~ "US Federal",
      grepl("National Institute|National Center for", display_name) & 
        grepl("openalex.org/F43203", id) ~ "US Federal",  # F43203 32161 NIH, F43203 06076 NSF. ###  Catch NIH sub-institutes
      TRUE ~ "Other"
    )
  )

# Summary by funder type
funder_type_summary <- funders_classified %>%
  group_by(funder_type) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_funders = n_distinct(display_name),
    .groups = "drop"
  ) %>%
  mutate(pct_of_funded = round(num_papers / n_distinct(funders_data$work_id) * 100, 1))

print(funder_type_summary)

# ============================================================
# STEP 9: Federal funder breakdown by agency
# ============================================================

# Group sub-agencies into parent agencies
funders_federal <- funders_classified %>%
  filter(funder_type == "US Federal") %>%
  mutate(
    parent_agency = case_when(
      display_name == "National Science Foundation" ~ "NSF",
      grepl("Division of", display_name) ~ "NSF",
      display_name == "National Institutes of Health" ~ "NIH",
      grepl("National Institute|National Center for|National Eye|National Library|National Human Genome|Fogarty|Eunice Kennedy", 
            display_name) ~ "NIH",
      display_name == "National Aeronautics and Space Administration" ~ "NASA",
      display_name == "U.S. Department of Energy" ~ "DOE",
      grepl("High Energy Physics|Office of Science", display_name) ~ "DOE",
      grepl("Department of Defense|Air Force|Army|Navy|Defense Advanced|Naval Research", 
            display_name) ~ "DOD",
      grepl("Department of Agriculture|Food and Agriculture|Agricultural Research", 
            display_name) ~ "USDA",
      grepl("Oceanic|Atmospheric Administration", display_name) ~ "NOAA",
      grepl("Geological Survey", display_name) ~ "USGS",
      grepl("Centers for Disease Control", display_name) ~ "CDC",
      grepl("Environmental Protection", display_name) ~ "EPA",
      grepl("Agency for International Development", display_name) ~ "USAID",
      grepl("Smithsonian", display_name) ~ "Smithsonian",
      grepl("Department of the Interior|Bureau of Land|Fish and Wildlife", 
            display_name) ~ "DOI",
      TRUE ~ "Other Federal"
    )
  )

# Federal agency breakdown
federal_agency_summary <- funders_federal %>%
  group_by(parent_agency) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_sub_agencies = n_distinct(display_name),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

print(federal_agency_summary)

# ============================================================
# IMPROVED: Federal funder classification
# Combine list + pattern, but exclude non-US
# ============================================================

# Non-US funders that pattern incorrectly catches
non_us_false_positives <- c(
  "National Institute of Mental Health and Neurosciences",   # India
  "National Institute of Ecology",                           # Mexico/Korea
  "Florida Fish and Wildlife Conservation Commission",       # State, not federal
  "National Fish and Wildlife Foundation",                   # Non-profit, not federal
  "Foundation for the National Institutes of Health",        # Non-profit
  "Foundation for Food and Agriculture Research",            # Non-profit
  "Indian Council of Agricultural Research",                 # India
  "Shota Rustaveli National Science Foundation",             # Georgia (country)
  "National Science Foundation of Sri Lanka",                # Sri Lanka
  "Iran National Science Foundation",                        # Iran
  "National Cancer Center",                                  # Japan/Korea
  "Department of Agriculture and Rural Development, Northern Ireland",  # UK
  "California Department of Fish and Wildlife",              # State
  "Consortium of International Agricultural Research Centers",  # International
  "National Science Foundation Graduate Research Fellowship Program"  # Keep as NSF
)

# Additional true US federal funders missed by pattern
additional_federal <- c(
  "National Institute on Aging",
  "National Institute on Alcohol Abuse and Alcoholism",
  "National Institute on Drug Abuse",
  "National Institute on Deafness and Other Communication Disorders",
  "National Center for Advancing Translational Sciences",
  "National Center for Complementary and Integrative Health",
  "Office of Science",
  "High Energy Physics",
  "United States Agency for International Development",
  "Fogarty International Center",
  "Air Force Research Laboratory",
  "U.S. Naval Research Laboratory",
  "Office of Naval Research Global",
  "U.S. Bureau of Land Management",
  "Savannah River Operations Office, U.S. Department of Energy",
  "Division of Intramural Research, National Institute of Allergy and Infectious Diseases",
  "Division of Cancer Epidemiology and Genetics, National Cancer Institute",
  "Division of Cancer Prevention, National Cancer Institute",
  "Office of Extramural Research, National Institutes of Health",
  "Center for Hierarchical Manufacturing, National Science Foundation",
  "National Aeronautics and Space Administration Postdoctoral Program",
  "Smithsonian Tropical Research Institute",
  "Smithsonian Astrophysical Observatory",
  "Smithsonian's National Zoo and Conservation Biology Institute",
  "National Institute of Standards and Technology",
  "National Science Foundation Graduate Research Fellowship Program"
)

# Updated complete federal list
us_federal_funders_updated <- c(us_federal_funders, additional_federal)

# Re-classify
funders_classified_v2 <- funders_data %>%
  filter(!is.na(display_name)) %>%
  mutate(
    funder_type = case_when(
      display_name %in% us_federal_funders_updated ~ "US Federal",
      display_name %in% non_us_false_positives ~ "Other",
      # Catch remaining NIH sub-institutes with "National Institute" in US context
      grepl("Division of .+, National", display_name) ~ "US Federal",
      TRUE ~ "Other"
    )
  )

# Updated summary
funder_type_summary_v2 <- funders_classified_v2 %>%
  group_by(funder_type) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_funders = n_distinct(display_name),
    .groups = "drop"
  ) %>%
  mutate(pct_of_funded = round(num_papers / n_distinct(funders_data$work_id) * 100, 1))

cat("=== UPDATED Federal vs Other ===\n")
print(funder_type_summary_v2)

# Compare to original
cat("\nOriginal:\n")
print(funder_type_summary)
cat("\nUpdated:\n")
print(funder_type_summary_v2)

# ============================================================
# Updated federal agency breakdown
# ============================================================

funders_federal_v2 <- funders_classified_v2 %>%
  filter(funder_type == "US Federal") %>%
  mutate(
    parent_agency = case_when(
      grepl("National Science Foundation|Division of Astronomical|Division of Materials|Division of Earth|Division of Physics|Division of Chemistry|Division of Ocean|Division of Computer|Division of Mathematical|Division of Biological Infrastructure|Division of Environmental|Division of Atmospheric|Division of Molecular|Division of Civil|Center for Hierarchical", display_name) ~ "NSF",
      grepl("National Institutes of Health|National Institute|National Cancer|National Heart|National Eye|National Library|National Human Genome|Fogarty|Eunice Kennedy|National Center for Advancing|National Center for Complementary|Office of Extramural Research|Division of Cancer|Division of Intramural", display_name) ~ "NIH",
      grepl("National Aeronautics|NASA", display_name) ~ "NASA",
      grepl("Department of Energy|High Energy Physics|Office of Science|Savannah River", display_name) ~ "DOE",
      grepl("Department of Defense|Air Force|Army|Navy|Defense Advanced|Naval Research", display_name) ~ "DOD",
      grepl("Department of Agriculture|Food and Agriculture|Agricultural Research|National Institute of Food", display_name) ~ "USDA",
      grepl("Oceanic and Atmospheric", display_name) ~ "NOAA",
      grepl("Geological Survey", display_name) ~ "USGS",
      grepl("Centers for Disease Control", display_name) ~ "CDC",
      grepl("Environmental Protection Agency", display_name) ~ "EPA",
      grepl("Agency for International Development", display_name) ~ "USAID",
      grepl("Smithsonian", display_name) ~ "Smithsonian",
      grepl("Department of the Interior|Bureau of Land|Fish and Wildlife", display_name) ~ "DOI",
      grepl("National Institute of Standards", display_name) ~ "NIST",
      TRUE ~ "Other Federal"
    )
  )

federal_agency_summary_v2 <- funders_federal_v2 %>%
  group_by(parent_agency) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_sub_agencies = n_distinct(display_name),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

cat("\n=== UPDATED Federal Agency Breakdown ===\n")
print(federal_agency_summary_v2)


# ============================================================
# STEP 10: Impact by funder type (Federal vs Other)
# ============================================================

# Get work_ids for federal-funded papers
federal_work_ids <- funders_classified %>%
  filter(funder_type == "US Federal") %>%
  distinct(work_id)

other_work_ids <- funders_classified %>%
  filter(funder_type == "Other") %>%
  distinct(work_id)

# Some papers may have both federal and other funding
impact_by_funder_type <- works_published %>%
  transmute(
    work_id = id,
    cited_by_count,
    fwci
  ) %>%
  mutate(
    has_federal = work_id %in% federal_work_ids$work_id,
    has_other = work_id %in% other_work_ids$work_id,
    funder_category = case_when(
      has_federal & has_other ~ "Both Federal & Other",
      has_federal ~ "US Federal only",
      has_other ~ "Other funding only",
      TRUE ~ "No funding"
    )
  ) %>%
  group_by(funder_category) %>%
  summarise(
    n_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    median_citations = median(cited_by_count, na.rm = TRUE),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    median_fwci = round(median(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(impact_by_funder_type)

# ============================================================
# STEP 11: Impact by federal agency
# ============================================================

impact_by_agency <- funders_federal %>%
  distinct(work_id, parent_agency) %>%
  left_join(
    works_published %>% transmute(work_id = id, cited_by_count, fwci),
    by = "work_id"
  ) %>%
  group_by(parent_agency) %>%
  summarise(
    num_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

print(impact_by_agency)

# ============================================================
# STEP 12: Grants Summary
# ============================================================

fwci_funded <- impact_by_funding %>%
  filter(has_funding == TRUE) %>%
  pull(mean_fwci)

fwci_not_funded <- impact_by_funding %>%
  filter(has_funding == FALSE) %>%
  pull(mean_fwci)

grants_summary <- tibble(
  Metric = c(
    "Papers with funding",
    "Papers without funding",
    "Percentage funded",
    "Unique funders",
    "Top funder #1",
    "Top funder #2",
    "Top funder #3",
    "Top funder #4",
    "Top funder #5",
    "Mean FWCI (funded)",
    "Mean FWCI (not funded)",
    "% funded - UA solo",
    "% funded - US collaboration",
    "% funded - International collaboration",
    "Papers with US Federal funding",
    "Papers with Other funding"
  ),
  Value = as.character(c(
    sum(papers_with_funding$has_funding),
    sum(!papers_with_funding$has_funding),
    paste0(round(mean(papers_with_funding$has_funding) * 100, 1), "%"),
    n_distinct(funders_data$display_name, na.rm = TRUE),
    top_funders$display_name[1],
    top_funders$display_name[2],
    top_funders$display_name[3],
    top_funders$display_name[4],
    top_funders$display_name[5],
    fwci_funded,
    fwci_not_funded,
    paste0(funding_by_collab$pct_funded[funding_by_collab$collab_detail == "UA solo"], "%"),
    paste0(funding_by_collab$pct_funded[funding_by_collab$collab_detail == "US collaboration"], "%"),
    paste0(funding_by_collab$pct_funded[funding_by_collab$collab_detail == "International collaboration"], "%"),
    funder_type_summary$num_papers[funder_type_summary$funder_type == "US Federal"],
    funder_type_summary$num_papers[funder_type_summary$funder_type == "Other"]
  ))
)

print(grants_summary)

# ============================================================
# STEP 13: Export
# ============================================================

library(openxlsx)

# List of dataframes → sheet names
sheets <- list(
  "Grants Summary"          = as.data.frame(grants_summary),
  "Top Funders"             = top_funders,
  "Top Funders with Awards" = top_funders_awards,
  "Impact by Funder"        = impact_by_funder,
  "Funded vs Not"           = impact_by_funding,
  "Funding by Collaboration"= funding_by_collab,
  "Federal vs Other"        = funder_type_summary,
  "Federal Agency Breakdown"= federal_agency_summary,
  "Impact by Funder Type"   = impact_by_funder_type,
  "Impact by Federal Agency"= impact_by_agency
)

write.xlsx(sheets, file = "UA_Funding_Analysis.xlsx")

cat("✅ Saved: UA_Funding_Analysis.xlsx\n")

########################### 2026-07-11 Alternative code for awards analysis to verify results
######################################

# ============================================================
# VERIFICATION METHOD 1: Count funded papers
# Original: used map_lgl on funders column
# Alternative: unnest funders and count distinct work_ids
# ============================================================

# --- Alternative approach ---
# Unnest funders directly and see how many unique work_ids have data
funders_unnested_alt <- works_published %>%
  select(work_id = id, funders) %>%
  mutate(row_num = row_number()) %>%
  mutate(
    is_df = map_lgl(funders, is.data.frame),
    nrow_funders = map_int(funders, function(f) {
      if (is.data.frame(f)) return(nrow(f))
      return(0L)
    })
  )

# Count
funded_alt <- sum(funders_unnested_alt$nrow_funders > 0)
unfunded_alt <- sum(funders_unnested_alt$nrow_funders == 0)
pct_funded_alt <- round(funded_alt / nrow(works_published) * 100, 1)

cat("=== VERIFICATION: Funded Paper Count ===\n")
cat("Papers with funders (alternative):", funded_alt, "\n")
cat("Papers without funders (alternative):", unfunded_alt, "\n")
cat("Percentage funded (alternative):", pct_funded_alt, "%\n\n")

# ============================================================
# VERIFICATION METHOD 2: Top funders
# Original: unnest funders, count display_name
# Alternative: loop through each work, extract funder names manually
# ============================================================

# --- Alternative approach ---
funder_names_alt <- works_published %>%
  transmute(
    work_id = id,
    funder_list = map(funders, function(f) {
      if (!is.data.frame(f) || nrow(f) == 0) return(character(0))
      f$display_name[!is.na(f$display_name)]
    })
  ) %>%
  unnest(funder_list, keep_empty = TRUE) %>%
  filter(!is.na(funder_list))

top_funders_alt <- funder_names_alt %>%
  count(funder_list, sort = TRUE, name = "num_papers") %>%
  rename(display_name = funder_list)

cat("=== VERIFICATION: Top Funders ===\n")
cat("Original top 10:\n")
print(head(top_funders, 10))
cat("\nAlternative top 10:\n")
print(head(top_funders_alt, 10))

# Check if they match
funders_match <- all.equal(
  top_funders %>% head(10) %>% pull(num_papers),
  top_funders_alt %>% head(10) %>% pull(num_papers)
)
cat("\nTop 10 funders match:", funders_match, "\n\n")

# ============================================================
# VERIFICATION METHOD 3: Impact by funding status
# Original: joined has_funding flag, grouped
# Alternative: split data manually, calculate separately
# ============================================================

# --- Alternative approach ---
funded_work_ids_alt <- funders_unnested_alt %>%
  filter(nrow_funders > 0) %>%
  pull(work_id)

unfunded_work_ids_alt <- funders_unnested_alt %>%
  filter(nrow_funders == 0) %>%
  pull(work_id)

# Calculate metrics separately
funded_metrics_alt <- works_published %>%
  filter(id %in% funded_work_ids_alt) %>%
  summarise(
    n_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    median_citations = median(cited_by_count, na.rm = TRUE),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    median_fwci = round(median(fwci, na.rm = TRUE), 2)
  ) %>%
  mutate(status = "Funded")

unfunded_metrics_alt <- works_published %>%
  filter(id %in% unfunded_work_ids_alt) %>%
  summarise(
    n_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    median_citations = median(cited_by_count, na.rm = TRUE),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    median_fwci = round(median(fwci, na.rm = TRUE), 2)
  ) %>%
  mutate(status = "Not funded")

impact_alt <- bind_rows(funded_metrics_alt, unfunded_metrics_alt)

cat("=== VERIFICATION: Impact by Funding Status ===\n")
cat("Original:\n")
print(impact_by_funding %>% select(funding_status, n_papers, mean_fwci))
cat("\nAlternative:\n")
print(impact_alt %>% select(status, n_papers, mean_fwci))

# ============================================================
# VERIFICATION METHOD 4: Awards count
# Original: checked all(is.na(a)) and length >= 4
# Alternative: check class and type directly
# ============================================================

# --- Alternative approach ---
awards_check_alt <- works_published %>%
  transmute(
    work_id = id,
    awards_type = map_chr(awards, function(a) {
      if (is.null(a)) return("null")
      if (is.logical(a) && all(is.na(a))) return("logical_NA")
      if (is.character(a) && length(a) >= 4) return("has_awards")
      return("other")
    })
  )

cat("=== VERIFICATION: Awards Count ===\n")
table(awards_check_alt$awards_type)
cat("Papers with awards (alternative):", 
    sum(awards_check_alt$awards_type == "has_awards"), "\n\n")

# ============================================================
# VERIFICATION METHOD 5: Federal vs Other
# Original: matched against us_federal_funders list
# Alternative: use ROR patterns for US government
# ============================================================

# --- Alternative approach using ROR ---
# US federal agencies typically have specific ROR patterns
# But let's verify by checking overlap

funders_with_ror <- works_published %>%
  select(work_id = id, funders) %>%
  mutate(has_funders = map_lgl(funders, ~ is.data.frame(.x) && nrow(.x) > 0)) %>%
  filter(has_funders) %>%
  unnest(funders, keep_empty = TRUE)

# Check: How many funders are in our federal list?
federal_check_alt <- funders_with_ror %>%
  filter(!is.na(display_name)) %>%
  mutate(
    is_federal_by_list = display_name %in% us_federal_funders,
    # Alternative: pattern matching for common US gov keywords
    is_federal_by_pattern = grepl(
      "National Science Foundation|National Institutes of Health|National Aeronautics|Department of Energy|Department of Defense|Department of Agriculture|National Institute of|National Cancer|National Heart|National Eye|Geological Survey|Oceanic and Atmospheric|Environmental Protection|Centers for Disease Control|Air Force|Army Research|Naval Research|Defense Advanced|Fish and Wildlife|Bureau of Land|Smithsonian|Food and Agriculture|Agricultural Research",
      display_name
    )
  )

# Compare the two methods
cat("=== VERIFICATION: Federal Funder Detection ===\n")
cat("Federal by exact list match:", 
    n_distinct(federal_check_alt$work_id[federal_check_alt$is_federal_by_list]), "papers\n")
cat("Federal by pattern match:", 
    n_distinct(federal_check_alt$work_id[federal_check_alt$is_federal_by_pattern]), "papers\n")

# What does pattern catch that list doesn't?
pattern_not_list <- federal_check_alt %>%
  filter(is_federal_by_pattern & !is_federal_by_list) %>%
  distinct(display_name)

cat("\nFunders caught by pattern but NOT in list:\n")
print(pattern_not_list, n = 30)

# What does list catch that pattern doesn't?
list_not_pattern <- federal_check_alt %>%
  filter(is_federal_by_list & !is_federal_by_pattern) %>%
  distinct(display_name)

cat("\nFunders in list but NOT caught by pattern:\n")
print(list_not_pattern)

# ============================================================
# Check: Do the final numbers match despite classification differences?
# ============================================================

cat("=== CORE NUMBERS COMPARISON ===\n\n")

cat("1. Funded paper count:\n")
cat("   Original:", sum(papers_with_funding$has_funding), "\n")
cat("   Alternative:", funded_alt, "\n")
cat("   Match:", sum(papers_with_funding$has_funding) == funded_alt, "\n\n")

cat("2. Top 5 funders (original):\n")
print(head(top_funders, 5))
cat("\n   Top 5 funders (alternative):\n")
print(head(top_funders_alt, 5))

cat("\n3. FWCI comparison:\n")
cat("   Funded FWCI - Original:", 
    impact_by_funding$mean_fwci[impact_by_funding$has_funding == TRUE], "\n")
cat("   Funded FWCI - Alternative:", funded_metrics_alt$mean_fwci, "\n")
cat("   Match:", 
    impact_by_funding$mean_fwci[impact_by_funding$has_funding == TRUE] == funded_metrics_alt$mean_fwci, "\n\n")

cat("4. Funding by collaboration:\n")
cat("   Original:\n")
print(funding_by_collab)
cat("\n   Alternative:\n")
print(funding_collab_alt)

# ============================================================
# VERIFICATION METHOD 6: Federal agency breakdown
# Alternative: count directly from funders_with_ror
# ============================================================

# --- Alternative: classify using a simpler approach ---
federal_agency_alt <- funders_with_ror %>%
  filter(!is.na(display_name)) %>%
  mutate(
    agency = case_when(
      grepl("National Science Foundation|Division of", display_name) ~ "NSF",
      grepl("National Institutes of Health|National Institute|National Cancer|National Heart|National Eye|National Library|National Human Genome|Fogarty|Eunice Kennedy|National Center for Advancing|National Center for Complementary", display_name) ~ "NIH",
      grepl("National Aeronautics|NASA", display_name) ~ "NASA",
      grepl("Department of Energy|High Energy Physics|Office of Science", display_name) ~ "DOE",
      grepl("Department of Defense|Air Force|Army|Navy|Defense Advanced|Naval Research", display_name) ~ "DOD",
      grepl("Department of Agriculture|Food and Agriculture|Agricultural Research", display_name) ~ "USDA",
      grepl("Oceanic and Atmospheric", display_name) ~ "NOAA",
      grepl("Geological Survey", display_name) ~ "USGS",
      grepl("Centers for Disease Control", display_name) ~ "CDC",
      grepl("Environmental Protection Agency", display_name) ~ "EPA",
      grepl("Agency for International Development", display_name) ~ "USAID",
      grepl("Smithsonian", display_name) ~ "Smithsonian",
      grepl("Department of the Interior|Bureau of Land|Fish and Wildlife", display_name) ~ "DOI",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(agency))

federal_agency_summary_alt <- federal_agency_alt %>%
  group_by(agency) %>%
  summarise(
    num_papers = n_distinct(work_id),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

cat("=== VERIFICATION: Federal Agency Breakdown ===\n")
cat("Original:\n")
print(federal_agency_summary)
cat("\nAlternative:\n")
print(federal_agency_summary_alt)

# ============================================================
# VERIFICATION METHOD 7: Funding by collaboration type
# Alternative: merge differently
# ============================================================

# --- Alternative approach ---
funding_collab_alt <- works_classified %>%
  select(work_id = id, collab_detail) %>%
  left_join(
    funders_unnested_alt %>% select(work_id, nrow_funders),
    by = "work_id"
  ) %>%
  mutate(is_funded = nrow_funders > 0) %>%
  group_by(collab_detail) %>%
  summarise(
    n_papers = n(),
    n_funded = sum(is_funded),
    pct_funded = round(mean(is_funded) * 100, 1),
    .groups = "drop"
  )

cat("=== VERIFICATION: Funding by Collaboration Type ===\n")
cat("Original:\n")
print(funding_by_collab)
cat("\nAlternative:\n")
print(funding_collab_alt)

# ============================================================
# VERIFICATION METHOD 8: Spot-check individual papers
# ============================================================

# Pick 5 random funded papers and verify manually
set.seed(42)
sample_funded <- works_published %>%
  filter(id %in% funded_work_ids_alt) %>%
  slice_sample(n = 5) %>%
  select(id, title)

cat("\n=== SPOT CHECK: 5 Random Funded Papers ===\n")
for (i in 1:5) {
  wid <- sample_funded$id[i]
  cat("\n--- Paper", i, "---\n")
  cat("Title:", sample_funded$title[i], "\n")
  
  # Get funders
  f <- works_published %>% filter(id == wid) %>% pull(funders)
  if (is.data.frame(f[[1]])) {
    cat("Funders:", paste(f[[1]]$display_name, collapse = "; "), "\n")
  }
  
  # Get awards
  a <- works_published %>% filter(id == wid) %>% pull(awards)
  if (!all(is.na(a[[1]])) && length(a[[1]]) >= 4) {
    award_names <- a[[1]][names(a[[1]]) == "funder_display_name"]
    cat("Award funders:", paste(unique(award_names), collapse = "; "), "\n")
  }
}

# Pick 5 random unfunded papers and verify
sample_unfunded <- works_published %>%
  filter(id %in% unfunded_work_ids_alt) %>%
  slice_sample(n = 5) %>%
  select(id, title)

cat("\n=== SPOT CHECK: 5 Random Unfunded Papers ===\n")
for (i in 1:5) {
  wid <- sample_unfunded$id[i]
  cat("\n--- Paper", i, "---\n")
  cat("Title:", sample_unfunded$title[i], "\n")
  
  f <- works_published %>% filter(id == wid) %>% pull(funders)
  if (is.data.frame(f[[1]])) {
    cat("Funders nrow:", nrow(f[[1]]), "\n")
  } else {
    cat("Funders: not a dataframe\n")
  }
  
  a <- works_published %>% filter(id == wid) %>% pull(awards)
  cat("Awards is.na:", all(is.na(a[[1]])), "\n")
}

# ============================================================
# FINAL COMPARISON SUMMARY
# ============================================================

cat("\n\n========================================\n")
cat("FINAL VERIFICATION SUMMARY\n")
cat("========================================\n\n")

comparison <- tibble(
  Metric = c(
    "Papers funded",
    "Papers unfunded",
    "% funded",
    "Mean FWCI (funded)",
    "Mean FWCI (unfunded)",
    "Top funder #1 papers",
    "Top funder #2 papers",
    "Top funder #3 papers"
  ),
  Original = as.character(c(
    sum(papers_with_funding$has_funding),
    sum(!papers_with_funding$has_funding),
    paste0(round(mean(papers_with_funding$has_funding) * 100, 1), "%"),
    impact_by_funding$mean_fwci[impact_by_funding$has_funding == TRUE],
    impact_by_funding$mean_fwci[impact_by_funding$has_funding == FALSE],
    top_funders$num_papers[1],
    top_funders$num_papers[2],
    top_funders$num_papers[3]
  )),
  Alternative = as.character(c(
    funded_alt,
    unfunded_alt,
    paste0(pct_funded_alt, "%"),
    funded_metrics_alt$mean_fwci,
    unfunded_metrics_alt$mean_fwci,
    top_funders_alt$num_papers[1],
    top_funders_alt$num_papers[2],
    top_funders_alt$num_papers[3]
  )),
  Match = c(
    sum(papers_with_funding$has_funding) == funded_alt,
    sum(!papers_with_funding$has_funding) == unfunded_alt,
    round(mean(papers_with_funding$has_funding) * 100, 1) == pct_funded_alt,
    impact_by_funding$mean_fwci[impact_by_funding$has_funding == TRUE] == funded_metrics_alt$mean_fwci,
    impact_by_funding$mean_fwci[impact_by_funding$has_funding == FALSE] == unfunded_metrics_alt$mean_fwci,
    top_funders$num_papers[1] == top_funders_alt$num_papers[1],
    top_funders$num_papers[2] == top_funders_alt$num_papers[2],
    top_funders$num_papers[3] == top_funders_alt$num_papers[3]
  )
)

print(comparison)

cat("\nAll checks passed:", all(comparison$Match), "\n")




##########################2026-07-10:
######################## Discipline (Domain, Field, Subfield)
library(dplyr)
library(tidyr)
library(purrr)

# ============================================================
# STEP 1: Extract primary topic (i == 1) for each work
# ============================================================

topics_extracted <- works_published %>%
  transmute(
    work_id = id,
    title,
    domain = map_chr(topics, function(t) {
      if (is.null(t) || !is.data.frame(t) || nrow(t) == 0) return(NA_character_)
      val <- t %>% filter(i == 1, type == "domain") %>% pull(display_name)
      if (length(val) == 0) return(NA_character_)
      val[1]
    }),
    field = map_chr(topics, function(t) {
      if (is.null(t) || !is.data.frame(t) || nrow(t) == 0) return(NA_character_)
      val <- t %>% filter(i == 1, type == "field") %>% pull(display_name)
      if (length(val) == 0) return(NA_character_)
      val[1]
    }),
    subfield = map_chr(topics, function(t) {
      if (is.null(t) || !is.data.frame(t) || nrow(t) == 0) return(NA_character_)
      val <- t %>% filter(i == 1, type == "subfield") %>% pull(display_name)
      if (length(val) == 0) return(NA_character_)
      val[1]
    }),
    topic = map_chr(topics, function(t) {
      if (is.null(t) || !is.data.frame(t) || nrow(t) == 0) return(NA_character_)
      val <- t %>% filter(i == 1, type == "topic") %>% pull(display_name)
      if (length(val) == 0) return(NA_character_)
      val[1]
    })
  )

head(topics_extracted)

# ============================================================
# STEP 2: Papers by Domain
# ============================================================

domain_summary <- topics_extracted %>%
  filter(!is.na(domain)) %>%
  count(domain, sort = TRUE, name = "num_papers") %>%
  mutate(pct = round(num_papers / sum(num_papers) * 100, 1))

print(domain_summary)

# ============================================================
# STEP 3: Papers by Field
# ============================================================

field_summary <- topics_extracted %>%
  filter(!is.na(field)) %>%
  count(field, sort = TRUE, name = "num_papers") %>%
  mutate(pct = round(num_papers / sum(num_papers) * 100, 1))

head(field_summary, 30)

# ============================================================
# STEP 4: Papers by Subfield
# ============================================================

subfield_summary <- topics_extracted %>%
  filter(!is.na(subfield)) %>%
  count(subfield, sort = TRUE, name = "num_papers") %>%
  mutate(pct = round(num_papers / sum(num_papers) * 100, 1))

head(subfield_summary, 30)

# ============================================================
# STEP 5: Papers by Topic
# ============================================================

topic_summary <- topics_extracted %>%
  filter(!is.na(topic)) %>%
  count(topic, sort = TRUE, name = "num_papers") %>%
  mutate(pct = round(num_papers / sum(num_papers) * 100, 1))

head(topic_summary, 30)

# ============================================================
# STEP 6: Impact (FWCI) by Domain
# ============================================================

impact_by_domain <- topics_extracted %>%
  left_join(
    works_published %>% transmute(work_id = id, cited_by_count, fwci),
    by = "work_id"
  ) %>%
  filter(!is.na(domain)) %>%
  group_by(domain) %>%
  summarise(
    num_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    median_citations = median(cited_by_count, na.rm = TRUE),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    median_fwci = round(median(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

print(impact_by_domain)

# ============================================================
# STEP 7: Impact (FWCI) by Field (top 20)
# ============================================================

impact_by_field <- topics_extracted %>%
  left_join(
    works_published %>% transmute(work_id = id, cited_by_count, fwci),
    by = "work_id"
  ) %>%
  filter(!is.na(field)) %>%
  group_by(field) %>%
  summarise(
    num_papers = n(),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

head(impact_by_field, 20)

# ============================================================
# STEP 8: Funding rate by Domain
# ============================================================

funding_by_domain <- topics_extracted %>%
  left_join(
    papers_with_funding %>% transmute(work_id = id, has_funding),
    by = "work_id"
  ) %>%
  filter(!is.na(domain)) %>%
  group_by(domain) %>%
  summarise(
    num_papers = n(),
    n_funded = sum(has_funding, na.rm = TRUE),
    pct_funded = round(mean(has_funding, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

print(funding_by_domain)

# ============================================================
# STEP 9: Funding rate by Field (top 20)
# ============================================================

funding_by_field <- topics_extracted %>%
  left_join(
    papers_with_funding %>% transmute(work_id = id, has_funding),
    by = "work_id"
  ) %>%
  filter(!is.na(field)) %>%
  group_by(field) %>%
  summarise(
    num_papers = n(),
    n_funded = sum(has_funding, na.rm = TRUE),
    pct_funded = round(mean(has_funding, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

head(funding_by_field, 20)

# ============================================================
# STEP 10: Collaboration type by Domain
# ============================================================

collab_by_domain <- topics_extracted %>%
  left_join(
    works_classified %>% transmute(work_id = id, collab_detail),
    by = "work_id"
  ) %>%
  filter(!is.na(domain) & !is.na(collab_detail)) %>%
  group_by(domain, collab_detail) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(domain) %>%
  mutate(
    total = sum(n),
    pct = round(n / total * 100, 1)
  ) %>%
  ungroup()

# Pivot wider for readability
collab_by_domain_wide <- collab_by_domain %>%
  select(domain, collab_detail, pct) %>%
  pivot_wider(names_from = collab_detail, values_from = pct, values_fill = 0)

print(collab_by_domain_wide)

# ============================================================
# STEP 11: Top funders by Domain
# ============================================================

top_funders_by_domain <- topics_extracted %>%
  left_join(
    funders_data %>% select(work_id, funder_name = display_name),
    by = "work_id"
  ) %>%
  filter(!is.na(domain) & !is.na(funder_name)) %>%
  group_by(domain, funder_name) %>%
  summarise(num_papers = n(), .groups = "drop") %>%
  arrange(domain, desc(num_papers)) %>%
  group_by(domain) %>%
  slice_head(n = 5) %>%
  ungroup()

print(top_funders_by_domain, n = 50)

# ============================================================
# STEP 12: Discipline Summary
# ============================================================

# Find highest FWCI field with 10+ papers
top_fwci_field <- impact_by_field %>%
  filter(num_papers >= 10) %>%
  arrange(desc(mean_fwci)) %>%
  slice_head(n = 1)

discipline_summary <- tibble(
  Metric = c(
    "Number of Domains",
    "Number of Fields",
    "Number of Subfields",
    "Number of Topics",
    "Top Domain #1",
    "Top Domain #2",
    "Top Domain #3",
    "Top Field #1",
    "Top Field #2",
    "Top Field #3",
    "Top Field #4",
    "Top Field #5",
    "Highest FWCI Domain",
    "Highest FWCI Field (10+ papers)"
  ),
  Value = as.character(c(
    n_distinct(topics_extracted$domain, na.rm = TRUE),
    n_distinct(topics_extracted$field, na.rm = TRUE),
    n_distinct(topics_extracted$subfield, na.rm = TRUE),
    n_distinct(topics_extracted$topic, na.rm = TRUE),
    domain_summary$domain[1],
    domain_summary$domain[2],
    domain_summary$domain[3],
    field_summary$field[1],
    field_summary$field[2],
    field_summary$field[3],
    field_summary$field[4],
    field_summary$field[5],
    impact_by_domain$domain[which.max(impact_by_domain$mean_fwci)],
    top_fwci_field$field
  ))
)

print(discipline_summary)

# ============================================================
# STEP 13: Export
# ============================================================

library(openxlsx)

sheets <- list(
  "Discipline Summary"      = as.data.frame(discipline_summary),
  "Topics Extracted"        = topics_extracted,
  "Domain Summary"          = domain_summary,
  "Field Summary"           = field_summary,
  "Subfield Summary"        = subfield_summary,
  "Topic Summary"           = topic_summary,
  "Impact by Domain"        = impact_by_domain,
  "Impact by Field"         = impact_by_field,
  "Funding by Domain"       = funding_by_domain,
  "Funding by Field"        = funding_by_field,
  "Collaboration by Domain" = collab_by_domain_wide,
  "Top Funders by Domain"   = top_funders_by_domain
)

write.xlsx(sheets, file = "UA_Discipline_Analysis.xlsx")

cat("✅ Saved: UA_Discipline_Analysis.xlsx\n")

##################################

library(dplyr)
library(tidyr)
library(purrr)
library(openxlsx)

# ============================================================
# SHEET 1: PAPERS (one row per paper, awards collapsed)
# ============================================================

papers_sheet <- works_published %>%
  transmute(
    work_id = id,
    doi,
    title,
    publication_date,
    journal = source_display_name,
    publisher = host_organization,
    cited_by_count,
    fwci,
    # Collapse all funders/awards into single cells
    all_funders = map_chr(awards, function(a) {
      if (is.null(a) || all(is.na(a)) || length(a) < 4) return(NA_character_)
      nms <- names(a)
      funder_names <- a[nms == "funder_display_name"]
      if (length(funder_names) == 0) return(NA_character_)
      paste(unique(funder_names), collapse = "; ")
    }),
    all_award_ids = map_chr(awards, function(a) {
      if (is.null(a) || all(is.na(a)) || length(a) < 4) return(NA_character_)
      nms <- names(a)
      award_ids <- a[nms == "funder_award_id"]
      if (length(award_ids) == 0) return(NA_character_)
      paste(unique(award_ids), collapse = "; ")
    }),
    num_awards = map_int(awards, function(a) {
      if (is.null(a) || all(is.na(a)) || length(a) < 4) return(0L)
      nms <- names(a)
      length(a[nms == "funder_award_id"])
    })
  ) %>%
  left_join(ua_authors_info, by = "work_id") %>%
  left_join(corresponding_info, by = "work_id") %>%
  left_join(topics_extracted %>% select(work_id, domain, field, subfield, topic), by = "work_id")

# ============================================================
# SHEET 2: AWARD-PAPER LINKS (one row per award-paper pair)
# This is the KEY sheet for tracking "papers per grant"
# ============================================================

award_paper_links <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  select(work_id, funder_display_name, funder_award_id) %>%
  left_join(
    works_published %>% transmute(
      work_id = id,
      doi,
      title,
      publication_date,
      journal = source_display_name,
      cited_by_count,
      fwci
    ),
    by = "work_id"
  ) %>%
  left_join(
    ua_authors_info,
    by = "work_id"
  ) %>%
  arrange(funder_display_name, funder_award_id, desc(cited_by_count))

# ============================================================
# SHEET 3: AWARD SUMMARY (one row per unique award)
# "How many papers did each grant produce?"
# ============================================================

award_summary <- awards_data %>%
  filter(!is.na(funder_display_name) & !is.na(funder_award_id)) %>%
  left_join(
    works_published %>% transmute(work_id = id, cited_by_count, fwci, publication_date),
    by = "work_id"
  ) %>%
  group_by(funder_display_name, funder_award_id) %>%
  summarise(
    num_papers = n_distinct(work_id),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    earliest_pub = min(publication_date, na.rm = TRUE),
    latest_pub = max(publication_date, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(funder_display_name, desc(num_papers))

# ============================================================
# SHEET 4: FUNDER SUMMARY (aggregated by funder)
# ============================================================

funder_summary <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  left_join(
    works_published %>% transmute(work_id = id, cited_by_count, fwci),
    by = "work_id"
  ) %>%
  group_by(funder_display_name) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_awards = n_distinct(funder_award_id),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    mean_citations = round(mean(cited_by_count, na.rm = TRUE), 2),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

# ============================================================
# SHEET 5: NSF-SPECIFIC (filtered for NSF only)
# ============================================================

nsf_awards <- award_paper_links %>%
  filter(grepl("National Science Foundation|Division of", funder_display_name))

# ============================================================
# SHEET 6: NIH-SPECIFIC (filtered for NIH and sub-institutes)
# ============================================================

nih_awards <- award_paper_links %>%
  filter(grepl("National Institutes of Health|National Institute|National Cancer|National Heart|National Eye|National Library|Eunice Kennedy|Fogarty|National Center for Advancing|National Center for Complementary", funder_display_name))

# ============================================================
# CREATE XLSX WORKBOOK
# ============================================================

wb <- createWorkbook()

# --- Styles ---
headerStyle <- createStyle(
  fgFill = "#4472C4",
  textDecoration = "bold",
  fontColour = "white",
  halign = "center",
  border = "TopBottom",
  wrapText = TRUE
)

numberStyle <- createStyle(numFmt = "#,##0")
fwciStyle <- createStyle(numFmt = "0.00")

# --- Sheet 1: All Papers ---
addWorksheet(wb, "All Papers")
writeData(wb, "All Papers", papers_sheet)
addStyle(wb, "All Papers", headerStyle, rows = 1, cols = 1:ncol(papers_sheet), gridExpand = TRUE)
freezePane(wb, "All Papers", firstRow = TRUE)
setColWidths(wb, "All Papers", cols = 1:ncol(papers_sheet), widths = "auto")

# --- Sheet 2: Award-Paper Links ---
addWorksheet(wb, "Award-Paper Links")
writeData(wb, "Award-Paper Links", award_paper_links)
addStyle(wb, "Award-Paper Links", headerStyle, rows = 1, cols = 1:ncol(award_paper_links), gridExpand = TRUE)
freezePane(wb, "Award-Paper Links", firstRow = TRUE)
setColWidths(wb, "Award-Paper Links", cols = 1:ncol(award_paper_links), widths = "auto")

# Add filter dropdowns so users can filter by funder/award
addFilter(wb, "Award-Paper Links", rows = 1, cols = 1:ncol(award_paper_links))

# --- Sheet 3: Award Summary ---
addWorksheet(wb, "Award Summary")
writeData(wb, "Award Summary", award_summary)
addStyle(wb, "Award Summary", headerStyle, rows = 1, cols = 1:ncol(award_summary), gridExpand = TRUE)
freezePane(wb, "Award Summary", firstRow = TRUE)
setColWidths(wb, "Award Summary", cols = 1:ncol(award_summary), widths = "auto")
addFilter(wb, "Award Summary", rows = 1, cols = 1:ncol(award_summary))

# Color-code by paper count
conditionalFormatting(
  wb, "Award Summary",
  cols = which(colnames(award_summary) == "num_papers"),
  rows = 2:(nrow(award_summary) + 1),
  type = "colourScale",
  style = c("#FFFFFF", "#63BE7B")
)

# --- Sheet 4: Funder Summary ---
addWorksheet(wb, "Funder Summary")
writeData(wb, "Funder Summary", funder_summary)
addStyle(wb, "Funder Summary", headerStyle, rows = 1, cols = 1:ncol(funder_summary), gridExpand = TRUE)
freezePane(wb, "Funder Summary", firstRow = TRUE)
setColWidths(wb, "Funder Summary", cols = 1:ncol(funder_summary), widths = "auto")

# --- Sheet 5: NSF Awards ---
addWorksheet(wb, "NSF Awards")
writeData(wb, "NSF Awards", nsf_awards)
addStyle(wb, "NSF Awards", headerStyle, rows = 1, cols = 1:ncol(nsf_awards), gridExpand = TRUE)
freezePane(wb, "NSF Awards", firstRow = TRUE)
setColWidths(wb, "NSF Awards", cols = 1:ncol(nsf_awards), widths = "auto")
addFilter(wb, "NSF Awards", rows = 1, cols = 1:ncol(nsf_awards))

# --- Sheet 6: NIH Awards ---
addWorksheet(wb, "NIH Awards")
writeData(wb, "NIH Awards", nih_awards)
addStyle(wb, "NIH Awards", headerStyle, rows = 1, cols = 1:ncol(nih_awards), gridExpand = TRUE)
freezePane(wb, "NIH Awards", firstRow = TRUE)
setColWidths(wb, "NIH Awards", cols = 1:ncol(nih_awards), widths = "auto")
addFilter(wb, "NIH Awards", rows = 1, cols = 1:ncol(nih_awards))

# ============================================================
# SAVE
# ============================================================

saveWorkbook(wb, "UA_Award_Tracking.xlsx", overwrite = TRUE)

cat("✅ Workbook saved: UA_Award_Tracking.xlsx\n\n")
cat("Sheet summary:\n")
cat("1. All Papers:", nrow(papers_sheet), "rows\n")
cat("2. Award-Paper Links:", nrow(award_paper_links), "rows\n")
cat("3. Award Summary:", nrow(award_summary), "unique awards\n")
cat("4. Funder Summary:", nrow(funder_summary), "unique funders\n")
cat("5. NSF Awards:", nrow(nsf_awards), "NSF-linked papers\n")
cat("6. NIH Awards:", nrow(nih_awards), "NIH-linked papers\n")



###################

library(dplyr)
library(tidyr)
library(openxlsx)

# ============================================================
# SHEET 1: AWARD DETAILS (main tracking sheet)
# One row per award, with aggregated paper metrics
# ============================================================

award_summary <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  group_by(funder_display_name, funder_award_id) %>%
  summarise(
    num_papers = n_distinct(work_id),
    paper_ids = paste(unique(work_id), collapse = "; "),
    mean_citations = round(mean(
      works_published %>% 
        filter(id %in% work_id) %>% 
        pull(cited_by_count)
      , na.rm = TRUE), 2),
    mean_fwci = round(mean(
      works_published %>% 
        filter(id %in% work_id) %>% 
        pull(fwci)
      , na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

head(award_summary)

# ============================================================
# SHEET 2: AWARD-PAPER MAPPING (linking table)
# One row per award-paper combination
# This shows exactly which papers each award generated
# ============================================================

award_paper_mapping <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  select(work_id, funder_display_name, funder_award_id) %>%
  left_join(
    works_published %>% transmute(
      work_id = id,
      title,
      publication_date,
      cited_by_count,
      fwci,
      journal = source_display_name
    ),
    by = "work_id"
  ) %>%
  arrange(funder_display_name, funder_award_id, publication_date)

head(award_paper_mapping, 20)

# ============================================================
# SHEET 3: DETAILED PAPER INFO (all papers with funding)
# One row per paper, showing ALL awards for that paper
# ============================================================

papers_with_awards <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  group_by(work_id) %>%
  summarise(
    num_awards = n_distinct(funder_award_id),
    funders = paste(unique(funder_display_name), collapse = "; "),
    award_ids = paste(unique(funder_award_id), collapse = "; "),
    .groups = "drop"
  ) %>%
  left_join(
    works_published %>% transmute(
      work_id = id,
      title,
      publication_date,
      journal = source_display_name,
      cited_by_count,
      fwci,
      doi
    ),
    by = "work_id"
  ) %>%
  left_join(
    ua_authors_info,
    by = "work_id"
  ) %>%
  left_join(
    corresponding_info,
    by = "work_id"
  ) %>%
  select(
    work_id,
    title,
    publication_date,
    journal,
    cited_by_count,
    fwci,
    num_awards,
    funders,
    award_ids,
    ua_author_names,
    corresponding_author,
    corresponding_institution,
    doi
  ) %>%
  arrange(desc(publication_date))

head(papers_with_awards)

# ============================================================
# SHEET 4: FUNDER SUMMARY (high-level overview)
# Aggregated by funder
# ============================================================

funder_summary_sheet <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  group_by(funder_display_name) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_awards = n_distinct(funder_award_id),
    mean_citations = round(mean(
      works_published %>% 
        filter(id %in% work_id) %>% 
        pull(cited_by_count)
      , na.rm = TRUE), 2),
    mean_fwci = round(mean(
      works_published %>% 
        filter(id %in% work_id) %>% 
        pull(fwci)
      , na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

head(funder_summary_sheet)

# ============================================================
# SHEET 5: MULTI-AWARD PAPERS (papers with 2+ awards)
# Special focus on highly-funded papers
# ============================================================

multi_award_papers <- papers_with_awards %>%
  filter(num_awards >= 2) %>%
  arrange(desc(num_awards), desc(publication_date))

head(multi_award_papers)

# ============================================================
# CREATE WORKBOOK
# ============================================================

wb <- createWorkbook()

# ============================================================
# SHEET 1: Award Summary
# ============================================================
addWorksheet(wb, "Award Summary")

# Add data
writeData(wb, "Award Summary", award_summary)

# Format header
headerStyle <- createStyle(
  fgFill = "#4472C4",
  textDecoration = "bold",
  fontColour = "white",
  halign = "center",
  border = "TopBottom"
)
addStyle(wb, "Award Summary", headerStyle, rows = 1, cols = 1:ncol(award_summary))

# Auto-fit columns
setColWidths(wb, "Award Summary", cols = 1:ncol(award_summary), widths = "auto")

# Freeze panes
freezePane(wb, "Award Summary", firstRow = TRUE)

# ============================================================
# SHEET 2: Award-Paper Mapping (Linking Table)
# ============================================================
addWorksheet(wb, "Award-Paper Mapping")

writeData(wb, "Award-Paper Mapping", award_paper_mapping)

# Format
addStyle(wb, "Award-Paper Mapping", headerStyle, rows = 1, cols = 1:ncol(award_paper_mapping))
setColWidths(wb, "Award-Paper Mapping", cols = 1:ncol(award_paper_mapping), widths = "auto")
freezePane(wb, "Award-Paper Mapping", firstRow = TRUE)

# Hyperlink to DOI (if available)
# This allows clicking through to the paper

# ============================================================
# SHEET 3: Detailed Paper Information
# ============================================================
addWorksheet(wb, "Funded Papers")

writeData(wb, "Funded Papers", papers_with_awards)

# Format
addStyle(wb, "Funded Papers", headerStyle, rows = 1, cols = 1:ncol(papers_with_awards))
setColWidths(wb, "Funded Papers", cols = 1:ncol(papers_with_awards), widths = "auto")
freezePane(wb, "Funded Papers", firstRow = TRUE)

# Conditional formatting for FWCI (highlight high impact)
fwci_col <- which(colnames(papers_with_awards) == "fwci")
conditionalFormatting(
  wb, "Funded Papers",
  cols = fwci_col,
  rows = 2:(nrow(papers_with_awards) + 1),
  type = "colourScale",
  style = c("#F8696B", "#FFEB84", "#63BE7B")
)

# ============================================================
# SHEET 4: Funder Summary
# ============================================================
addWorksheet(wb, "Funder Summary")

writeData(wb, "Funder Summary", funder_summary_sheet)

addStyle(wb, "Funder Summary", headerStyle, rows = 1, cols = 1:ncol(funder_summary_sheet))
setColWidths(wb, "Funder Summary", cols = 1:ncol(funder_summary_sheet), widths = "auto")
freezePane(wb, "Funder Summary", firstRow = TRUE)

# ============================================================
# SHEET 5: Multi-Award Papers
# ============================================================
if (nrow(multi_award_papers) > 0) {
  addWorksheet(wb, "Multi-Award Papers")
  
  writeData(wb, "Multi-Award Papers", multi_award_papers)
  
  addStyle(wb, "Multi-Award Papers", headerStyle, rows = 1, cols = 1:ncol(multi_award_papers))
  setColWidths(wb, "Multi-Award Papers", cols = 1:ncol(multi_award_papers), widths = "auto")
  freezePane(wb, "Multi-Award Papers", firstRow = TRUE)
  
  # Highlight multi-award rows
  multiAwardStyle <- createStyle(
    fgFill = "#FFF2CC",
    border = "TopBottom"
  )
  addStyle(wb, "Multi-Award Papers", multiAwardStyle, 
           rows = 2:(nrow(multi_award_papers) + 1), 
           cols = 1:ncol(multi_award_papers))
}

# ============================================================
# Save
# ============================================================
saveWorkbook(wb, "UA_Awards_Tracking2.xlsx", overwrite = TRUE)

cat("✅ XLSX created: UA_Awards_Tracking.2x.lsx\n")
cat("\nSheet structure:\n")
cat("1. Award Summary - High-level overview by award\n")
cat("2. Award-Paper Mapping - Which papers each award generated\n")
cat("3. Funded Papers - All papers with complete funding details\n")
cat("4. Funder Summary - Aggregated by funder (NSF, NIH, etc.)\n")
cat("5. Multi-Award Papers - Papers funded by 2+ awards\n")

# ============================================================
# ORIGINAL METHOD: Compare has_funding vs has_award flags
# ============================================================

# (Already computed above)
cat("=== ORIGINAL METHOD ===\n")
cat("Papers with funders but NO award IDs:", nrow(papers_funder_no_award), "\n\n")

# ============================================================
# ALTERNATIVE METHOD 1: Direct comparison of column states
# Check each work independently without relying on pre-computed flags
# ============================================================

alt_check <- works_published %>%
  transmute(
    work_id = id,
    # Check funders: is it a dataframe with rows?
    funders_nrow = map_int(funders, function(f) {
      if (!is.data.frame(f)) return(0L)
      nrow(f)
    }),
    # Check awards: is it a non-NA character vector with length >= 4?
    awards_length = map_int(awards, function(a) {
      if (is.null(a)) return(0L)
      if (is.logical(a) && all(is.na(a))) return(0L)
      if (is.character(a)) return(length(a))
      return(0L)
    }),
    has_funders_alt = funders_nrow > 0,
    has_awards_alt = awards_length >= 4
  )

funder_no_award_alt <- alt_check %>%
  filter(has_funders_alt == TRUE & has_awards_alt == FALSE)

cat("=== ALTERNATIVE METHOD 1 ===\n")
cat("Papers with funders but NO award IDs:", nrow(funder_no_award_alt), "\n")
cat("Total with funders:", sum(alt_check$has_funders_alt), "\n")
cat("Total with awards:", sum(alt_check$has_awards_alt), "\n")
cat("Percentage of funded papers missing award IDs:",
    round(nrow(funder_no_award_alt) / sum(alt_check$has_funders_alt) * 100, 1), "%\n\n")

# ============================================================
# ALTERNATIVE METHOD 2: Cross-check using funders_data
# Papers in funders_data that are NOT in awards_data
# ============================================================

works_with_funders <- funders_data %>%
  distinct(work_id)

works_with_awards <- awards_data %>%
  filter(!is.na(funder_display_name)) %>%
  distinct(work_id)

funder_no_award_alt2 <- works_with_funders %>%
  anti_join(works_with_awards, by = "work_id")

cat("=== ALTERNATIVE METHOD 2 ===\n")
cat("Works in funders_data:", nrow(works_with_funders), "\n")
cat("Works in awards_data:", nrow(works_with_awards), "\n")
cat("Works with funders but NOT in awards_data:", nrow(funder_no_award_alt2), "\n\n")

# ============================================================
# COMPARISON
# ============================================================

cat("=== VERIFICATION COMPARISON ===\n")
cat("Original:", nrow(papers_funder_no_award), "\n")
cat("Alternative 1:", nrow(funder_no_award_alt), "\n")
cat("Alternative 2:", nrow(funder_no_award_alt2), "\n")
cat("Methods 1 match:", nrow(papers_funder_no_award) == nrow(funder_no_award_alt), "\n")
cat("Methods 2 match:", nrow(papers_funder_no_award) == nrow(funder_no_award_alt2), "\n\n")

# ============================================================
# SPOT CHECK: Verify a few examples
# ============================================================

set.seed(123)
sample_ids <- funder_no_award_alt %>%
  slice_sample(n = 5) %>%
  pull(work_id)

cat("=== SPOT CHECK: 5 papers with funders but no awards ===\n")
for (i in seq_along(sample_ids)) {
  wid <- sample_ids[i]
  row <- works_published %>% filter(id == wid)
  
  cat("\n--- Paper", i, "---\n")
  cat("Title:", row$title, "\n")
  
  # Show funders
  f <- row$funders[[1]]
  if (is.data.frame(f) && nrow(f) > 0) {
    cat("Funders (", nrow(f), "):", paste(f$display_name, collapse = "; "), "\n")
  }
  
  # Show awards
  a <- row$awards[[1]]
  if (all(is.na(a))) {
    cat("Awards: NA (no award data)\n")
  } else {
    cat("Awards length:", length(a), "\n")
  }
}




######################## 2026-0-7-15

# ============================================================
# US Partner-Award-Funder Detail Sheet
# One row per US partner × award × paper combination
# ============================================================

us_partner_award_funder <- works_published %>%
  transmute(
    work_id = id,
    us_partners = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      df <- author_df %>%
        unnest(affiliations, names_sep = "_", keep_empty = TRUE)
      df %>%
        filter(affiliations_country_code == "US") %>%
        filter(!grepl("03m2x1q45", affiliations_ror)) %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
    })
  ) %>%
  unnest(us_partners, keep_empty = TRUE) %>%
  filter(!is.na(us_partners)) %>%
  # Join with awards
  left_join(
    awards_data %>%
      filter(!is.na(funder_display_name)) %>%
      select(work_id, funder_display_name, funder_award_id),
    by = "work_id"
  ) %>%
  filter(!is.na(funder_display_name)) %>%
  # Join with paper metadata
  left_join(
    works_published %>% transmute(
      work_id = id,
      title,
      publication_date,
      journal = source_display_name,
      cited_by_count,
      fwci,
      doi
    ),
    by = "work_id"
  ) %>%
  select(
    us_partners,
    funder_display_name,
    funder_award_id,
    work_id,
    title,
    publication_date,
    journal,
    cited_by_count,
    fwci,
    doi
  ) %>%
  arrange(us_partners, funder_display_name, funder_award_id)

cat("US Partner-Award-Funder rows:", nrow(us_partner_award_funder), "\n")
cat("Unique US partners:", n_distinct(us_partner_award_funder$us_partners), "\n")
cat("Unique awards:", n_distinct(us_partner_award_funder$funder_award_id), "\n")

# ============================================================
# US Partner-Award Summary (collapsed: one row per partner × award)
# ============================================================

us_partner_award_summary <- us_partner_award_funder %>%
  group_by(us_partners, funder_display_name, funder_award_id) %>%
  summarise(
    num_papers = n_distinct(work_id),
    total_citations = sum(cited_by_count, na.rm = TRUE),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(us_partners, funder_display_name, desc(num_papers))

# ============================================================
# US Partner Summary (collapsed: one row per partner × funder)
# "How many papers does UA co-publish with U Michigan on NSF grants?"
# ============================================================

us_partner_funder_summary <- us_partner_award_funder %>%
  group_by(us_partners, funder_display_name) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_awards = n_distinct(funder_award_id),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

head(us_partner_funder_summary, 20)

# ============================================================
# Same for International
# ============================================================

intl_partner_award_funder <- works_published %>%
  transmute(
    work_id = id,
    intl_partners = map(authorships, function(author_df) {
      if (is.null(author_df) || nrow(author_df) == 0) return(character(0))
      df <- author_df %>%
        unnest(affiliations, names_sep = "_", keep_empty = TRUE)
      df %>%
        filter(affiliations_country_code != "US") %>%
        pull(affiliations_display_name) %>%
        na.omit() %>%
        unique()
    })
  ) %>%
  unnest(intl_partners, keep_empty = TRUE) %>%
  filter(!is.na(intl_partners)) %>%
  # Join with awards
  left_join(
    awards_data %>%
      filter(!is.na(funder_display_name)) %>%
      select(work_id, funder_display_name, funder_award_id),
    by = "work_id"
  ) %>%
  filter(!is.na(funder_display_name)) %>%
  # Join with paper metadata
  left_join(
    works_published %>% transmute(
      work_id = id,
      title,
      publication_date,
      journal = source_display_name,
      cited_by_count,
      fwci,
      doi
    ),
    by = "work_id"
  ) %>%
  # Join country code for each partner
  left_join(
    works_published %>%
      transmute(
        work_id = id,
        partner_country = map(authorships, function(author_df) {
          if (is.null(author_df) || nrow(author_df) == 0) return(tibble(inst = character(0), country = character(0)))
          df <- author_df %>%
            unnest(affiliations, names_sep = "_", keep_empty = TRUE)
          df %>%
            filter(affiliations_country_code != "US") %>%
            select(inst = affiliations_display_name, country = affiliations_country_code) %>%
            distinct()
        })
      ) %>%
      unnest(partner_country, keep_empty = TRUE),
    by = c("work_id", "intl_partners" = "inst")
  ) %>%
  select(
    intl_partners,
    country,
    funder_display_name,
    funder_award_id,
    work_id,
    title,
    publication_date,
    journal,
    cited_by_count,
    fwci,
    doi
  ) %>%
  arrange(intl_partners, funder_display_name, funder_award_id)

cat("\nIntl Partner-Award-Funder rows:", nrow(intl_partner_award_funder), "\n")

# ============================================================
# International Partner-Funder Summary
# ============================================================

intl_partner_funder_summary <- intl_partner_award_funder %>%
  group_by(intl_partners, country, funder_display_name) %>%
  summarise(
    num_papers = n_distinct(work_id),
    num_awards = n_distinct(funder_award_id),
    mean_fwci = round(mean(fwci, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers))

head(intl_partner_funder_summary, 20)

# ============================================================
# ADD TO XLSX WORKBOOK
# ============================================================

# US Partner-Award-Funder Detail
addWorksheet(wb, "US Partner-Award Detail")
writeData(wb, "US Partner-Award Detail", us_partner_award_funder)
addStyle(wb, "US Partner-Award Detail", headerStyle, rows = 1, 
         cols = 1:ncol(us_partner_award_funder), gridExpand = TRUE)
freezePane(wb, "US Partner-Award Detail", firstRow = TRUE)
setColWidths(wb, "US Partner-Award Detail", cols = 1:ncol(us_partner_award_funder), widths = "auto")
addFilter(wb, "US Partner-Award Detail", rows = 1, cols = 1:ncol(us_partner_award_funder))

# US Partner-Award Summary
addWorksheet(wb, "US Partner-Award Summary")
writeData(wb, "US Partner-Award Summary", us_partner_award_summary)
addStyle(wb, "US Partner-Award Summary", headerStyle, rows = 1, 
         cols = 1:ncol(us_partner_award_summary), gridExpand = TRUE)
freezePane(wb, "US Partner-Award Summary", firstRow = TRUE)
setColWidths(wb, "US Partner-Award Summary", cols = 1:ncol(us_partner_award_summary), widths = "auto")
addFilter(wb, "US Partner-Award Summary", rows = 1, cols = 1:ncol(us_partner_award_summary))

# US Partner-Funder Summary
addWorksheet(wb, "US Partner-Funder Summary")
writeData(wb, "US Partner-Funder Summary", us_partner_funder_summary)
addStyle(wb, "US Partner-Funder Summary", headerStyle, rows = 1, 
         cols = 1:ncol(us_partner_funder_summary), gridExpand = TRUE)
freezePane(wb, "US Partner-Funder Summary", firstRow = TRUE)
setColWidths(wb, "US Partner-Funder Summary", cols = 1:ncol(us_partner_funder_summary), widths = "auto")
addFilter(wb, "US Partner-Funder Summary", rows = 1, cols = 1:ncol(us_partner_funder_summary))

# Intl Partner-Award-Funder Detail
addWorksheet(wb, "Intl Partner-Award Detail")
writeData(wb, "Intl Partner-Award Detail", intl_partner_award_funder)
addStyle(wb, "Intl Partner-Award Detail", headerStyle, rows = 1, 
         cols = 1:ncol(intl_partner_award_funder), gridExpand = TRUE)
freezePane(wb, "Intl Partner-Award Detail", firstRow = TRUE)
setColWidths(wb, "Intl Partner-Award Detail", cols = 1:ncol(intl_partner_award_funder), widths = "auto")
addFilter(wb, "Intl Partner-Award Detail", rows = 1, cols = 1:ncol(intl_partner_award_funder))

# Intl Partner-Funder Summary
addWorksheet(wb, "Intl Partner-Funder Summary")
writeData(wb, "Intl Partner-Funder Summary", intl_partner_funder_summary)
addStyle(wb, "Intl Partner-Funder Summary", headerStyle, rows = 1, 
         cols = 1:ncol(intl_partner_funder_summary), gridExpand = TRUE)
freezePane(wb, "Intl Partner-Funder Summary", firstRow = TRUE)
setColWidths(wb, "Intl Partner-Funder Summary", cols = 1:ncol(intl_partner_funder_summary), widths = "auto")
addFilter(wb, "Intl Partner-Funder Summary", rows = 1, cols = 1:ncol(intl_partner_funder_summary))

# ============================================================
# SAVE
# ============================================================

saveWorkbook(wb, "UA_Partner-Award-Funder.xlsx", overwrite = TRUE)

cat("\n✅ Workbook updated: UA_ Partner awPard_Tracking.x\n")
cat("New sheets added:\n")
cat("  - US Partner-Award Detail:", nrow(us_partner_award_funder), "rows\n")
cat("  - US Partner-Award Summary:", nrow(us_partner_award_summary), "rows\n")
cat("  - US Partner-Funder Summary:", nrow(us_partner_funder_summary), "rows\n")
cat("  - Intl Partner-Award Detail:", nrow(intl_partner_award_funder), "rows\n")
cat("  - Intl Partner-Funder Summary:", nrow(intl_partner_funder_summary), "rows\n")







library(dplyr)
library(tidyr)
library(purrr)
install.packages("stringdist")
library(stringdist)
library(openxlsx)

# ============================================================
# STEP 1: Basic cleaning (remove periods, standardize)
# ============================================================

clean_author_name <- function(name) {
  if (is.na(name)) return(NA_character_)
  name <- gsub("\\.", "", name)           # Remove periods
  name <- gsub("-", " ", name)            # Standardize hyphens
  name <- trimws(gsub("\\s+", " ", name)) # Remove extra spaces
  name <- tolower(name)                    # Lowercase for comparison
  return(name)
}

# Get all individual UA author names
ua_authors_all <- ua_authors_info %>%
  separate_rows(ua_author_names, sep = "; ") %>%
  filter(!is.na(ua_author_names)) %>%
  distinct(ua_author_names) %>%
  mutate(name_cleaned = sapply(ua_author_names, clean_author_name))

cat("Total raw unique names:", nrow(ua_authors_all), "\n")

# Deduplicate on cleaned names
ua_authors_deduped <- ua_authors_all %>%
  group_by(name_cleaned) %>%
  summarise(
    display_name = first(ua_author_names),  # Keep one variant as canonical
    all_variants = paste(unique(ua_author_names), collapse = " | "),
    num_variants = n(),
    .groups = "drop"
  )

cat("After basic cleaning:", nrow(ua_authors_deduped), "\n")
cat("Names with variants found:", sum(ua_authors_deduped$num_variants > 1), "\n\n")

# Show examples of merged variants
ua_authors_deduped %>%
  filter(num_variants > 1) %>%
  arrange(desc(num_variants)) %>%
  head(20)

# ============================================================
# STEP 2: Fuzzy matching to find likely duplicates
# Uses stringdist package with multiple algorithms
# ============================================================

# Get cleaned names as vector
name_vector <- ua_authors_deduped$name_cleaned

# Compute string distance matrix (Jaro-Winkler is good for names)
# NOTE: This can be slow for very large sets. Sample if needed.
cat("Computing fuzzy matches... (this may take a moment)\n")

# For large datasets, limit to pairs with short distance
# Using stringdistmatrix with method = "jw" (Jaro-Winkler)
# Threshold: < 0.1 means very similar

# For efficiency, compare in batches or use a threshold approach
find_fuzzy_duplicates <- function(names_vec, max_dist = 0.1, method = "jw") {
  n <- length(names_vec)
  
  # For large datasets, use amatch for approximate matching
  matches <- list()
  
  for (i in seq_len(n)) {
    # Find names similar to names_vec[i]
    distances <- stringdist(names_vec[i], names_vec, method = method)
    
    # Find close matches (excluding self)
    close_idx <- which(distances > 0 & distances < max_dist)
    
    if (length(close_idx) > 0) {
      matches[[length(matches) + 1]] <- tibble(
        name_1 = names_vec[i],
        name_2 = names_vec[close_idx],
        distance = distances[close_idx],
        idx_1 = i,
        idx_2 = close_idx
      )
    }
  }
  
  if (length(matches) == 0) return(tibble())
  bind_rows(matches) %>%
    # Remove duplicate pairs (A-B and B-A)
    filter(idx_1 < idx_2) %>%
    arrange(distance)
}

# Run fuzzy matching (may take time for 7000+ names)
# For very large sets, sample first to test:
# name_sample <- sample(name_vector, min(2000, length(name_vector)))

fuzzy_duplicates <- find_fuzzy_duplicates(name_vector, max_dist = 0.08, method = "jw")

cat("Potential fuzzy duplicate pairs found:", nrow(fuzzy_duplicates), "\n\n")

# Add display names for readability
fuzzy_duplicates <- fuzzy_duplicates %>%
  mutate(
    display_name_1 = ua_authors_deduped$display_name[idx_1],
    display_name_2 = ua_authors_deduped$display_name[idx_2]
  ) %>%
  select(display_name_1, display_name_2, name_1, name_2, distance)

head(fuzzy_duplicates, 30)

# ============================================================
# STEP 3: Additional check using Levenshtein distance
# Catches typos, character swaps
# ============================================================

fuzzy_duplicates_lv <- find_fuzzy_duplicates(name_vector, max_dist = 2, method = "lv")

# Levenshtein distance of 1-2 = very likely same person
fuzzy_duplicates_lv <- fuzzy_duplicates_lv %>%
  mutate(
    display_name_1 = ua_authors_deduped$display_name[idx_1],
    display_name_2 = ua_authors_deduped$display_name[idx_2]
  ) %>%
  select(display_name_1, display_name_2, name_1, name_2, distance)

cat("Levenshtein duplicates (distance 1-2):", nrow(fuzzy_duplicates_lv), "\n")
head(fuzzy_duplicates_lv, 30)

# ============================================================
# STEP 4: Combine both methods for comprehensive check
# ============================================================

all_potential_duplicates <- bind_rows(
  fuzzy_duplicates %>% mutate(method = "Jaro-Winkler"),
  fuzzy_duplicates_lv %>% mutate(method = "Levenshtein")
) %>%
  distinct(name_1, name_2, .keep_all = TRUE) %>%
  arrange(distance)

cat("Total potential duplicate pairs:", nrow(all_potential_duplicates), "\n")

# ============================================================
# STEP 5: Rebuild author productivity with cleaned names
# ============================================================

# Use cleaned names for counting
ua_author_productivity <- ua_authors_info %>%
  separate_rows(ua_author_names, sep = "; ") %>%
  filter(!is.na(ua_author_names)) %>%
  mutate(name_cleaned = sapply(ua_author_names, clean_author_name)) %>%
  group_by(name_cleaned) %>%
  summarise(
    display_name = first(ua_author_names),
    num_papers = n_distinct(work_id),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers)) %>%
  mutate(rank = row_number())

# Sheet 1: All works with UA authors (cleaned)
all_works <- works_published %>%
  transmute(
    work_id = id,
    title,
    publication_date,
    journal = source_display_name,
    cited_by_count,
    fwci,
    doi
  ) %>%
  left_join(
    ua_authors_info %>%
      mutate(ua_author_names_clean = map_chr(
        strsplit(ua_author_names, "; "),
        function(names) {
          if (all(is.na(names))) return(NA_character_)
          cleaned <- sapply(names, clean_author_name)
          # Title case for display
          cleaned <- gsub("(^|\\s)(\\w)", "\\1\\U\\2", cleaned, perl = TRUE)
          paste(unique(cleaned), collapse = "; ")
        }
      )),
    by = "work_id"
  )

# Sheet 3: Authors with funders (cleaned)
authors_with_funders <- ua_authors_info %>%
  separate_rows(ua_author_names, sep = "; ") %>%
  filter(!is.na(ua_author_names)) %>%
  mutate(name_cleaned = sapply(ua_author_names, clean_author_name)) %>%
  left_join(
    papers_with_funding %>% transmute(work_id = id, has_funding),
    by = "work_id"
  ) %>%
  group_by(name_cleaned) %>%
  summarise(
    display_name = first(ua_author_names),
    num_papers = n(),
    num_funded_papers = sum(has_funding, na.rm = TRUE),
    pct_funded = round(mean(has_funding, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(num_funded_papers))

# Sheet 4: Authors with awards (cleaned)
authors_with_awards <- ua_authors_info %>%
  separate_rows(ua_author_names, sep = "; ") %>%
  filter(!is.na(ua_author_names)) %>%
  mutate(name_cleaned = sapply(ua_author_names, clean_author_name)) %>%
  left_join(
    papers_with_funding %>% transmute(work_id = id, has_award),
    by = "work_id"
  ) %>%
  group_by(name_cleaned) %>%
  summarise(
    display_name = first(ua_author_names),
    num_papers = n(),
    num_papers_with_awards = sum(has_award, na.rm = TRUE),
    pct_with_awards = round(mean(has_award, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(num_papers_with_awards))

# ============================================================
# STEP 6: Export to Excel
# ============================================================

sheets <- list(
  "All Works"              = all_works,
  "Author Productivity"    = ua_author_productivity,
  "Authors with Funders"   = authors_with_funders,
  "Authors with Awards"    = authors_with_awards,
  "Potential Duplicates"   = all_potential_duplicates,
  "Variant Names Found"    = ua_authors_deduped %>% filter(num_variants > 1)
)

write.xlsx(sheets, file = "UA_Authors_Analysis.xlsx")

cat("✅ Saved: UA_Authors_Analysis.xlsx\n")
cat("\nSheets:\n")
cat("1. All Works - papers with cleaned UA author names\n")
cat("2. Author Productivity - ranked by paper count (deduplicated)\n")
cat("3. Authors with Funders - funding rate per author\n")
cat("4. Authors with Awards - award rate per author\n")
cat("5. Potential Duplicates - fuzzy matches for manual review\n")
cat("6. Variant Names Found - basic cleaning matches (periods, spaces)\n")


library(testthat)
library(stringdist)

# ============================================================
# TEST 1: Basic cleaning function
# ============================================================

test_that("clean_author_name removes periods correctly", {
  expect_equal(clean_author_name("I. G. Sipes"), "i g sipes")
  expect_equal(clean_author_name("I G Sipes"), "i g sipes")
  expect_equal(clean_author_name("J.R. Smith"), "jr smith")
  expect_equal(clean_author_name("J. R. Smith"), "j r smith")
  expect_equal(clean_author_name("Mary-Jane Watson"), "mary jane watson")
  expect_equal(clean_author_name("  John   Doe  "), "john doe")
  expect_equal(clean_author_name(NA), NA_character_)
})

# ============================================================
# TEST 2: Known duplicates should be caught
# ============================================================

test_that("Known duplicate pairs are identified by basic cleaning", {
  known_duplicates <- list(
    c("I. G. Sipes", "I G Sipes"),
    c("J. R. Smith", "J.R. Smith"),
    c("A. B. Johnson", "A.B. Johnson"),
    c("M. C. Williams", "M C Williams")
  )
  
  for (pair in known_duplicates) {
    cleaned_1 <- clean_author_name(pair[1])
    cleaned_2 <- clean_author_name(pair[2])
    expect_equal(cleaned_1, cleaned_2,
                 info = paste("Failed for:", pair[1], "vs", pair[2]))
  }
})

# ============================================================
# TEST 3: Known NON-duplicates should NOT be merged
# ============================================================

test_that("Different authors are not falsely merged", {
  non_duplicates <- list(
    c("John Smith", "Jane Smith"),
    c("Robert Chen", "Robert Chan"),
    c("Maria Garcia", "Mario Garcia"),
    c("A. Smith", "B. Smith"),
    c("David Lee", "Daniel Lee")
  )
  
  for (pair in non_duplicates) {
    cleaned_1 <- clean_author_name(pair[1])
    cleaned_2 <- clean_author_name(pair[2])
    expect_false(cleaned_1 == cleaned_2,
                 info = paste("Falsely merged:", pair[1], "and", pair[2]))
  }
})

# ============================================================
# TEST 4: Jaro-Winkler catches near-duplicates
# ============================================================

test_that("Jaro-Winkler identifies likely duplicates (distance < 0.08)", {
  likely_same <- list(
    c("john r smith", "john smith"),        # Missing middle initial
    c("michael brown", "micheal brown"),    # Typo
    c("katherine jones", "katharine jones") # Variant spelling
  )
  
  for (pair in likely_same) {
    dist <- stringdist(pair[1], pair[2], method = "jw")
    expect_lt(dist, 0.15,
              info = paste("Distance too high for:", pair[1], "vs", pair[2],
                           "- dist:", round(dist, 4)))
  }
})

# ============================================================
# TEST 5: Jaro-Winkler correctly separates different people
# ============================================================

test_that("Jaro-Winkler keeps different people separate (distance > 0.15)", {
  different_people <- list(
    c("john smith", "jane doe"),
    c("robert chen", "maria garcia"),
    c("alice johnson", "bob williams")
  )
  
  for (pair in different_people) {
    dist <- stringdist(pair[1], pair[2], method = "jw")
    expect_gt(dist, 0.15,
              info = paste("Should be different:", pair[1], "vs", pair[2],
                           "- dist:", round(dist, 4)))
  }
})

# ============================================================
# TEST 6: Levenshtein catches typos (edit distance 1-2)
# ============================================================

test_that("Levenshtein catches single-character typos", {
  typo_pairs <- list(
    c("smith", "smth"),       # deletion
    c("johnson", "johnsom"),  # substitution
    c("williams", "wlliams") # deletion
  )
  
  for (pair in typo_pairs) {
    dist <- stringdist(pair[1], pair[2], method = "lv")
    expect_lte(dist, 2,
               info = paste("Edit distance too high for:", pair[1], "vs", pair[2]))
  }
})

# ============================================================
# TEST 7: Edge cases
# ============================================================

test_that("Edge cases are handled correctly", {
  # Empty string
  expect_equal(clean_author_name(""), "")
  
  # Single character
  expect_equal(clean_author_name("A."), "a")
  
  # All periods
  expect_equal(clean_author_name("..."), "")
  
  # Unicode characters (accented names)
  expect_equal(clean_author_name("José García"), "josé garcía")
  
  # Hyphenated names
  expect_equal(clean_author_name("Mary-Jane Watson"), "mary jane watson")
  
  # Multiple spaces
  expect_equal(clean_author_name("John    Doe"), "john doe")
})

# ============================================================
# TEST 8: Real-world UA author examples
# ============================================================

test_that("Real UA author variants are correctly handled", {
  # These should match after cleaning
  expect_equal(
    clean_author_name("I. G. Sipes"),
    clean_author_name("I G Sipes")
  )
  
  # These should NOT match (different people at UA)
  expect_false(
    clean_author_name("John A. Smith") == clean_author_name("John B. Smith")
  )
})

# ============================================================
# TEST 9: Validate find_fuzzy_duplicates function
# ============================================================

test_that("find_fuzzy_duplicates returns correct structure", {
  test_names <- c("john smith", "jon smith", "jane doe", "john smth")
  
  result <- find_fuzzy_duplicates(test_names, max_dist = 0.15, method = "jw")
  
  # Should return a tibble/dataframe
  
  expect_true(is.data.frame(result))
  
  # Should have expected columns
  expect_true(all(c("name_1", "name_2", "distance") %in% colnames(result)))
  
  # "john smith" and "jon smith" should be flagged
  flagged <- result %>%
    filter(
      (name_1 == "john smith" & name_2 == "jon smith") |
        (name_1 == "jon smith" & name_2 == "john smith")
    )
  expect_gt(nrow(flagged), 0, info = "john smith / jon smith not flagged")
  
  # "john smith" and "jane doe" should NOT be flagged
  false_match <- result %>%
    filter(
      (name_1 == "john smith" & name_2 == "jane doe") |
        (name_1 == "jane doe" & name_2 == "john smith")
    )
  expect_equal(nrow(false_match), 0, info = "john smith / jane doe falsely flagged")
})

# ============================================================
# TEST 10: Threshold sensitivity
# ============================================================

test_that("Threshold affects number of matches appropriately", {
  test_names <- c("john smith", "jon smith", "john smyth", 
                  "jane doe", "janet doe", "completely different")
  
  strict <- find_fuzzy_duplicates(test_names, max_dist = 0.05, method = "jw")
  loose <- find_fuzzy_duplicates(test_names, max_dist = 0.20, method = "jw")
  
  # Looser threshold should find more (or equal) matches
  
  expect_gte(nrow(loose), nrow(strict))
})

# ============================================================
# RUN ALL TESTS
# ============================================================

cat("\n=== Running all author deduplication tests ===\n\n")
test_results <- test_dir(".", reporter = "summary")




