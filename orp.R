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

write.csv(top_funders, "UA_top_funders.csv", row.names = FALSE)
write.csv(top_funders_awards, "UA_top_funders_with_awards.csv", row.names = FALSE)
write.csv(impact_by_funder, "UA_impact_by_funder.csv", row.names = FALSE)
write.csv(impact_by_funding, "UA_funded_vs_not.csv", row.names = FALSE)
write.csv(funding_by_collab, "UA_funding_by_collaboration.csv", row.names = FALSE)
write.csv(funder_type_summary, "UA_federal_vs_other.csv", row.names = FALSE)
write.csv(federal_agency_summary, "UA_federal_agency_breakdown.csv", row.names = FALSE)
write.csv(impact_by_funder_type, "UA_impact_by_funder_type.csv", row.names = FALSE)
write.csv(impact_by_agency, "UA_impact_by_federal_agency.csv", row.names = FALSE)
write.csv(as.data.frame(grants_summary), "UA_grants_summary.csv", row.names = FALSE)




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

write.csv(topics_extracted, "UA_topics_extracted.csv", row.names = FALSE)
write.csv(domain_summary, "UA_domain_summary.csv", row.names = FALSE)
write.csv(field_summary, "UA_field_summary.csv", row.names = FALSE)
write.csv(subfield_summary, "UA_subfield_summary.csv", row.names = FALSE)
write.csv(topic_summary, "UA_topic_summary.csv", row.names = FALSE)
write.csv(impact_by_domain, "UA_impact_by_domain.csv", row.names = FALSE)
write.csv(impact_by_field, "UA_impact_by_field.csv", row.names = FALSE)
write.csv(funding_by_domain, "UA_funding_by_domain.csv", row.names = FALSE)
write.csv(funding_by_field, "UA_funding_by_field.csv", row.names = FALSE)
write.csv(collab_by_domain_wide, "UA_collaboration_by_domain.csv", row.names = FALSE)
write.csv(top_funders_by_domain, "UA_top_funders_by_domain.csv", row.names = FALSE)
write.csv(as.data.frame(discipline_summary), "UA_discipline_summary.csv", row.names = FALSE)





