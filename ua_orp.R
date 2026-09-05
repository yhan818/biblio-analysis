# ============================================================
# ua_all_works_2025.R
#
# ALL works with UA authorship, 2025 (OA and non-OA)
#
# THREE TARGET FIELDS — dual strategy:
#   1. Request from API via select (they ARE real API fields [1][2])
#   2. Calculate from authorships as fallback / cross-check
#   3. Compare the two (they can differ: authorships is capped
#      at the first 100 authors [1][2])
#
# NOTE ON select: only root-level fields are allowed.
#   select=best_oa_location      -> OK
#   select=best_oa_location.version -> ERROR [5]
# ============================================================

library(openalexR)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

ua_openalex_id <- "I138006243"
yr            <- 2025
date_pulled   <- Sys.time()

# -------------------------------------------------------
# Step 1: Count first (no OA filter — we want EVERYTHING)
# -------------------------------------------------------
count_result <- oa_fetch(
  entity = "works",
  authorships.institutions.id = ua_openalex_id,
  from_publication_date = paste0(yr, "-01-01"),
  to_publication_date   = paste0(yr, "-12-31"),
  count_only = TRUE,
  verbose = FALSE
)
message("Total UA works in ", yr, ": ", count_result$count, "  (expected ~9,700)")

# -------------------------------------------------------
# Step 2: Root-level fields to request
#
# Removed from my earlier draft because they are RETIRED:
#   - grants      -> removed; use funders / awards [1]
#   - type_crossref -> now raw_type [2]
#   - is_paratext -> deprecated; derived from type [1]
# -------------------------------------------------------
fields_to_select <- c(
  "id", "doi", "title", "display_name",
  "publication_year", "publication_date",
  "type", "language",
  
  "open_access",                  # is_oa, oa_status, oa_url,
  # any_repository_has_fulltext [2]
  "best_oa_location",             # target field 1 [1][2]
  "countries_distinct_count",     # target field 2 [1][2]
  "institutions_distinct_count",  # target field 3 [1][2]
  
  "primary_location", "locations", "locations_count",
  "authorships",                  # needed for local calculation
  "corresponding_author_ids", "corresponding_institution_ids",
  "apc_list", "apc_paid",
  "primary_topic", "topics", "keywords",
  "sustainable_development_goals",
  "cited_by_count", "counts_by_year", "fwci",
  "citation_normalized_percentile",
  "referenced_works_count",
  "funders", "awards",            # replaces grants [1]
  "ids", "indexed_in", "is_retracted", "has_fulltext"
)

message("\nFetching ", count_result$count, " works...")

ua_all_works <- oa_fetch(
  entity = "works",
  authorships.institutions.id = ua_openalex_id,
  from_publication_date = paste0(yr, "-01-01"),
  to_publication_date   = paste0(yr, "-12-31"),
  options = oa_options(select = fields_to_select),   # oa_options validates [4]
  verbose = TRUE
)

message("Fetched ", nrow(ua_all_works), " works")
cols <- names(ua_all_works)
print(cols)

# -------------------------------------------------------
# Step 3: Did the three API fields survive works2df()?
# -------------------------------------------------------
have_api_countries    <- "countries_distinct_count"    %in% cols
have_api_institutions <- "institutions_distinct_count" %in% cols
best_oa_cols          <- grep("best_oa", cols, value = TRUE, ignore.case = TRUE)

message("\n-- API field survival check --")
message("  countries_distinct_count    : ", if (have_api_countries) "PRESENT" else "DROPPED by works2df")
message("  institutions_distinct_count : ", if (have_api_institutions) "PRESENT" else "DROPPED by works2df")
message("  best_oa_location            : ",
        if (length(best_oa_cols)) paste(best_oa_cols, collapse = ", ") else "DROPPED by works2df")

# rename API versions so locals don't collide
if (have_api_countries)
  ua_all_works <- rename(ua_all_works, api_countries_distinct_count = countries_distinct_count)
if (have_api_institutions)
  ua_all_works <- rename(ua_all_works, api_institutions_distinct_count = institutions_distinct_count)

# -------------------------------------------------------
# Step 4: Calculate the counts locally from authorships
#
# openalexR may name the list-column `authorships` (when
# requested via select [4]) or `author` (default tibble).
# Institution country / id column names also vary, so we
# detect them rather than hard-code.
# -------------------------------------------------------
auth_col <- if ("authorships" %in% cols) "authorships" else if ("author" %in% cols) "author" else NA

count_distinct_in <- function(df, candidates) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(0L)
  hit <- intersect(candidates, names(df))
  if (!length(hit)) return(NA_integer_)
  v <- df[[hit[1]]]
  v <- v[!is.na(v) & v != ""]
  length(unique(v))
}

if (!is.na(auth_col)) {
  message("\nCalculating counts locally from `", auth_col, "` ...")
  
  ua_all_works <- ua_all_works |>
    mutate(
      calc_countries_distinct_count = map_int(
        .data[[auth_col]],
        ~ count_distinct_in(.x, c("institution_country_code",
                                  "country_code",
                                  "affiliation_country_code"))
      ),
      calc_institutions_distinct_count = map_int(
        .data[[auth_col]],
        ~ count_distinct_in(.x, c("institution_id", "id", "institution_ror"))
      ),
      calc_n_authors = map_int(.data[[auth_col]],
                               ~ if (is.data.frame(.x)) nrow(.x) else 0L)
    )
  
  # inspect nested structure once, so you can confirm column names
  message("Columns inside the authorships tibble (row 1):")
  print(names(ua_all_works[[auth_col]][[1]]))
} else {
  warning("No authorships/author list-column found — cannot calculate locally.")
}

# -------------------------------------------------------
# Step 5 (continued): Compare API vs calculated
# Expect divergence on works with >100 authors, since
# authorships is capped at the first 100 [1][2]
# -------------------------------------------------------
if (have_api_countries && "calc_countries_distinct_count" %in% names(ua_all_works)) {
  
  cmp_countries <- ua_all_works |>
    mutate(diff = api_countries_distinct_count - calc_countries_distinct_count) |>
    count(diff, sort = TRUE) |>
    mutate(pct = round(n / sum(n) * 100, 2))
  
  message("\nAPI minus calculated (countries) — 0 means agreement:")
  print(cmp_countries)
  
  message("\nDisagreements vs. author count (hyperauthorship check):")
  print(
    ua_all_works |>
      filter(api_countries_distinct_count != calc_countries_distinct_count) |>
      summarise(
        n_works              = n(),
        median_authors       = median(calc_n_authors, na.rm = TRUE),
        max_authors          = max(calc_n_authors, na.rm = TRUE),
        n_at_100_author_cap  = sum(calc_n_authors >= 100, na.rm = TRUE),
        pct_at_cap           = round(
          sum(calc_n_authors >= 100, na.rm = TRUE) / n() * 100, 1
        )
      )
  )
  
  # A high pct_at_cap confirms the 100-author truncation
  # is the source of divergence, not a calculation bug [1][2]
}

if (have_api_institutions &&
    "calc_institutions_distinct_count" %in% names(ua_all_works)) {
  
  cmp_institutions <- ua_all_works |>
    mutate(diff = api_institutions_distinct_count - calc_institutions_distinct_count) |>
    count(diff, sort = TRUE) |>
    mutate(pct = round(n / sum(n) * 100, 2))
  
  message("\nAPI minus calculated (institutions) — 0 means agreement:")
  print(cmp_institutions)
}

# -------------------------------------------------------
# Step 5b: Choose the authoritative column
#
# Rule: prefer the API value when present (it is not
# subject to the 100-author cap); fall back to the
# calculated value when the API column was dropped
# or is NA.
# -------------------------------------------------------
ua_all_works <- ua_all_works |>
  mutate(
    countries_distinct_count = if (have_api_countries) {
      coalesce(api_countries_distinct_count, calc_countries_distinct_count)
    } else {
      calc_countries_distinct_count
    },
    institutions_distinct_count = if (have_api_institutions) {
      coalesce(api_institutions_distinct_count, calc_institutions_distinct_count)
    } else {
      calc_institutions_distinct_count
    },
    countries_count_source = if (have_api_countries) "api" else "calculated",
    institutions_count_source = if (have_api_institutions) "api" else "calculated"
  )

message("\nFinal count columns created.")
message("  countries_distinct_count source    : ",
        unique(ua_all_works$countries_count_source))
message("  institutions_distinct_count source : ",
        unique(ua_all_works$institutions_count_source))

# -------------------------------------------------------
# Step 6: best_oa_location — flatten or reconstruct
#
# Case A: works2df() surfaced best_oa_* columns -> use them.
# Case B: dropped -> refetch with output = "list" and
#         extract manually. Only id + best_oa_location are
#         selected, so this second pass is cheap.
# Case C: still unavailable -> derive a best OA location
#         from the `locations` list-column using the
#         documented scoring rules [1][2].
# -------------------------------------------------------

if (length(best_oa_cols) > 0) {
  
  message("\nbest_oa_location present as tibble columns — no extra work needed.")
  print(ua_all_works |> select(all_of(best_oa_cols)) |> head(3))
  
} else {
  
  message("\nbest_oa_location dropped by works2df(); refetching as list...")
  
  boa_list <- oa_fetch(
    entity = "works",
    authorships.institutions.id = ua_openalex_id,
    from_publication_date = paste0(yr, "-01-01"),
    to_publication_date   = paste0(yr, "-12-31"),
    options = oa_options(select = c("id", "best_oa_location")),
    output  = "list",
    verbose = TRUE
  )
  
  message("Retrieved ", length(boa_list), " records in list form.")
  
  # Safe accessor for deeply nested, frequently-null JSON
  pluck_chr <- function(x, ...) {
    v <- purrr::pluck(x, ...)
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
  }
  pluck_lgl <- function(x, ...) {
    v <- purrr::pluck(x, ...)
    if (is.null(v) || length(v) == 0) NA else as.logical(v)[1]
  }
  
  boa_df <- map_dfr(boa_list, function(w) {
    b <- w$best_oa_location            # NULL when the work has no OA copy
    tibble(
      id                        = pluck_chr(w, "id"),
      best_oa_exists            = !is.null(b),
      best_oa_is_oa             = pluck_lgl(b, "is_oa"),
      best_oa_version           = pluck_chr(b, "version"),
      best_oa_license           = pluck_chr(b, "license"),
      best_oa_license_id        = pluck_chr(b, "license_id"),
      best_oa_landing_page_url  = pluck_chr(b, "landing_page_url"),
      best_oa_pdf_url           = pluck_chr(b, "pdf_url"),
      best_oa_is_accepted       = pluck_lgl(b, "is_accepted"),
      best_oa_is_published      = pluck_lgl(b, "is_published"),
      best_oa_source_id         = pluck_chr(b, "source", "id"),
      best_oa_source_name       = pluck_chr(b, "source", "display_name"),
      best_oa_source_type       = pluck_chr(b, "source", "type"),
      best_oa_source_is_in_doaj = pluck_lgl(b, "source", "is_in_doaj"),
      best_oa_host_org_name     = pluck_chr(b, "source", "host_organization_name")
    )
  })
  
  message("Extracted best_oa_location for ", nrow(boa_df), " works.")
  message("  Works with a best OA location : ", sum(boa_df$best_oa_exists))
  message("  Works with none (closed)      : ", sum(!boa_df$best_oa_exists))
  
  # Join back on id
  ua_all_works <- left_join(ua_all_works, boa_df, by = "id")
  
  # Sanity check: rows should not have multiplied
  stopifnot(nrow(ua_all_works) == nrow(boa_df) ||
              nrow(ua_all_works) == count_result$count)
}

# Derive a simple publisher-vs-repository indicator
if ("best_oa_source_type" %in% names(ua_all_works)) {
  ua_all_works <- ua_all_works |>
    mutate(
      best_oa_venue_class = case_when(
        is.na(best_oa_source_type)              ~ "none",
        best_oa_source_type == "repository"     ~ "repository",
        best_oa_source_type %in% c("journal", "conference",
                                   "book series", "ebook platform")
        ~ "publisher",
        TRUE                                    ~ "other"
      )
    )
}

# -------------------------------------------------------
# Step 7: Summary statistics — the actual point of the pull
# -------------------------------------------------------
message("\n", strrep("=", 60))
message("SUMMARY STATISTICS")
message(strrep("=", 60))

total_works <- nrow(ua_all_works)

# --- 7a: Overall OA rate ---
oa_summary <- ua_all_works |>
  count(oa_status, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1))

overall_oa_rate <- round(
  sum(oa_summary$n[oa_summary$oa_status != "closed"]) / total_works * 100, 1
)

message("\nOA status, ALL work types (n = ", total_works, "):")
print(oa_summary)
message("OVERALL OA RATE: ", overall_oa_rate, "%")

# --- 7b: Work types ---
type_summary <- ua_all_works |>
  count(type, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1))

message("\nWork types:")
print(type_summary)

# --- 7c: OA rate BY work type — the headline table ---
oa_by_type <- ua_all_works |>
  group_by(type) |>
  summarise(
    n_works = n(),
    n_oa    = sum(oa_status != "closed", na.rm = TRUE),
    oa_rate = round(n_oa / n_works * 100, 1),
    n_gold    = sum(oa_status == "gold",   na.rm = TRUE),
    n_hybrid  = sum(oa_status == "hybrid", na.rm = TRUE),
    n_green   = sum(oa_status == "green",  na.rm = TRUE),
    n_bronze  = sum(oa_status == "bronze", na.rm = TRUE),
    n_diamond = sum(oa_status == "diamond", na.rm = TRUE),
    n_closed  = sum(oa_status == "closed", na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(n_works))

message("\nOA rate by work type:")
print(oa_by_type)



# =======================================================
# Step 7d PREREQUISITE — identify or construct the
# discipline column. Run this BEFORE the 7d block.
# =======================================================

# --- 1. Does a flattened field column already exist? ---
field_col <- intersect(
  c("topic_field", "primary_topic_field", "field",
    "field_display_name", "primary_topic.field.display_name"),
  names(ua_all_works)
)

message("Flattened field column(s) found: ",
        if (length(field_col)) paste(field_col, collapse = ", ") else "none")

# --- 2. Inspect the topics list-column structure ---
# openalexR returns topics as a nested tibble; the webinar's
# "journal clocks" example unnests it and filters name == "field",
# which tells us the nested frame has `name` and `display_name`
# columns where name is one of topic/subfield/field/domain [1]
if ("topics" %in% names(ua_all_works)) {
  idx <- which(vapply(
    ua_all_works$topics,
    function(x) is.data.frame(x) && nrow(x) > 0,
    logical(1)
  ))[1]
  
  if (!is.na(idx)) {
    message("\nStructure of the `topics` nested tibble (row ", idx, "):")
    print(names(ua_all_works$topics[[idx]]))
    print(head(ua_all_works$topics[[idx]], 12))
  } else {
    message("\n`topics` exists but every element is empty/NULL.")
  }
} else {
  message("\nNo `topics` column present. Check that \"topics\" was included ",
          "in fields_to_select, or fall back to the `concepts` column.")
}

# --- 3. Build topic_field from the topics list-column ---
if (length(field_col) == 0 && "topics" %in% names(ua_all_works)) {
  
  get_field <- function(tp) {
    if (!is.data.frame(tp) || nrow(tp) == 0) return(NA_character_)
    
    # If there is a topic-rank index, keep only the primary topic
    if ("i" %in% names(tp)) {
      keep <- suppressWarnings(as.integer(tp$i))
      if (any(!is.na(keep))) tp <- tp[which(keep == min(keep, na.rm = TRUE)), , drop = FALSE]
    }
    
    # Long shape: name = "field" / "subfield" / "domain" / "topic" [1]
    if (all(c("name", "display_name") %in% names(tp))) {
      hit <- tp$display_name[tp$name == "field"]
      hit <- hit[!is.na(hit) & hit != ""]
      if (length(hit)) return(as.character(hit[1]))
    }
    
    # Wide shape fallbacks
    for (cand in c("field_display_name", "field.display_name", "field")) {
      if (cand %in% names(tp)) {
        v <- tp[[cand]]
        v <- v[!is.na(v) & v != ""]
        if (length(v)) return(as.character(v[1]))
      }
    }
    
    NA_character_
  }
  
  ua_all_works$topic_field <- vapply(ua_all_works$topics, get_field, character(1))
  field_col <- "topic_field"
  
  n_ok <- sum(!is.na(ua_all_works$topic_field))
  message("\nBuilt `topic_field`: ", n_ok, " of ", nrow(ua_all_works),
          " works assigned (",
          round(n_ok / nrow(ua_all_works) * 100, 1), "%)")
  
  if (n_ok > 0) {
    message("\nTop fields:")
    print(
      ua_all_works |>
        dplyr::count(topic_field, sort = TRUE) |>
        head(15)
    )
  } else {
    message("\nNo fields extracted — inspect the printed structure above ",
            "and adjust get_field() to match the actual column names.")
  }
}

# --- 4. Guard so Step 7d cannot error again ---
if (!exists("field_col")) field_col <- character(0)

message("\nfield_col is now: ",
        if (length(field_col)) field_col[1] else "empty (7d will be skipped)")


# -------------------------------------------------------
# Step 7d (continued): OA rate by discipline
# -------------------------------------------------------
if (length(field_col) > 0) {
  fc <- field_col[1]
  message("\nUsing '", fc, "' as the discipline column.")
  
  oa_by_field <- ua_all_works |>
    filter(!is.na(.data[[fc]])) |>
    group_by(discipline = .data[[fc]]) |>
    summarise(
      n_works   = n(),
      n_oa      = sum(oa_status != "closed", na.rm = TRUE),
      oa_rate   = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
      n_gold    = sum(oa_status == "gold",    na.rm = TRUE),
      n_diamond = sum(oa_status == "diamond", na.rm = TRUE),
      n_hybrid  = sum(oa_status == "hybrid",  na.rm = TRUE),
      n_green   = sum(oa_status == "green",   na.rm = TRUE),
      n_bronze  = sum(oa_status == "bronze",  na.rm = TRUE),
      n_closed  = sum(oa_status == "closed",  na.rm = TRUE),
      .groups   = "drop"
    ) |>
    arrange(desc(n_works))
  
  message("\nOA rate by discipline (all work types):")
  print(oa_by_field, n = 30)
  
  # --- Articles only: the cleanest cross-discipline comparison ---
  oa_by_field_articles <- ua_all_works |>
    filter(type == "article", !is.na(.data[[fc]])) |>
    group_by(discipline = .data[[fc]]) |>
    summarise(
      n_articles = n(),
      n_oa       = sum(oa_status != "closed", na.rm = TRUE),
      oa_rate    = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
      .groups    = "drop"
    ) |>
    arrange(desc(oa_rate))
  
  message("\nARTICLE OA rate by discipline (ranked):")
  print(oa_by_field_articles, n = 30)
  
  # --- Work type mix within each discipline ---
  type_by_field <- ua_all_works |>
    filter(!is.na(.data[[fc]])) |>
    count(discipline = .data[[fc]], type) |>
    group_by(discipline) |>
    mutate(pct_within_discipline = round(n / sum(n) * 100, 1)) |>
    ungroup() |>
    arrange(discipline, desc(n))
  
  message("\nWork type mix by discipline (first 25 rows):")
  print(type_by_field, n = 25)
  
} else {
  message("\nNo discipline/field column detected. Inspect names(ua_all_works) ",
          "and check the primary_topic / topics list-column structure.")
  oa_by_field          <- NULL
  oa_by_field_articles <- NULL
  type_by_field        <- NULL
}

# -------------------------------------------------------
# Step 7e: Articles-only headline numbers
# This is the number you originally asked for
# -------------------------------------------------------
articles <- ua_all_works |> filter(type == "article")
n_articles <- nrow(articles)

articles_oa_summary <- articles |>
  count(oa_status, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1))

article_oa_rate <- round(
  sum(articles_oa_summary$n[articles_oa_summary$oa_status != "closed"]) /
    n_articles * 100, 1
)

message("\n", strrep("-", 60))
message("HEADLINE: UA ARTICLES, ", yr)
message(strrep("-", 60))
message("  Total articles : ", n_articles)
message("  OA articles    : ",
        sum(articles_oa_summary$n[articles_oa_summary$oa_status != "closed"]))
message("  ARTICLE OA RATE: ", article_oa_rate, "%")
print(articles_oa_summary)

# -------------------------------------------------------
# Step 7f: Collaboration breadth vs. OA
# Uses the countries / institutions counts from Step 5b
# -------------------------------------------------------
oa_by_collaboration <- ua_all_works |>
  mutate(
    collab_class = case_when(
      is.na(countries_distinct_count)  ~ "unknown",
      countries_distinct_count <= 1    ~ "1 country (domestic)",
      countries_distinct_count == 2    ~ "2 countries",
      countries_distinct_count <= 5    ~ "3-5 countries",
      countries_distinct_count <= 10   ~ "6-10 countries",
      TRUE                             ~ "11+ countries"
    ),
    collab_class = factor(
      collab_class,
      levels = c("1 country (domestic)", "2 countries", "3-5 countries",
                 "6-10 countries", "11+ countries", "unknown")
    )
  ) |>
  group_by(collab_class) |>
  summarise(
    n_works      = n(),
    n_oa         = sum(oa_status != "closed", na.rm = TRUE),
    oa_rate      = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
    median_insts = median(institutions_distinct_count, na.rm = TRUE),
    .groups      = "drop"
  )

message("\nOA rate by international collaboration breadth:")
print(oa_by_collaboration)

# -------------------------------------------------------
# Step 7g: Where do the OA copies actually live?
# Requires the best_oa_* columns from Step 6
# -------------------------------------------------------
if ("best_oa_venue_class" %in% names(ua_all_works)) {
  
  best_oa_venue_summary <- ua_all_works |>
    count(best_oa_venue_class, sort = TRUE) |>
    mutate(pct_of_all_works = round(n / sum(n) * 100, 1))
  
  message("\nBest OA location: publisher vs. repository:")
  print(best_oa_venue_summary)
  
  best_oa_version_summary <- ua_all_works |>
    filter(!is.na(best_oa_version)) |>
    count(best_oa_version, best_oa_venue_class, sort = TRUE) |>
    mutate(pct_of_oa = round(n / sum(n) * 100, 1))
  
  message("\nBest OA location: version x venue:")
  print(best_oa_version_summary)
  
  best_oa_license_summary <- ua_all_works |>
    filter(!is.na(best_oa_license)) |>
    count(best_oa_license, sort = TRUE) |>
    mutate(pct_of_licensed = round(n / sum(n) * 100, 1))
  
  message("\nBest OA location: licenses:")
  print(best_oa_license_summary, n = 20)
  
  best_oa_source_summary <- ua_all_works |>
    filter(!is.na(best_oa_source_name)) |>
    count(best_oa_source_name, best_oa_source_type, sort = TRUE) |>
    head(40)
  
  message("\nTop 40 best-OA sources:")
  print(best_oa_source_summary, n = 40)
  
} else {
  best_oa_venue_summary   <- NULL
  best_oa_version_summary <- NULL
  best_oa_license_summary <- NULL
  best_oa_source_summary  <- NULL
  message("\nNo best_oa_* columns available; skipping Step 7g.")
}

# -------------------------------------------------------
# Step 8: Prepare data for flat-file export
#
# CSV and XLSX cannot hold list-columns, so:
#   8a. Save the full object (list-columns intact) as .rds
#   8b. Write a long-format authorship table
#   8c. Collapse remaining list-columns to delimited strings
# -------------------------------------------------------

# --- 8a: Lossless R-native save ---
rds_file <- paste0("UA_ALL_works_", yr, ".rds")
saveRDS(ua_all_works, rds_file)
message("\nSaved lossless copy (list-columns intact): ", rds_file)

# --- 8b: Long-format authorship table ---
if (!is.na(auth_col)) {
  authorships_long <- ua_all_works |>
    select(id, doi, publication_date, type, oa_status,
           all_of(auth_col)) |>
    tidyr::unnest(all_of(auth_col), names_sep = "_", keep_empty = TRUE)
  
  write_csv(authorships_long,
            paste0("UA_ALL_works_", yr, "_authorships_long.csv"))
  message("Saved authorship detail: UA_ALL_works_", yr,
          "_authorships_long.csv (", nrow(authorships_long), " rows)")
} else {
  authorships_long <- NULL
}

# -------------------------------------------------------
# Step 8c (continued): Flatten list-columns for the wide table
#
# CSV and XLSX cells hold only scalars, so every list-column
# has to be collapsed to a single delimited string. The helper
# below handles the three shapes openalexR produces:
#   - atomic vectors  (e.g. c("a","b"))
#   - plain lists     (e.g. list("a","b"))
#   - nested tibbles  (e.g. the topics / authorships frames)
# -------------------------------------------------------
collapse_list_col <- function(v, sep = "; ", max_chars = 30000) {
  if (is.null(v) || length(v) == 0) return(NA_character_)
  
  out <- if (is.data.frame(v)) {
    # Nested tibble: prefer a human-readable name column,
    # otherwise fall back to the first column present.
    name_cols <- intersect(
      c("display_name", "name", "title", "id", "award_id"),
      names(v)
    )
    if (length(name_cols) > 0) {
      paste(unique(stats::na.omit(as.character(v[[name_cols[1]]]))),
            collapse = sep)
    } else if (ncol(v) > 0) {
      paste(unique(stats::na.omit(as.character(v[[1]]))), collapse = sep)
    } else {
      NA_character_
    }
  } else if (is.list(v)) {
    paste(unique(stats::na.omit(unlist(v, use.names = FALSE))), collapse = sep)
  } else {
    paste(unique(stats::na.omit(as.character(v))), collapse = sep)
  }
  
  if (is.na(out) || out == "") return(NA_character_)
  
  # Excel hard-caps a cell at 32,767 characters
  if (nchar(out) > max_chars) {
    out <- paste0(substr(out, 1, max_chars), " ...[TRUNCATED]")
  }
  out
}

# Identify every list-column still present
list_cols <- names(ua_all_works)[
  vapply(ua_all_works, function(x) is.list(x), logical(1))
]

message("\nList-columns to collapse (", length(list_cols), "): ",
        if (length(list_cols)) paste(list_cols, collapse = ", ") else "none")

ua_all_works_flat <- ua_all_works

for (lc in list_cols) {
  ua_all_works_flat[[lc]] <- vapply(
    ua_all_works_flat[[lc]],
    collapse_list_col,
    character(1)
  )
}

# Confirm the frame is now fully rectangular
stopifnot(!any(vapply(ua_all_works_flat, is.list, logical(1))))
message("Flattened frame: ", nrow(ua_all_works_flat), " rows x ",
        ncol(ua_all_works_flat), " columns")

# Put the analytically important columns first
front_cols <- intersect(
  c("id", "doi", "title", "publication_date", "publication_year",
    "type", "language",
    "is_oa", "oa_status", "oa_url", "any_repository_has_fulltext",
    "best_oa_exists", "best_oa_venue_class", "best_oa_version",
    "best_oa_license", "best_oa_source_name", "best_oa_source_type",
    "best_oa_pdf_url", "best_oa_landing_page_url",
    "countries_distinct_count", "institutions_distinct_count",
    "countries_count_source", "institutions_count_source",
    "calc_n_authors", "cited_by_count", "fwci",
    "so", "source_display_name", "publisher", "issn_l"),
  names(ua_all_works_flat)
)

ua_all_works_flat <- ua_all_works_flat |>
  select(all_of(front_cols), everything())

# -------------------------------------------------------
# Step 9: Provenance / data-dictionary tables
# -------------------------------------------------------
run_metadata <- tibble::tibble(
  field = c(
    "date_pulled", "time_pulled_local", "timezone",
    "institution_openalex_id", "institution_name",
    "publication_year", "date_filter",
    "oa_filter_applied",
    "expected_count_from_api", "rows_returned",
    "counts_source_countries", "counts_source_institutions",
    "best_oa_location_method",
    "discipline_column_used",
    "openalexR_version", "R_version", "platform",
    "script_name", "notes"
  ),
  value = c(
    format(as.Date(date_pulled), "%Y-%m-%d"),
    format(date_pulled, "%Y-%m-%d %H:%M:%S"),
    Sys.timezone(),
    ua_openalex_id, "University of Arizona",
    as.character(yr),
    paste0(yr, "-01-01 to ", yr, "-12-31"),
    "NONE - all works returned, OA and closed",
    as.character(count_result$count),
    as.character(nrow(ua_all_works)),
    unique(ua_all_works$countries_count_source),
    unique(ua_all_works$institutions_count_source),
    if (length(best_oa_cols) > 0) "tibble columns from oa_fetch" else
      "second pass with output='list', manual extraction",
    if (length(field_col) > 0) field_col[1] else "none detected",
    as.character(utils::packageVersion("openalexR")),
    R.version.string,
    R.version$platform,
    "ua_all_works_2025.R",
    paste0("Full-universe pull for OA rate analysis. Denominator = all ",
           "UA-affiliated works, so OA percentages are valid. ",
           "Note: OpenAlex authorships are capped at the first 100 authors, ",
           "so locally calculated distinct counts can undercount on ",
           "hyperauthorship papers.")
  )
)

message("\nRun metadata:")
print(run_metadata, n = Inf)

# Row-count reconciliation, so discrepancies are visible later
reconciliation <- tibble::tibble(
  check = c("API count_only", "Rows fetched", "Difference",
            "Unique OpenAlex IDs", "Duplicate IDs"),
  value = c(
    count_result$count,
    nrow(ua_all_works),
    count_result$count - nrow(ua_all_works),
    dplyr::n_distinct(ua_all_works$id),
    nrow(ua_all_works) - dplyr::n_distinct(ua_all_works$id)
  )
)

message("\nReconciliation:")
print(reconciliation)

if (reconciliation$value[3] != 0) {
  warning("Fetched row count does not match the API count. ",
          "Check for pagination limits or works added/removed mid-pull.")
}

# Column inventory doubles as a data dictionary
column_inventory <- tibble::tibble(
  column    = names(ua_all_works_flat),
  class     = vapply(ua_all_works_flat, function(x) class(x)[1], character(1)),
  n_missing = vapply(ua_all_works_flat, function(x) sum(is.na(x)), integer(1))
) |>
  mutate(pct_missing = round(n_missing / nrow(ua_all_works_flat) * 100, 1))

# -------------------------------------------------------
# Step 10: Write CSVs
# -------------------------------------------------------
stamp    <- format(as.Date(date_pulled), "%Y%m%d")
prefix   <- paste0("UA_ALL_works_", yr, "_", stamp)

csv_targets <- list(
  main                 = ua_all_works_flat,
  metadata             = run_metadata,
  reconciliation       = reconciliation,
  column_inventory     = column_inventory,
  oa_summary           = oa_summary,
  type_summary         = type_summary,
  oa_by_type           = oa_by_type,
  articles_oa_summary  = articles_oa_summary,
  oa_by_collaboration  = oa_by_collaboration,
  oa_by_field          = oa_by_field,
  oa_by_field_articles = oa_by_field_articles,
  type_by_field        = type_by_field,
  best_oa_venue        = best_oa_venue_summary,
  best_oa_version      = best_oa_version_summary,
  best_oa_license      = best_oa_license_summary,
  best_oa_sources      = best_oa_source_summary
)

# Drop anything that was skipped upstream
csv_targets <- csv_targets[!vapply(csv_targets, is.null, logical(1))]

for (nm in names(csv_targets)) {
  f <- paste0(prefix, "_", nm, ".csv")
  readr::write_csv(csv_targets[[nm]], f, na = "")
  message("  wrote ", f, "  (", nrow(csv_targets[[nm]]), " rows)")
}

# =======================================================
# Step 11 PREREQUISITE — define stamp / prefix / xlsx_file
# and build the named list of sheets.
# Run this BEFORE the Step 11 writexl/openxlsx block.
# =======================================================

# --- 1. File naming (re-create if Step 10 wasn't run in this session) ---
if (!exists("date_pulled")) date_pulled <- Sys.time()
if (!exists("yr"))          yr <- 2025

stamp  <- format(as.Date(date_pulled), "%Y%m%d")
prefix <- paste0("UA_ALL_works_", yr, "_", stamp)

xlsx_file <- paste0(prefix, ".xlsx")

# --- 2. Safe fetch: returns NULL if the object was never created ---
pick <- function(nm) {
  if (exists(nm, inherits = TRUE)) {
    obj <- get(nm, inherits = TRUE)
    if (is.data.frame(obj) && nrow(obj) > 0) return(obj)
  }
  NULL
}

# --- 3. Build the sheet list.
# Provenance first, so the date pulled is the first thing
# anyone sees when they open the workbook.
sheets <- list(
  "README_date_pulled"   = pick("run_metadata"),
  "reconciliation"       = pick("reconciliation"),
  "all_works"            = pick("ua_all_works_flat"),
  "oa_summary"           = pick("oa_summary"),
  "type_summary"         = pick("type_summary"),
  "oa_by_type"           = pick("oa_by_type"),
  "articles_oa"          = pick("articles_oa_summary"),
  "oa_by_collaboration"  = pick("oa_by_collaboration"),
  "oa_by_field"          = pick("oa_by_field"),
  "oa_by_field_articles" = pick("oa_by_field_articles"),
  "oa_by_domain"         = pick("oa_by_domain"),
  "type_by_field"        = pick("type_by_field"),
  "best_oa_venue"        = pick("best_oa_venue_summary"),
  "best_oa_version"      = pick("best_oa_version_summary"),
  "best_oa_license"      = pick("best_oa_license_summary"),
  "best_oa_sources"      = pick("best_oa_source_summary"),
  "column_inventory"     = pick("column_inventory")
)

# Drop anything that wasn't created upstream
missing_sheets <- names(sheets)[vapply(sheets, is.null, logical(1))]
sheets <- sheets[!vapply(sheets, is.null, logical(1))]

if (length(missing_sheets)) {
  message("Skipped (object not found or empty): ",
          paste(missing_sheets, collapse = ", "))
}

# --- 4. Sanitise sheet names for Excel ---
# Worksheet names: max 31 characters, and Excel forbids : \ / ? * [ ]
clean_sheet_name <- function(x) {
  x <- gsub("[:\\\\/?*\\[\\]]", "_", x)
  x <- substr(x, 1, 31)
  x
}
names(sheets) <- clean_sheet_name(names(sheets))
names(sheets) <- make.unique(names(sheets), sep = "_")

# --- 5. Guard: don't attempt to write an empty workbook ---
stopifnot(length(sheets) > 0)

message("\nWorkbook target : ", xlsx_file)
message("Sheets to write : ", length(sheets))
print(data.frame(
  sheet = names(sheets),
  rows  = vapply(sheets, nrow, integer(1)),
  cols  = vapply(sheets, ncol, integer(1)),
  row.names = NULL
))

# -------------------------------------------------------
# Step 11 (continued): Write a single multi-sheet XLSX
#
# Two engines, in order of preference:
#   1. writexl  - no Java, no dependencies, accepts a named
#      list of data frames to create one sheet per element [4]
#   2. openxlsx - fallback; build the workbook sheet by sheet
#      with createWorkbook() / addWorksheet() / writeData(),
#      then saveWorkbook(). This route also allows styling,
#      frozen header rows, and column widths [2]
# -------------------------------------------------------
if (requireNamespace("writexl", quietly = TRUE)) {
  
  writexl::write_xlsx(sheets, path = xlsx_file, format_headers = TRUE)
  message("\nWrote workbook via writexl: ", xlsx_file,
          "  (", length(sheets), " sheets)")
  
} else if (requireNamespace("openxlsx", quietly = TRUE)) {
  
  message("\n'writexl' not installed; falling back to 'openxlsx'.")
  
  wb <- openxlsx::createWorkbook()
  
  header_style <- openxlsx::createStyle(
    fontColour     = "#FFFFFF",
    fgFill         = "#1E4D2B",   # UA-ish dark green
    halign         = "left",
    valign         = "center",
    textDecoration = "bold",
    border         = "Bottom",
    wrapText       = TRUE
  )
  
  for (nm in names(sheets)) {
    dat <- sheets[[nm]]
    
    openxlsx::addWorksheet(wb, sheetName = nm)
    openxlsx::writeData(wb, sheet = nm, x = dat,
                        startRow = 1, startCol = 1,
                        withFilter = TRUE)
    
    # Style the header row
    openxlsx::addStyle(
      wb, sheet = nm, style = header_style,
      rows = 1, cols = seq_len(ncol(dat)), gridExpand = TRUE
    )
    
    # Freeze the header so it stays visible when scrolling 9,700 rows
    openxlsx::freezePane(wb, sheet = nm, firstRow = TRUE)
    
    # Reasonable column widths without letting long URLs explode the sheet
    openxlsx::setColWidths(
      wb, sheet = nm, cols = seq_len(ncol(dat)),
      widths = "auto", ignoreMergedCells = TRUE
    )
  }
  
  openxlsx::saveWorkbook(wb, file = xlsx_file, overwrite = TRUE)
  message("Wrote workbook via openxlsx: ", xlsx_file,
          "  (", length(sheets), " sheets)")
  
} else {
  
  warning(
    "Neither 'writexl' nor 'openxlsx' is installed, so no XLSX was written.\n",
    "Install one of them and re-run only Step 11:\n",
    "  install.packages(\"writexl\")   # lightweight, no Java\n",
    "  install.packages(\"openxlsx\")  # adds styling / formatting\n",
    "The CSVs from Step 10 already contain everything."
  )
  xlsx_file <- NA_character_
  
}

# -------------------------------------------------------
# Step 12: Provenance - session info and a plain-text run log
#
# The metadata sheet records WHEN the data was pulled;
# this records WITH WHAT it was pulled, so the run can be
# reconstructed later [9].
# -------------------------------------------------------
session_txt <- paste0(prefix, "_sessionInfo.txt")

session_lines <- c(
  "UA ALL WORKS 2025 - session information",
  paste0("Written: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ",
         Sys.timezone()),
  strrep("-", 60),
  ""
)

# sessioninfo::session_info() is more informative than utils::sessionInfo()
# because it reports the source of each package (CRAN, GitHub, local) [9]
if (requireNamespace("sessioninfo", quietly = TRUE)) {
  session_lines <- c(
    session_lines,
    capture.output(sessioninfo::session_info())
  )
} else {
  session_lines <- c(
    session_lines,
    capture.output(utils::sessionInfo()),
    "",
    "NOTE: install.packages('sessioninfo') for richer provenance output."
  )
}

writeLines(session_lines, con = session_txt)
message("Wrote session info: ", session_txt)

# --- Plain-text run log: the key numbers, in one place ---
log_txt <- paste0(prefix, "_runlog.txt")

log_lines <- c(
  "UA ALL WORKS 2025 - run log",
  strrep("=", 60),
  paste0("Date pulled          : ", format(as.Date(date_pulled), "%Y-%m-%d")),
  paste0("Institution          : University of Arizona (", ua_openalex_id, ")"),
  paste0("Publication window   : ", yr, "-01-01 to ", yr, "-12-31"),
  paste0("OA filter applied    : NONE (all works, OA and closed)"),
  paste0("API count_only       : ", count_result$count),
  paste0("Rows fetched         : ", nrow(ua_all_works)),
  paste0("Unique OpenAlex IDs  : ", dplyr::n_distinct(ua_all_works$id)),
  "",
  paste0("OVERALL OA RATE      : ", overall_oa_rate, "%"),
  paste0("Total articles       : ", n_articles),
  paste0("ARTICLE OA RATE      : ", article_oa_rate, "%"),
  "",
  paste0("countries count from    : ",
         unique(ua_all_works$countries_count_source)),
  paste0("institutions count from : ",
         unique(ua_all_works$institutions_count_source)),
  paste0("best_oa_location via    : ",
         if (length(best_oa_cols) > 0) "oa_fetch tibble columns" else
           "second pass, output='list', manual extraction"),
  "",
  "Files written:",
  paste0("  - ", rds_file),
  if (!is.na(xlsx_file)) paste0("  - ", xlsx_file) else
    "  - (no XLSX: writexl/openxlsx unavailable)",
  paste0("  - ", session_txt),
  paste0("  - ", prefix, "_*.csv  (", length(csv_targets), " files)"),
  if (!is.null(authorships_long))
    paste0("  - UA_ALL_works_", yr, "_authorships_long.csv") else NULL,
  "",
  "Caveats:",
  "  * OpenAlex caps authorships at the first 100 authors, so locally",
  "    calculated distinct country/institution counts can undercount on",
  "    hyperauthorship papers. Prefer the API values where present.",
  "  * best_oa_location is NULL for closed works by design; a missing",
  "    value there is informative, not an error.",
  "  * 2025 is a partial/recent year: indexing and OA status (especially",
  "    green OA via repositories) will continue to accrue after this pull,",
  "    so the OA rate is a floor, not a final figure."
)

writeLines(log_lines, con = log_txt)
message("Wrote run log: ", log_txt)

# -------------------------------------------------------
# Step 13: Final console summary
# -------------------------------------------------------
message("\n", strrep("=", 60))
message("DONE - UA ALL WORKS ", yr)
message(strrep("=", 60))
message("Works fetched     : ", nrow(ua_all_works))
message("Overall OA rate   : ", overall_oa_rate, "%")
message("Article OA rate   : ", article_oa_rate, "%  (n = ", n_articles, ")")
message("Workbook          : ",
        if (!is.na(xlsx_file)) xlsx_file else "not written")
message("Lossless R object : ", rds_file)
message("\nTo reload the full object with list-columns intact:")
message("  ua_all_works <- readRDS(\"", rds_file, "\")")


