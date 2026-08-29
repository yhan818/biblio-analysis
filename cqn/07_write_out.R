## ============================================================
## CQN — Step 7: codebook, notes, and write-out
## Requires in the environment: cqn_wide, awards_long, requested
## ============================================================

stopifnot(exists("cqn_wide"), exists("awards_long"), exists("requested"))
for (p in c("readr", "writexl", "readxl")) {
  if (!requireNamespace(p, quietly = TRUE)) stop("Please install: ", p)
}

## ---- 7a. Auto-generated stats: one row per actual column ----
field_stats <- tibble::tibble(
  position = seq_along(cqn_wide),
  field    = names(cqn_wide),
  r_class  = vapply(cqn_wide, function(x) class(x)[1], character(1)),
  n_filled = vapply(cqn_wide, function(x) sum(!is.na(x)), integer(1)),
  fill_pct = round(100 * vapply(cqn_wide, function(x) mean(!is.na(x)), numeric(1)), 1),
  n_unique = vapply(cqn_wide, function(x) length(unique(x[!is.na(x)])), integer(1)),
  example  = vapply(cqn_wide, function(x) {
    v <- x[!is.na(x)]
    if (!length(v)) return(NA_character_)
    substr(as.character(v[1]), 1, 60)
  }, character(1))
)

## ---- 7b. Hand-written definitions ----
defs <- tibble::tribble(
  ~field, ~definition, ~notes,
  
  ## -- reconciliation / provenance
  "requested_work_id", "OpenAlex Work ID as supplied in the source workbook.", "Primary key. 1,194 unique IDs.",
  "returned_openalex_id", "Work ID returned by the API.", "Differs from requested only if a record was merged; no merges found here.",
  "fetch_status", "ok / not_found.", "All 1,194 = ok.",
  "fetch_method", "How the record was retrieved.", "list_filter = batch query; singleton_xpac = fetched individually.",
  "is_xpac", "TRUE = expansion-corpus record.", "2 works. Thinner metadata; excluded from OpenAlex queries by default.",
  
  ## -- YOUR FIVE ADDED FIELDS
  "countries_distinct_count", "OpenAlex's count of distinct author countries.", "Authoritative; use this over the *_computed version.",
  "institutions_distinct_count", "OpenAlex's count of distinct author institutions.", "Exceeds locally recomputed value on 12 works.",
  "authorships_countries", "ISO country code(s) per author.", "' | ' separates authors (order preserved); ',' separates multiple countries for one author; empty slot = no country.",
  "funder_display_names", "Funder names for the work.", "Requested as grants.funder_display_name. Union of awards[].funder_display_name and funders[]. '; ' delimited, deduplicated.",
  "award_ids", "Funder's own grant/award numbers.", "Requested as grants.award_id; sourced from awards[].funder_award_id. ' | ' delimited because 13 works have grant IDs containing ';'.",
  
  ## -- YOUR TWO USER-FRIENDLY FIELDS
  "author_names_clean", "All author names, submission order preserved.", "'; ' delimited. No OpenAlex IDs, no affiliations. Not limited to UA.",
  "institution_names_clean", "Distinct institution names for the work.", "'; ' delimited, deduplicated. Names only, no OpenAlex IDs.",
  
  ## -- supporting author/geography fields
  "authorships_institutions", "Institutions per author, positionally aligned.", "' | ' between authors; ' + ' within one author (institution names contain commas).",
  "authorships_countries_src", "Whether countries came from authorship.countries or institution.country_code.", "Provenance flag.",
  "distinct_countries_list", "Deduplicated list of country codes.", "'; ' delimited.",
  "countries_distinct_computed", "Country count recomputed locally.", "Cross-check against OpenAlex's count.",
  "institutions_distinct_computed", "Institution count recomputed locally.", "Lower than OpenAlex's on 12 works.",
  "authors_count", "Number of authorships on the record.", "Capped at 100 by the API.",
  "authorships_truncated_flag", "TRUE if authors_count >= 100.", "List fields are truncated on these works.",
  "corresponding_author_names", "Authors flagged is_corresponding.", "Often empty; upstream metadata is inconsistent.",
  
  ## -- funding
  "funding_provenance", "award_with_grant_id / funder_only / none.", "790 / 61 / 343. Distinguishes 'unfunded' from 'funder known, grant unmatched'.",
  "award_count", "Number of award objects.", "Range 0-64.",
  "award_ids_count", "Count of non-empty grant IDs.", "Equals award_count for all works (100% grant-ID fill).",
  "award_display_names", "Award/grant titles.", "~76% empty: Crossref grant records carry no title.",
  
  ## -- flags
  "is_paratext", "Derived from type == 'paratext'.", "Native field deprecated. All FALSE.",
  "is_paratext_raw", "The is_paratext value as returned.", "Retained to show it agrees with the derivation.",
  "is_retracted", "Retraction flag.", "All FALSE.",
  
  ## -- derived conveniences
  "is_international_collab", "countries_distinct_count > 1.", "",
  "is_multi_institution", "institutions_distinct_count > 1.", "",
  "is_single_author", "authors_count == 1.", "",
  "has_abstract", "TRUE if an abstract_inverted_index was present.", "",
  "topics_count", "True number of topics.", "topics_all is capped at 3.",
  "concepts_count", "True number of concepts.", "concepts is capped at 5."
)

## ---- 7c. Join, and flag anything undocumented ----
codebook <- field_stats |>
  dplyr::left_join(defs, by = "field") |>
  dplyr::mutate(
    documented = !is.na(definition),
    definition = dplyr::coalesce(definition,
                                 paste0("Standard OpenAlex Work field: ", field)),
    notes = dplyr::coalesce(notes, "")
  ) |>
  dplyr::arrange(position)

## nothing should ship with a placeholder you haven't reviewed
codebook |> dplyr::filter(!documented) |> dplyr::pull(field)


notes_tbl <- tibble::tribble(
  ~topic, ~note,
  "Source list", "Workbook lists 1,195 rows; W4389192884 appears twice, so 1,194 unique Work IDs. [1]",
  "Retrieval", "1,192 returned by batch query; 2 (W7028777612, W7084109945) required individual fetch because they are expansion-corpus records excluded from queries by default.",
  "grants -> awards", "The requested grants.funder_display_name / grants.award_id no longer exist on the Work object; the payload carries awards[] and funders[]. Delivered columns keep the requested names and are sourced from awards[].funder_display_name and awards[].funder_award_id.",
  "Delimiters", "'; ' = deduplicated name lists. ' | ' = positional, one slot per author (also used for award_ids). ' + ' = multiple institutions within a single author.",
  "Author cap", "The API returns at most 100 authorships; author/institution/country list fields are truncated on such works (see authorships_truncated_flag).",
  "Institution counts", "On 12 works OpenAlex's institutions_distinct_count exceeds the locally recomputed count. Both columns are shipped so the gap is visible.",
  "Funding", "851 works have funder metadata; only 790 have structured award records. Use funding_provenance, not has_funding_data, to distinguish.",
  "Award titles", "~76% of award records have no title. Absence reflects the upstream source, not missing extraction.",
  "Type mapping", "OpenAlex collapses journal articles, proceedings papers, and posted content into type='article', distinguishing them by venue. Check raw_type for the upstream label; posters likely appear as conference-abstract or other."
)

dir.create("cqn/out", showWarnings = FALSE, recursive = TRUE)

## ---- 7d. CSVs: authoritative, no cell limits ----
readr::write_csv(cqn_wide,    "cqn/out/CQN_works_wide.csv",   na = "")
readr::write_csv(codebook,    "cqn/out/CQN_codebook.csv",     na = "")
readr::write_csv(notes_tbl,   "cqn/out/CQN_notes.csv",        na = "")
readr::write_csv(awards_long, "cqn/out/CQN_awards_long.csv",  na = "")
readr::write_csv(requested,   "cqn/out/CQN_requested_ids.csv", na = "")

## ---- 7e. Excel-safe copy ----
XL_MAX <- 32767L

xl_safe <- function(df, limit = XL_MAX) {
  dplyr::mutate(df, dplyr::across(
    dplyr::where(is.character),
    function(x) {
      too_long <- !is.na(x) & nchar(x) > limit
      if (any(too_long)) {
        x[too_long] <- paste0(substr(x[too_long], 1, limit - 15), " [TRUNCATED]")
      }
      x
    }
  ))
}

cqn_xl <- xl_safe(cqn_wide)

## report what got clipped, if anything
clipped <- sum(vapply(cqn_wide[sapply(cqn_wide, is.character)],
                      function(x) sum(!is.na(x) & nchar(x) > XL_MAX),
                      integer(1)))
message("Cells truncated for Excel: ", clipped)

## ---- 7f. Workbook: data + dictionary + caveats together ----
writexl::write_xlsx(
  list(
    works         = cqn_xl,
    codebook      = codebook,
    notes         = notes_tbl,
    awards_long   = awards_long,
    requested_ids = requested
  ),
  path = "cqn/out/CQN_works.xlsx",
  col_names = TRUE,
  format_headers = TRUE
)

## ---- 7g. Confirm what landed on disk ----
list.files("cqn/out", full.names = TRUE) |>
  (\(f) tibble::tibble(
    file  = basename(f),
    kb    = round(file.size(f) / 1024, 1)
  ))()

## round-trip check: the workbook reads back at the expected shape
back <- readxl::read_excel("cqn/out/CQN_works.xlsx", sheet = "works")
dim(back)
nrow(back) == 1194L
sum(duplicated(back$requested_work_id))
setdiff(work_ids, back$requested_work_id)


