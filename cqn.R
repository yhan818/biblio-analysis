# install.packages(c("openalexR","readxl","dplyr","purrr","tibble","stringr","janitor"))
library(openalexR)
library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(stringr)


PATH <- "/home/yhan/Documents/biblio-analysis"
setwd(PATH)
getwd()

Sys.getenv("OPENALEXR_APIKEY")

options(openalexR.mailto = "you@arizona.edu")
stopifnot(nzchar(openalexR::oa_apikey()))   # fail fast if key missing

dir.create("cqn", showWarnings = FALSE)
dir.create("cqn/raw_batches", showWarnings = FALSE)

xlsx_path <- "CQN-ResearchOutputs-OpenAlexIDs.xlsx"
excel_sheets(xlsx_path)   # confirm which sheet holds the flat ID list vs. the pivot

raw <- read_excel(xlsx_path, sheet = 1, col_types = "text")

# Pull anything that looks like an OpenAlex Work ID from any column.
# This is deliberately tolerant: the file has a flat list AND a pivot table
# with "1 W4384522497" style rows, so regex extraction is safer than
# trusting a single named column [1]
ids_all <- raw |>
  mutate(across(everything(), as.character)) |>
  unlist(use.names = FALSE) |>
  str_extract_all("W\\d{6,}") |>
  unlist(use.names = FALSE)

work_ids <- ids_all |>
  str_trim() |>
  str_to_upper() |>
  unique()

length(ids_all)    # total mentions, incl. pivot duplicates
length(work_ids)   # unique IDs -- expect ~1194 given the W4389192884 dupe [1]

# Keep the canonical input list for later reconciliation
requested <- tibble(requested_work_id = work_ids)
nrow(requested)                                    # 1194
write.csv(requested, "cqn/requested_ids.csv", row.names = FALSE)

batches <- split(work_ids, ceiling(seq_along(work_ids) / 50))
length(batches)   # ~24 batches of 50

fetch_batch <- function(ids, i, output = c("list", "tibble")) {
  output <- match.arg(output)
  f <- sprintf("cqn/raw_batches/batch_%03d_%s.rds", i, output)
  if (file.exists(f)) return(readRDS(f))          # resume-safe
  
  res <- try(
    oa_fetch(
      entity     = "works",
      identifier = ids,
      output     = output,
      abstract   = TRUE,
      verbose    = TRUE
    ),
    silent = TRUE
  )
  
  if (inherits(res, "try-error")) {
    message(sprintf("BATCH %d FAILED (%s): %s", i, output, conditionMessage(attr(res, "condition"))))
    return(NULL)
  }
  saveRDS(res, f)
  Sys.sleep(1)                                     # be polite
  res
}

# 2a. Raw JSON lists -- the authoritative copy of every field
raw_list <- imap(batches, ~ fetch_batch(.x, as.integer(.y), "list"))
works_raw <- purrr::list_flatten(compact(raw_list))
saveRDS(works_raw, "cqn/works_raw_all.rds")
length(works_raw)

# 2b. openalexR's flattened tibble -- convenience / cross-check
tbl_list <- imap(batches, ~ fetch_batch(.x, as.integer(.y), "tibble"))
works_tbl <- bind_rows(compact(tbl_list))
saveRDS(works_tbl, "cqn/works_tibble_all.rds")
dim(works_tbl)


# What did we actually get back?
returned <- map_chr(works_raw, ~ sub("https://openalex.org/", "", .x$id %||% NA_character_))

setdiff(work_ids, returned)   # requested but not returned
setdiff(returned, work_ids)   # returned under a DIFFERENT id => merged/redirected work
sum(duplicated(returned))

# Which top-level fields exist across the corpus, and how consistently?
field_coverage <- works_raw |>
  map(names) |>
  unlist() |>
  table() |>
  sort(decreasing = TRUE) |>
  enframe(name = "field", value = "n_works") |>
  mutate(pct = round(100 * as.integer(n_works) / length(works_raw), 1))
print(field_coverage, n = 100)

# Confirm the fields you asked for are present before I write the flattener
c("countries_distinct_count", "institutions_distinct_count",
  "awards", "funders", "grants", "is_xpac", "is_paratext",
  "is_retracted", "type", "fwci") |>
  set_names() |>
  map_dbl(~ mean(map_lgl(works_raw, function(w) !is.null(w[[.x]])))) |>
  round(3)

# Hyperauthorship / truncation check (authorships cap at 100)
n_auth <- map_int(works_raw, ~ length(.x$authorships %||% list()))
table(cut(n_auth, c(-1, 0, 1, 10, 50, 99, Inf)))
sum(n_auth >= 100)

# Type mix, so we know what "articles, conference papers, chapters, posters, preprints" resolves to
table(map_chr(works_raw, ~ .x$type %||% NA_character_), useNA = "ifany")

# One record, fully expanded, for eyeballing
str(works_raw[[1]], max.level = 2)


library(httr2)

# length-zero-safe scalar coalesce
`%|0|%` <- function(x, y) {
  if (length(x) != 1 || is.na(x) || (is.character(x) && !nzchar(x))) y else x
}

oa_key <- tryCatch(openalexR::oa_apikey(), error = function(e) NULL)
has_key <- length(oa_key) == 1 && !is.na(oa_key) && nzchar(oa_key)
has_key

missing_ids <- setdiff(work_ids, returned)
missing_ids   # "W7028777612" "W7084109945"
########## missing_ids: 200

probe_id <- function(id, corpus = NULL) {
  req <- request("https://api.openalex.org/works") |>
    req_url_path_append(id) |>
    req_url_query(mailto = "you@arizona.edu") |>
    req_error(is_error = function(resp) FALSE)   # capture 404 rather than throw
  
  if (has_key)          req <- req_url_query(req, api_key = oa_key)
  if (!is.null(corpus)) req <- req_url_query(req, corpus = corpus)
  
  resp <- req_perform(req)
  st   <- resp_status(resp)
  ok   <- st == 200
  body <- if (ok) tryCatch(resp_body_json(resp), error = function(e) NULL) else NULL
  
  tibble::tibble(
    requested   = id,
    corpus      = corpus %|0|% "(default)",
    status      = st,
    returned_id = sub("https://openalex.org/", "", body$id %|0|% NA_character_),
    is_xpac     = as.character(body$is_xpac %|0|% NA),
    type        = body$type %|0|% NA_character_,
    title       = substr(body$display_name %|0|% "", 1, 80),
    body_snip   = if (!ok) substr(resp_body_string(resp), 1, 200) else NA_character_
  )
}

probe_results <- dplyr::bind_rows(
  purrr::map(missing_ids, probe_id),
  purrr::map(missing_ids, probe_id, corpus = "all")
)
print(probe_results, width = Inf)

# Singleton fetch -> same nested-list shape as oa_fetch(output = "list") elements
fetch_singleton <- function(id) {
  req <- request("https://api.openalex.org/works") |>
    req_url_path_append(id) |>
    req_url_query(mailto = "you@arizona.edu") |>
    req_retry(max_tries = 4)
  if (has_key) req <- req_url_query(req, api_key = oa_key)
  resp_body_json(req_perform(req))          # simplifyVector = FALSE by default
}

missing_ids  <- setdiff(work_ids, returned)
xpac_records <- lapply(missing_ids, fetch_singleton)
names(xpac_records) <- missing_ids

# Structural compatibility check before binding
setdiff(names(works_raw[[1]]), names(xpac_records[[1]]))   # in core, absent from xpac
setdiff(names(xpac_records[[1]]), names(works_raw[[1]]))   # in xpac, absent from core

works_raw_full <- c(works_raw, xpac_records)
length(works_raw_full)                                      # expect 1194

# Provenance: how each record was obtained
fetch_method <- c(rep("list_filter", length(works_raw)),
                  rep("singleton_xpac", length(xpac_records)))

saveRDS(works_raw_full, "cqn/works_raw_full.rds")
saveRDS(tibble::tibble(requested_work_id = names(works_raw_full) %|0|% NA,
                       fetch_method      = fetch_method),
        "cqn/fetch_provenance.rds")

# Final reconciliation -- both should be empty / zero
returned_full <- vapply(works_raw_full,
                        function(w) sub("https://openalex.org/", "", w$id %|0|% NA_character_),
                        character(1))
setdiff(work_ids, returned_full)
sum(duplicated(returned_full))


`%||%` <- function(x, y) if (is.null(x)) y else x

# Is_xpac distribution across the whole corpus (confirms only these 2)
table(vapply(works_raw_full, function(w) as.character(w$is_xpac %||% NA), character(1)),
      useNA = "ifany")

# 1. Field coverage
field_coverage <- works_raw_full |>
  purrr::map(names) |> unlist() |> table() |> sort(decreasing = TRUE) |>
  tibble::enframe(name = "field", value = "n_works") |>
  dplyr::mutate(n_works = as.integer(n_works),
                pct = round(100 * n_works / length(works_raw_full), 1))
print(field_coverage, n = 100)

# 2. The fields your deliverable needs  <-- awards vs grants is the decisive line
c("countries_distinct_count", "institutions_distinct_count",
  "awards", "funders", "grants", "is_xpac", "is_paratext",
  "is_retracted", "fwci", "citation_normalized_percentile",
  "primary_topic", "sustainable_development_goals", "keywords") |>
  rlang::set_names() |>
  purrr::map_dbl(~ mean(purrr::map_lgl(works_raw_full, function(w) !is.null(w[[.x]])))) |>
  round(3)

# 3. Author-count distribution / 100-author truncation
n_auth <- purrr::map_int(works_raw_full, ~ length(.x$authorships %||% list()))
table(cut(n_auth, c(-1, 0, 1, 10, 50, 99, Inf)))
sum(n_auth >= 100)

# 4. Type mix
table(purrr::map_chr(works_raw_full, ~ .x$type %||% NA_character_), useNA = "ifany")

# 5. Local tibble conversion, no API cost -- does it survive the xpac records?
works_tbl <- openalexR::works2df(works_raw_full)
dim(works_tbl); names(works_tbl)


library(dplyr); library(purrr); library(tibble); library(stringr); library(tidyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

strip_oa <- function(x) sub("^https?://openalex\\.org/", "", x)

# Scalar getters, NA-safe against missing/empty nested paths
gs <- function(w, ..., .type = NA_character_) {
  v <- purrr::pluck(w, ...)
  if (is.null(v) || length(v) == 0) return(.type)
  v <- v[[1]]
  if (is.null(v)) .type else v
}
g_chr <- function(w, ...) as.character(gs(w, ..., .type = NA_character_))
g_int <- function(w, ...) suppressWarnings(as.integer(gs(w, ..., .type = NA_integer_)))
g_dbl <- function(w, ...) suppressWarnings(as.numeric(gs(w, ..., .type = NA_real_)))
g_lgl <- function(w, ...) as.logical(gs(w, ..., .type = NA))

# Collapse a character vector to one delimited cell
cc <- function(x, sep = "; ", dedupe = FALSE) {
  x <- unlist(x, use.names = FALSE)
  x <- x[!is.na(x) & nzchar(x)]
  if (dedupe) x <- unique(x)
  if (!length(x)) NA_character_ else paste(x, collapse = sep)
}

rebuild_abstract <- function(aii) {
  if (is.null(aii) || !length(aii)) return(NA_character_)
  pos <- unlist(aii, use.names = FALSE)
  wrd <- rep(names(aii), lengths(aii))
  paste(wrd[order(pos)], collapse = " ")
}







source("cqn/04_extract.R")

## the function must now contain a return block
grepl("tibble::tibble", paste(deparse(extract_people), collapse = "\n"))   # TRUE

## single record: expect a 1 x 17 tibble, not a bare logical
extract_people(works_raw_full[[1]])

people <- purrr::map_dfr(works_raw_full, extract_people)
dim(people)                              # 1194 x 17
sum(duplicated(people$requested_work_id))  # 0
dim(people)   # expect 1194 rows


## 1. Do OpenAlex's counts agree with mine? Disagreements are informative,
##    not necessarily wrong -- they flag ROR-unmatched affiliations.
people |>
  dplyr::mutate(
    ctry_gap = countries_distinct_count    - countries_distinct_computed,
    inst_gap = institutions_distinct_count - institutions_distinct_computed
  ) |>
  dplyr::count(ctry_gap, inst_gap, sort = TRUE) |>
  print(n = 20)

### Testing! 1% gap. over 100 authors etc.
gap_ids <- people |>
  dplyr::mutate(inst_gap = institutions_distinct_count - institutions_distinct_computed) |>
  dplyr::filter(inst_gap > 0) |>
  dplyr::select(requested_work_id, authors_count, inst_gap,
                institutions_distinct_count, institutions_distinct_computed,
                authorships_truncated_flag)
print(gap_ids, n = 20)

w <- works_raw_full[[gap_ids$requested_work_id[which.max(gap_ids$inst_gap)]]]
lapply(w$authorships, function(au) {
  lapply(au$institutions, function(i) c(id = i$id, name = i$display_name))
})

############ awards

source("cqn/04_extract.R")

awards <- purrr::map_dfr(works_raw_full, extract_awards)
awards$requested_work_id <- vapply(works_raw_full, function(w) strip_oa(w$id), character(1))
awards <- dplyr::relocate(awards, requested_work_id)

people_awards <- dplyr::left_join(people, awards, by = "requested_work_id")
dim(people_awards)   # 1194 rows, 17 + 7 - 1 = 23 cols

awards_long <- purrr::map_dfr(works_raw_full, awards_long_of)
dim(awards_long)

#####################################3
## fill rate -- how many CQN works carry any funding metadata at all
mean(people_awards$has_funding_data)
table(people_awards$award_count)

## do the two award fields have the same number of items per work?
awards |>
  dplyr::mutate(
    n_ids = ifelse(is.na(award_ids), 0L,
                   stringr::str_count(award_ids, stringr::fixed("; ")) + 1L)
  ) |>
  dplyr::count(award_count, n_ids)

## which funders appear most across the project
awards_long |>
  dplyr::count(funder_display_name, sort = TRUE) |>
  print(n = 25)

## blank funder_award_id is common -- funder known, grant number not
mean(is.na(awards_long$funder_award_id))

## A. The 61: funder known, no award object
funder_only <- people_awards |>
  dplyr::filter(award_count == 0, has_funding_data) |>
  dplyr::select(requested_work_id, funder_count, funder_display_names)
nrow(funder_only)                      # expect 61
print(head(funder_only, 10), width = Inf)

## B. Does award_ids carry one item per award? (n_ids <= award_count expected)
awards |>
  dplyr::mutate(
    n_ids = dplyr::if_else(is.na(award_ids), 0L,
                           stringr::str_count(award_ids, stringr::fixed("; ")) + 1L),
    gap   = award_count - n_ids
  ) |>
  dplyr::count(gap, sort = TRUE)

## C. Fill rate of the grant number itself
mean(is.na(awards_long$funder_award_id))
mean(is.na(awards_long$award_display_name))
mean(is.na(awards_long$award_openalex_id))

## D. Who funds CQN
awards_long |>
  dplyr::count(funder_display_name, sort = TRUE) |>
  print(n = 30)

## E. The 64-award outlier -- real, or over-matched?
outlier <- people_awards |> dplyr::slice_max(award_count, n = 3) |>
  dplyr::select(requested_work_id, authors_count, award_count, funder_count)
print(outlier, width = Inf)

awards_long |>
  dplyr::filter(requested_work_id == outlier$requested_work_id[1]) |>
  dplyr::select(funder_display_name, funder_award_id, award_display_name) |>
  print(n = 70)

people_awards <- people_awards |>
  dplyr::mutate(
    has_awards = award_count > 0L,
    funding_provenance = dplyr::case_when(
      award_count > 0L & !is.na(award_ids)          ~ "award_with_grant_id",
      award_count > 0L &  is.na(award_ids)          ~ "award_no_grant_id",
      award_count == 0L & has_funding_data          ~ "funder_only",
      TRUE                                          ~ "none"
    )
  )

table(people_awards$funding_provenance)


source("cqn/04_extract.R")

awards <- purrr::map_dfr(works_raw_full, extract_awards)
awards$requested_work_id <- vapply(works_raw_full, function(w) strip_oa(w$id), character(1))
awards <- dplyr::relocate(awards, requested_work_id)

## must be all-zero now
awards |> dplyr::count(gap = award_count - award_ids_count)

## how many works carry a semicolon inside a grant ID
sum(awards$award_id_has_delim)

people_awards <- people |>
  dplyr::left_join(awards, by = "requested_work_id") |>
  dplyr::mutate(
    funding_provenance = dplyr::case_when(
      has_awards & award_ids_count > 0  ~ "award_with_grant_id",
      has_awards & award_ids_count == 0 ~ "award_no_grant_id",
      has_funding_data                  ~ "funder_only",
      TRUE                              ~ "none"
    )
  )

table(people_awards$funding_provenance)


## Which award IDs contain the delimiter?
awards_long |>
  dplyr::filter(stringr::str_detect(funder_award_id, ";")) |>
  dplyr::select(requested_work_id, funder_display_name, funder_award_id) |>
  print(n = 30, width = Inf)

## Are the 12 negative-gap works exactly those works?
neg_gap <- awards |>
  dplyr::mutate(
    n_ids = dplyr::if_else(is.na(award_ids), 0L,
                           stringr::str_count(award_ids, stringr::fixed("; ")) + 1L)
  ) |>
  dplyr::filter(award_count - n_ids < 0) |>
  dplyr::pull(requested_work_id)

collide <- awards_long |>
  dplyr::filter(stringr::str_detect(funder_award_id, ";")) |>
  dplyr::pull(requested_work_id) |> unique()

setdiff(neg_gap, collide)   # should be empty
setdiff(collide, neg_gap)   # should be empty



source("cqn/04_extract.R")

meta <- purrr::map_dfr(works_raw_full, extract_meta)
dim(meta)          # expect 1194 rows
names(meta)

## sanity: keys unique and matching the other tables
sum(duplicated(meta$requested_work_id))
setdiff(meta$requested_work_id, people$requested_work_id)

## the paratext derivation vs whatever the payload carried
table(meta$is_paratext, meta$is_paratext_raw, useNA = "ifany")

## flags worth eyeballing before delivery
table(meta$type, useNA = "ifany")
table(meta$is_retracted, useNA = "ifany")
table(meta$is_xpac, useNA = "ifany")     # expect the 2 expansion records
table(meta$oa_status, useNA = "ifany")
summary(meta$publication_year)

###################################3333
############## Step 5: 
## canonical requested list (1194 unique IDs from your workbook)
requested <- tibble::tibble(requested_work_id = work_ids)

## provenance from the expansion-corpus recovery
prov <- tibble::tibble(
  requested_work_id = vapply(works_raw_full, function(w) strip_oa(w$id), character(1)),
  fetch_method      = fetch_method
)

## no column-name collisions across the three tables
intersect(setdiff(names(meta), "requested_work_id"), names(people))
intersect(setdiff(names(meta), "requested_work_id"), names(awards))
intersect(setdiff(names(people), "requested_work_id"), names(awards))

cqn_wide <- requested |>
  dplyr::left_join(meta,   by = "requested_work_id") |>
  dplyr::left_join(people, by = "requested_work_id") |>
  dplyr::left_join(awards, by = "requested_work_id") |>
  dplyr::left_join(prov,   by = "requested_work_id") |>
  dplyr::mutate(
    fetch_status = dplyr::if_else(is.na(returned_openalex_id), "not_found", "ok"),
    funding_provenance = dplyr::case_when(
      has_awards & award_ids_count > 0  ~ "award_with_grant_id",
      has_awards & award_ids_count == 0 ~ "award_no_grant_id",
      has_funding_data                  ~ "funder_only",
      TRUE                              ~ "none"
    ),
    is_international_collab = countries_distinct_count > 1L,
    is_multi_institution    = institutions_distinct_count > 1L,
    is_single_author        = authors_count == 1L
  ) |>
  dplyr::relocate(
    requested_work_id, returned_openalex_id, fetch_status, fetch_method,
    is_xpac, doi, title, type, raw_type, publication_year, publication_date,
    source_display_name, author_names_clean, institution_names_clean,
    authorships_countries, countries_distinct_count, institutions_distinct_count,
    funder_display_names, award_ids
  )

dim(cqn_wide)
table(cqn_wide$fetch_status)
sum(is.na(cqn_wide$title))

## row count must equal the deduped input
nrow(cqn_wide) == length(work_ids)
sum(duplicated(cqn_wide$requested_work_id))

## every requested ID present, nothing extra
setdiff(work_ids, cqn_wide$requested_work_id)
setdiff(cqn_wide$requested_work_id, work_ids)

## your seven requested fields: fill rates
cqn_wide |>
  dplyr::summarise(
    dplyr::across(
      c(countries_distinct_count, institutions_distinct_count,
        authorships_countries, funder_display_names, award_ids,
        author_names_clean, institution_names_clean),
      ~ round(mean(!is.na(.x)), 3)
    )
  ) |> t()

## collaboration profile
table(cqn_wide$is_international_collab, useNA = "ifany")
summary(cqn_wide$countries_distinct_count)
summary(cqn_wide$institutions_distinct_count)
table(cqn_wide$funding_provenance)

## Excel cell-limit exposure (32,767 chars)
long_cells <- cqn_wide |>
  dplyr::summarise(dplyr::across(dplyr::where(is.character),
                                 ~ max(nchar(.x), na.rm = TRUE))) |>
  t() |> as.data.frame() |> setNames("max_chars") |>
  tibble::rownames_to_column("field") |>
  dplyr::filter(max_chars > 30000)
long_cells

### output
source("cqn/07_write_out.R")

########## TESTing

source("cqn/08_validate.R")




################## TBD: TALK TO OPENALEX

## Audit: authorships where the raw string exists but matching failed
affil_audit <- purrr::map_dfr(works_raw_full, function(w) {
  a <- w$authorships %||% list()
  if (!length(a)) return(NULL)
  purrr::imap_dfr(a, function(au, i) {
    raw <- c(
      unlist(au$raw_affiliation_strings %||% list(), use.names = FALSE),
      vapply(au$affiliations %||% list(),
             function(x) chr1(x$raw_affiliation_string), character(1))
    )
    raw <- unique(raw[!is.na(raw) & nzchar(raw)])
    tibble::tibble(
      requested_work_id = strip_oa(w$id),
      doi               = chr1((w$ids %||% list())$doi),
      author_position   = as.integer(i),
      author_name       = chr1(au$author$display_name),
      n_institutions    = length(au$institutions %||% list()),
      n_countries       = length(unlist(au$countries %||% list())),
      raw_affiliation   = paste(raw, collapse = " || ")
    )
  })
})

## the reportable set: text present, nothing resolved
unmatched <- affil_audit |>
  dplyr::filter(nzchar(raw_affiliation),
                n_institutions == 0, n_countries == 0)

nrow(unmatched)
dplyr::n_distinct(unmatched$requested_work_id)

## group by the offending string -- repeated strings are the strongest signal
unmatched |>
  dplyr::count(raw_affiliation, sort = TRUE) |>
  print(n = 40)

readr::write_csv(unmatched, "cqn/out/CQN_affiliation_report.csv", na = "")


