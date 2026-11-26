# ============================================================
# ua_all_works_2025_direct.R
#
# ALL works with University of Arizona authorship, 2025.
# Direct OpenAlex REST API + JSON. openalexR is NOT used.
#
# WHY DIRECT:
#   openalexR's oa_fetch(output = "tibble") passes the JSON
#   through oa2df()/works2df(), which maps only a fixed set of
#   fields into columns. Anything outside that map is dropped
#   silently -- which is why best_oa_location,
#   countries_distinct_count and institutions_distinct_count
#   never appeared. Raw JSON has no field map: you get exactly
#   what you select.
#
# CONSEQUENCES:
#   * countries_distinct_count / institutions_distinct_count
#     come straight from OpenAlex. NO local calculation, so no
#     exposure to the 100-author cap on `authorships`.
#   * best_oa_location is flattened directly, including the
#     NULL-when-closed case.
#   * primary_topic gives field / subfield / domain directly,
#     so no digging through the topics list-column.
#
# CREATED: see date_pulled below
# ============================================================

# ---- Packages ------------------------------------------------
need <- c("httr2", "jsonlite", "purrr", "dplyr", "tibble",
          "tidyr", "readr", "stringr")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) {
  stop("Install first: install.packages(c(",
       paste0('"', miss, '"', collapse = ", "), "))")
}

library(openalexR)
library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(tibble)
library(tidyr)
library(readr)
library(stringr)


`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- Configuration -------------------------------------------
UA_ID       <- "I138006243"          # University of Arizona
UA_ROR      <- "https://ror.org/03m2x1q45"
YR          <- 2025
MAILTO      <- "yhan@arizona.edu"     # <-- EDIT: polite pool
PER_PAGE    <- 100                   # API maximum
INCLUDE_ABSTRACT <- TRUE            # TRUE roughly triples payload size
BASE        <- "https://api.openalex.org/works"


# Preferred environment-variable name
API_KEY <- Sys.getenv("OPENALEX_API_KEY", unset = "")

# Backward-compatible fallbacks for names used in older scripts
if (!nzchar(API_KEY)) {
  API_KEY <- Sys.getenv("OPENALEXR_APIKEY", unset = "")
}

if (!nzchar(API_KEY)) {
  API_KEY <- Sys.getenv("OPENALEX_KEY", unset = "")
}

if (!nzchar(API_KEY)) {
  stop(
    "No OpenAlex API key was found.\n",
    "Add OPENALEX_API_KEY=your_key to ~/.Renviron, ",
    "then restart R."
  )
}

message(
  "OpenAlex API key loaded: ",
  substr(API_KEY, 1, 4),
  "...",
  substr(API_KEY, nchar(API_KEY) - 3, nchar(API_KEY))
)

# ============================================================
# OPENALEX API KEY AND CURSOR-PAGINATION TEST
# ============================================================

if (!exists("API_KEY") || !nzchar(API_KEY)) {
  stop("API_KEY is missing or empty.")
}

if (!exists("FILTER") || !nzchar(FILTER)) {
  stop("FILTER is missing or empty.")
}

if (!exists("oa_get") || !is.function(oa_get)) {
  stop("Function oa_get() has not been defined.")
}

message("Testing cursor page 1...")

cursor_test_1 <- oa_get(list(
  filter     = FILTER,
  select     = "id",
  `per-page` = 1,
  cursor     = "*"
))

if (cursor_test_1$status != 200) {
  stop(
    "Cursor test page 1 failed: HTTP ",
    cursor_test_1$status,
    " — ",
    cursor_test_1$body$message %||% "No API message returned"
  )
}

next_cursor <- cursor_test_1$body$meta$next_cursor

if (is.null(next_cursor) || !nzchar(next_cursor)) {
  stop("Page 1 did not return a next_cursor.")
}

message("Page 1 passed. Testing cursor page 2...")

cursor_test_2 <- oa_get(list(
  filter     = FILTER,
  select     = "id",
  `per-page` = 1,
  cursor     = next_cursor
))

if (cursor_test_2$status != 200) {
  stop(
    "Cursor test page 2 failed: HTTP ",
    cursor_test_2$status,
    " — ",
    cursor_test_2$body$message %||% "No API message returned"
  )
}

message("PASS: API key is valid and cursor page 2 is accessible.")



PATH <- "/home/yhan/Documents/biblio-analysis"
setwd(PATH)
getwd()

date_pulled <- Sys.time()
stamp       <- format(as.Date(date_pulled), "%Y%m%d")
prefix      <- paste0("UA_ALL_works_", YR, "_", stamp)

FILTER <- paste0(
  "authorships.institutions.id:", UA_ID,
  ",from_publication_date:", YR, "-01-01",
  ",to_publication_date:",   YR, "-12-31"
)

message(strrep("=", 62))
message("UA ALL WORKS ", YR, " -- direct OpenAlex JSON")
message("Pulled: ", format(date_pulled, "%Y-%m-%d %H:%M:%S"), " ", Sys.timezone())
message("Filter: ", FILTER)
message(strrep("=", 62))

# ---- Request helper ------------------------------------------
# Polite pool via mailto; retries with backoff on 429/5xx.
oa_get <- function(query) {
  q <- c(query, list(mailto = MAILTO))
  if (nzchar(API_KEY)) q$api_key <- API_KEY
  
  req <- request(BASE) |>
    req_url_query(!!!q) |>
    req_user_agent(paste0("UA-OA-analysis/1.0 (mailto:", MAILTO, ")")) |>
    req_timeout(120) |>
    req_retry(
      max_tries = 6,
      is_transient = function(resp) resp_status(resp) %in% c(429, 500, 502, 503, 504),
      backoff = function(i) min(60, 2^i)
    ) |>
    req_error(is_error = function(resp) FALSE)   # inspect status ourselves
  
  resp <- req_perform(req)
  list(status = resp_status(resp),
       body   = tryCatch(resp_body_json(resp, simplifyVector = FALSE),
                         error = function(e) NULL))
}

# ============================================================
# STEP 1 -- Count first
# ============================================================
r1 <- oa_get(list(filter = FILTER, `per-page` = 1))
stopifnot(r1$status == 200)
expected_n <- r1$body$meta$count

message("\nSTEP 1  API reports ", format(expected_n, big.mark = ","),
        " works (expected ~9,734+)")

# ============================================================
# STEP 2 -- Validate every candidate field against the API
#
# select accepts ROOT-LEVEL fields only. Nested paths such as
# best_oa_location.version are rejected. Rather than assume,
# probe each field and keep only those the API accepts.
# ============================================================
candidate_fields <- c(
  # identity
  "id", "doi", "ids", "title", "display_name",
  "publication_year", "publication_date", "language",
  "type", "type_crossref",
  # OA
  "open_access", "best_oa_location", "primary_location",
  "locations_count", "has_fulltext", "indexed_in",
  # authorship + the two distinct counts we want from the API
  "authorships", "countries_distinct_count",
  "institutions_distinct_count",
  "corresponding_author_ids", "corresponding_institution_ids",
  "authors_count",
  # money
  "apc_list", "apc_paid", "grants", "funders", "awards",
  # classification
  "primary_topic", "topics", "keywords", "concepts",
  "sustainable_development_goals",
  # impact
  "cited_by_count", "counts_by_year", "fwci",
  "citation_normalized_percentile", "referenced_works_count",
  # flags
  "is_retracted", "is_paratext",
  # optional
  if (INCLUDE_ABSTRACT) "abstract_inverted_index" else NULL
)

message("\nSTEP 2  Probing ", length(candidate_fields), " candidate fields...")

probe <- function(f) {
  r <- oa_get(list(filter = FILTER, select = paste("id", f, sep = ","),
                   `per-page` = 1))
  ok  <- r$status == 200
  key <- ok && !is.null(r$body$results) && length(r$body$results) > 0 &&
    f %in% names(r$body$results[[1]])
  tibble(field = f, http = r$status, accepted = ok, key_returned = key)
}

probe_tbl <- map_dfr(setdiff(candidate_fields, "id"), function(f) {
  Sys.sleep(0.12)   # stay well inside 10 req/sec
  probe(f)
})

probe_tbl
# ============================================================
# STEP 2 (continued) -- Review probe results, build final select
# ============================================================
message("\nProbe results:")
print(probe_tbl, n = Inf)

rejected <- probe_tbl |> filter(!accepted)
silent   <- probe_tbl |> filter(accepted, !key_returned)

if (nrow(rejected)) {
  message("\nREJECTED by API (dropped from select):")
  print(rejected)
  message("  Note: type_crossref is expected to fail -- it was renamed raw_type.")
}
if (nrow(silent)) {
  message("\nAccepted but no key on the sampled record (may just be null):")
  print(silent$field)
}

FIELDS <- c("id", probe_tbl$field[probe_tbl$accepted])
FIELDS <- unique(FIELDS)

# If type_crossref was rejected, swap in its replacement
if ("type_crossref" %in% rejected$field) {
  r <- probe("raw_type")
  if (r$accepted) {
    FIELDS <- c(FIELDS, "raw_type")
    message("  -> added raw_type in place of type_crossref")
  }
}

message("\nFinal select list (", length(FIELDS), " fields):")
message("  ", paste(FIELDS, collapse = ", "))

# ============================================================
# STEP 3 -- Fetch everything with cursor pagination
# per-page max is 200; cursor="*" starts the walk, then follow
# meta.next_cursor until it comes back null.
# ============================================================
fetch_all <- function(filter, fields, per_page = PER_PAGE) {
  cursor <- "*"; pages <- list(); i <- 0L; got <- 0L
  t0 <- Sys.time()
  
  repeat {
    r <- oa_get(list(
      filter     = filter,
      select     = paste(fields, collapse = ","),
      `per-page` = per_page,
      cursor     = cursor
    ))
    
    if (r$status != 200) {
      stop("HTTP ", r$status, " on page ", i + 1L, ": ",
           r$body$message %||% "no message returned")
    }
    
    res <- r$body$results %||% list()
    if (!length(res)) break
    
    i <- i + 1L
    pages[[i]] <- res
    got <- got + length(res)
    
    if (i %% 5L == 0L || got >= expected_n) {
      el <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
      message("  page ", i, "  records ", format(got, big.mark = ","),
              "/", format(expected_n, big.mark = ","), "  (", el, "s)")
    }
    
    cursor <- r$body$meta$next_cursor
    if (is.null(cursor) || !nzchar(cursor)) break
  }
  
  message("  done: ", format(got, big.mark = ","), " records in ", i, " pages")
  unlist(pages, recursive = FALSE)
}

message("\nSTEP 3  Fetching all works...")
works_raw <- fetch_all(FILTER, FIELDS)

stopifnot(length(works_raw) > 0)
saveRDS(works_raw, paste0(prefix, "_raw_json.rds"))
message("Raw JSON list saved: ", prefix, "_raw_json.rds")

########################### 
#works_raw <- readRDS(paste0(prefix, "_raw_json.rds"))
 
# ============================================================
# PMCID NORMALIZATION FIX
#
# Required existing object:
#   pmcid_check
#
# Required column:
#   pmcid_raw
#
# No packages required.
# ============================================================

if (!exists("pmcid_check", inherits = TRUE)) {
  stop("Object `pmcid_check` was not found.")
}

if (!is.data.frame(pmcid_check)) {
  stop("`pmcid_check` must be a data frame.")
}

if (!"pmcid_raw" %in% names(pmcid_check)) {
  stop("`pmcid_check` does not contain `pmcid_raw`.")
}

# ------------------------------------------------------------
# Normalize one PMCID value
# ------------------------------------------------------------

normalize_pmcid <- function(x) {
  
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  x <- as.character(x[[1L]])
  
  if (is.na(x)) {
    return(NA_character_)
  }
  
  x <- trimws(x)
  
  if (!nzchar(x)) {
    return(NA_character_)
  }
  
  # Case 1:
  # Already contains the canonical form PMC followed by digits.
  #
  # Examples:
  #   PMC12011386
  #   https://.../PMC12011386/
  canonical_match <- regexpr(
    "PMC[0-9]+",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  if (canonical_match[[1L]] != -1L) {
    return(toupper(regmatches(x, canonical_match)))
  }
  
  # Case 2:
  # Current OpenAlex value is an NCBI PMC URL whose article
  # component contains digits without the PMC prefix.
  #
  # Example:
  # https://www.ncbi.nlm.nih.gov/pmc/articles/12011386
  numeric_url_match <- regexec(
    paste0(
      "^https?://",
      "(?:www\\.)?",
      "(?:",
      "ncbi\\.nlm\\.nih\\.gov/pmc",
      "|",
      "pmc\\.ncbi\\.nlm\\.nih\\.gov",
      ")",
      "/articles/([0-9]+)",
      "/?",
      "(?:[?#].*)?",
      "$"
    ),
    x,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  matched_parts <- regmatches(x, numeric_url_match)[[1L]]
  
  if (length(matched_parts) >= 2L) {
    return(paste0("PMC", matched_parts[[2L]]))
  }
  
  # The value exists but is not a recognized PMCID format.
  NA_character_
}

# ------------------------------------------------------------
# Controlled tests
# ------------------------------------------------------------

test_input <- c(
  "PMC12011386",
  "pmc12535840",
  "https://www.ncbi.nlm.nih.gov/pmc/articles/12011386",
  "https://www.ncbi.nlm.nih.gov/pmc/articles/12535840/",
  "https://pmc.ncbi.nlm.nih.gov/articles/12687902/",
  "https://pmc.ncbi.nlm.nih.gov/articles/PMC12016788/",
  NA_character_,
  "",
  "not-a-pmcid"
)

test_expected <- c(
  "PMC12011386",
  "PMC12535840",
  "PMC12011386",
  "PMC12535840",
  "PMC12687902",
  "PMC12016788",
  NA_character_,
  NA_character_,
  NA_character_
)

test_observed <- vapply(
  test_input,
  normalize_pmcid,
  character(1)
)

test_results <- data.frame(
  input = test_input,
  expected = test_expected,
  observed = test_observed,
  stringsAsFactors = FALSE
)

print(test_results, row.names = FALSE)

if (!identical(test_observed, test_expected)) {
  stop("PMCID normalization test failed.")
}

message("PASS: PMCID normalization test succeeded.")

# ------------------------------------------------------------
# Apply the corrected normalizer
# ------------------------------------------------------------

pmcid_check$pmcid <- vapply(
  pmcid_check$pmcid_raw,
  normalize_pmcid,
  character(1)
)

# A PMCID key was present, but the value was not recognized.
pmcid_check$pmcid_parse_problem <-
  pmcid_check$pmcid_key_present &
  is.na(pmcid_check$pmcid)

# ------------------------------------------------------------
# Corrected summary
#
# Important:
# "No PMCID reported" means no pmcid key was supplied.
# It is not the same as a PMCID that failed parsing.
# ------------------------------------------------------------

pmcid_summary <- data.frame(
  measure = c(
    "Total works",
    "Works with an ids object",
    "Works with a pmcid key",
    "Successfully normalized PMCIDs",
    "PMCID keys that could not be parsed",
    "Works with no PMCID reported"
  ),
  
  count = c(
    nrow(pmcid_check),
    sum(pmcid_check$ids_object_present, na.rm = TRUE),
    sum(pmcid_check$pmcid_key_present, na.rm = TRUE),
    sum(!is.na(pmcid_check$pmcid)),
    sum(pmcid_check$pmcid_parse_problem, na.rm = TRUE),
    sum(!pmcid_check$pmcid_key_present, na.rm = TRUE)
  ),
  
  stringsAsFactors = FALSE
)

pmcid_summary$percent <- round(
  100 * pmcid_summary$count / nrow(pmcid_check),
  2
)

cat("\n================ CORRECTED PMCID SUMMARY ================\n")
print(pmcid_summary, row.names = FALSE)

# ------------------------------------------------------------
# Show examples
# ------------------------------------------------------------

pmcid_found <- pmcid_check[
  !is.na(pmcid_check$pmcid),
  intersect(
    c("openalex_id", "pmcid", "pmcid_raw", "title"),
    names(pmcid_check)
  ),
  drop = FALSE
]

cat("\nFirst 20 successfully normalized PMCIDs:\n")

if (nrow(pmcid_found) > 0L) {
  print(
    utils::head(pmcid_found, 20L),
    row.names = FALSE
  )
} else {
  cat("No PMCIDs were successfully normalized.\n")
}

pmcid_problems <- pmcid_check[
  pmcid_check$pmcid_parse_problem,
  intersect(
    c("openalex_id", "pmcid_raw", "title"),
    names(pmcid_check)
  ),
  drop = FALSE
]

cat("\nUnrecognized PMCID values:\n")

if (nrow(pmcid_problems) > 0L) {
  print(
    utils::head(pmcid_problems, 50L),
    row.names = FALSE
  )
} else {
  cat("None. Every PMCID value was normalized successfully.\n")
}

# ------------------------------------------------------------
# Save the corrected diagnostic file
# ------------------------------------------------------------

utils::write.csv(
  pmcid_check,
  "pmcid_check_corrected.csv",
  row.names = FALSE,
  na = ""
)

message("Saved: pmcid_check_corrected.csv")




# ============================================================
# STEP 4 -- Flatten each work to a single row
#
# Null-tolerant throughout. The two cases that bite:
#   * best_oa_location is null on closed works (by design)
#   * source can be null even when best_oa_location exists
# ============================================================

short_id <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  sub("^https?://openalex\\.org/", "", as.character(x)[1])
}

s_chr <- function(x, ...) {
  if (is.null(x)) return(NA_character_)
  v <- purrr::pluck(x, ...)
  if (is.null(v) || !length(v)) NA_character_ else as.character(v)[1]
}
s_int <- function(x, ...) {
  if (is.null(x)) return(NA_integer_)
  v <- purrr::pluck(x, ...)
  if (is.null(v) || !length(v)) NA_integer_ else as.integer(v)[1]
}
s_dbl <- function(x, ...) {
  if (is.null(x)) return(NA_real_)
  v <- purrr::pluck(x, ...)
  if (is.null(v) || !length(v)) NA_real_ else as.numeric(v)[1]
}
s_lgl <- function(x, ...) {
  if (is.null(x)) return(NA)
  v <- purrr::pluck(x, ...)
  if (is.null(v) || !length(v)) NA else as.logical(v)[1]
}

# Collapse a JSON array of objects to one delimited string
join_map <- function(lst, f, sep = "; ") {
  if (is.null(lst) || !length(lst)) return(NA_character_)
  v <- vapply(lst, function(e) {
    out <- tryCatch(f(e), error = function(...) NA_character_)
    if (is.null(out) || !length(out) || is.na(out[1])) NA_character_ else as.character(out)[1]
  }, character(1))
  v <- unique(v[!is.na(v) & nzchar(v)])
  if (!length(v)) NA_character_ else paste(v, collapse = sep)
}

# Reconstruct an abstract from the inverted index, if requested
undo_inverted <- function(ii) {
  if (is.null(ii) || !length(ii)) return(NA_character_)
  pos <- unlist(ii, use.names = FALSE)
  wrd <- rep(names(ii), times = vapply(ii, length, integer(1)))
  if (!length(pos)) return(NA_character_)
  paste(wrd[order(pos)], collapse = " ")
}

# UA name variants for raw-string fallback detection
ua_regex <- paste0(
  "University of Arizona|Univ(ersity)?\\.? of Ariz|UArizona|",
  "Arizona, Tucson|Tucson, Ariz|AZ ?857"
)

inst_is_ua <- function(i) {
  if (identical(short_id(i$id), UA_ID)) return(TRUE)
  lin <- vapply(i$lineage %||% list(), short_id, character(1))
  if (UA_ID %in% lin) return(TRUE)
  identical(i$ror %||% "", UA_ROR)
}

flatten_work <- function(w) {
  
  boa <- w$best_oa_location
  pl  <- w$primary_location
  oa  <- w$open_access
  pt  <- w$primary_topic
  au  <- w$authorships %||% list()      # <-- `au` is born here
  
  # --- UA detection: exact id, lineage, ROR, raw-string mapping ---
  ua_hit <- vapply(au, function(a) {
    if (any(vapply(a$institutions %||% list(), inst_is_ua, logical(1)))) return(TRUE)
    any(vapply(a$affiliations %||% list(), function(f)
      UA_ID %in% vapply(f$institution_ids %||% list(), short_id, character(1)),
      logical(1)))
  }, logical(1))
  
  n_listed <- length(au)
  
  # ---- NEW: raw-string fallback + country-only detection ----
  raw_all <- paste(
    unlist(lapply(au, function(a) unlist(a$raw_affiliation_strings %||% list()))),
    collapse = " // "
  )
  
  ua_in_raw_text <- nzchar(raw_all) &&
    grepl(ua_regex, raw_all, ignore.case = TRUE)
  
  n_no_inst_but_country <- sum(vapply(au, function(a)
    length(a$institutions %||% list()) == 0 &&
      length(a$countries %||% list()) > 0, logical(1)))
 
  grant_source <- if (length(w$awards %||% list()) > 0) w$awards
  else if (length(w$grants %||% list()) > 0) w$grants
  else list()
  
  aw <- w$awards  %||% list()   # specific grants [1]
  fu <- w$funders %||% list()   # funding organizations [1]
  # NOTE: w$grants is gone -- removed from the Work object [6]
  
  # Collect every (institution, country) pair across all authorships.
  # Each dehydrated institution carries id, display_name, ror,
  # country_code, type, and lineage [3].
  inst_rows <- do.call(rbind, lapply(au, function(a) {
    ins <- a$institutions %||% list()
    if (!length(ins)) return(NULL)
    do.call(rbind, lapply(ins, function(i) data.frame(
      inst_id   = short_id(i$id),
      name      = i$display_name %||% NA_character_,
      ror       = i$ror          %||% NA_character_,
      cc        = i$country_code %||% NA_character_,
      itype     = i$type         %||% NA_character_,
      stringsAsFactors = FALSE
    )))
  }))
  
  if (is.null(inst_rows)) {
    inst_rows <- data.frame(inst_id = character(0), name = character(0),
                            ror = character(0), cc = character(0),
                            itype = character(0), stringsAsFactors = FALSE)
  } else {
    # Dedupe on institution ID only, not on all columns.
    # unique() compares every column, so the same institution
    # appearing with a differing display_name or country_code
    # across authorships would survive twice and inflate
    
    inst_rows <- inst_rows[!duplicated(inst_rows$inst_id), , drop = FALSE]
  }
  
  
  tibble(
    # ---------- identity ----------
    id                = short_id(w$id),
    openalex_url      = s_chr(w, "id"),
    doi               = sub("^https?://doi\\.org/", "", s_chr(w, "doi")),
    pmid              = sub("^https?://pubmed\\.ncbi\\.nlm\\.nih\\.gov/", "",
                            s_chr(w, "ids", "pmid")),
    #pmcid             = s_chr(w, "ids", "pmcid"),
    pmcid = normalize_pmcid(s_chr(w, "ids", "pmcid")),
    title             = s_chr(w, "title"),
    publication_year  = s_int(w, "publication_year"),
    publication_date  = s_chr(w, "publication_date"),
    language          = s_chr(w, "language"),
    type              = s_chr(w, "type"),
    raw_type = dplyr::coalesce(
      s_chr(w, "raw_type"),
      s_chr(w, "type_crossref"),
      s_chr(pl, "raw_type")
    ),
    
    # ---------- open access ----------
    is_oa                       = s_lgl(oa, "is_oa"),
    oa_status                   = s_chr(oa, "oa_status"),
    oa_url                      = s_chr(oa, "oa_url"),
    any_repository_has_fulltext = s_lgl(oa, "any_repository_has_fulltext"),
    
    # ---------- best_oa_location (flattened) ----------
    best_oa_exists            = !is.null(boa),
    best_oa_is_oa             = s_lgl(boa, "is_oa"),
    best_oa_version           = s_chr(boa, "version"),
    best_oa_license           = s_chr(boa, "license"),
    best_oa_license_id        = s_chr(boa, "license_id"),
    best_oa_is_accepted       = s_lgl(boa, "is_accepted"),
    best_oa_is_published      = s_lgl(boa, "is_published"),
    best_oa_landing_page_url  = s_chr(boa, "landing_page_url"),
    best_oa_pdf_url           = s_chr(boa, "pdf_url"),
    best_oa_source_id         = short_id(purrr::pluck(boa, "source", "id")),
    best_oa_source_name       = s_chr(boa, "source", "display_name"),
    best_oa_source_type       = s_chr(boa, "source", "type"),
    best_oa_source_issn_l     = s_chr(boa, "source", "issn_l"),
    best_oa_source_is_in_doaj = s_lgl(boa, "source", "is_in_doaj"),
    best_oa_host_org          = s_chr(boa, "source", "host_organization_name"),
    
    # ---------- primary (version of record) location ----------
    primary_source_id         = short_id(purrr::pluck(pl, "source", "id")),
    primary_source_name       = s_chr(pl, "source", "display_name"),
    source_display_name       = s_chr(pl, "source", "display_name"), # alias
    primary_source_type       = s_chr(pl, "source", "type"),
    primary_source_issn_l     = s_chr(pl, "source", "issn_l"),
    primary_source_is_in_doaj = s_lgl(pl, "source", "is_in_doaj"),
    primary_publisher         = s_chr(pl, "source", "host_organization_name"),
    host_organization_name    = s_chr(pl, "source", "host_organization_name"), # alias
    primary_is_oa             = s_lgl(pl, "is_oa"),
    primary_version           = s_chr(pl, "version"),
    primary_license           = s_chr(pl, "license"),
    primary_landing_page_url  = s_chr(pl, "landing_page_url"),
    primary_pdf_url           = s_chr(pl, "pdf_url"),
    locations_count           = s_int(w, "locations_count"),
    indexed_in                = join_map(w$indexed_in, function(e) e),
    # ---------- continued from primary location ----------
    has_fulltext = s_lgl(w, "has_fulltext"),
    
    # ---------- authorship counts ----------
    # These two come STRAIGHT FROM OPENALEX. Nothing is
    # calculated locally, so the 100-author cap on
    # `authorships` does not affect them.
    countries_distinct_count    = s_int(w, "countries_distinct_count"),
    institutions_distinct_count = s_int(w, "institutions_distinct_count"),
    
    # Author counts that DO depend on the (possibly capped)
    # authorships array -- named so the distinction is obvious.
    authors_listed_in_payload = n_listed,
    authors_count_api         = s_int(w, "authors_count"),
    
    # UA-specific flags derived from the authorships array
    ua_author_count       = sum(ua_hit),
    ua_exact_parent_count = sum(vapply(au, function(a)
      any(vapply(a$institutions %||% list(),
                 function(i) identical(short_id(i$id), UA_ID), logical(1))),
      logical(1))),
    ua_in_raw_text        = ua_in_raw_text,
    ua_detect_zero_reason = dplyr::case_when(
      sum(ua_hit) > 0           ~ NA_character_,
      n_listed == 0             ~ "no authorships in payload",
      ua_in_raw_text            ~ "UA in raw string, not linked (parser miss/over-merge)",
      n_no_inst_but_country > 0 ~ "affiliation resolved to country only",
      TRUE                      ~ "no UA signal in payload"
    ),
    ua_is_first_author   = length(ua_hit) > 0 && isTRUE(ua_hit[1]),

    ua_is_corresponding  = {
      cai <- vapply(w$corresponding_institution_ids %||% list(),
                    short_id, character(1))
      UA_ID %in% cai
    },
    
    # ---------- money ----------
    apc_list_value_usd = s_int(w, "apc_list", "value_usd"),
    apc_list_currency  = s_chr(w, "apc_list", "currency"),
    apc_paid_value_usd = s_int(w, "apc_paid", "value_usd"),
    apc_paid_provenance = s_chr(w, "apc_paid", "provenance"),
    
    # --- Awards (grants) ---
    n_awards           = length(aw),
    award_ids          = join_map(aw, function(e) e$funder_award_id %||% NA_character_),
    award_openalex_ids = join_map(aw, function(e) short_id(e$id %||% NA_character_)),
    award_dois         = join_map(aw, function(e) e$doi %||% NA_character_),
    award_funder_names = join_map(aw, function(e) e$funder_display_name %||% NA_character_),
    award_funder_ids   = join_map(aw, function(e) short_id(e$funder_id %||% NA_character_)),
    
    # --- Funders (organizations) — separate array, no award numbers ---
    n_funders    = length(fu),
    funder_names = join_map(fu, function(e) e$display_name %||% NA_character_),
    funder_ids   = join_map(fu, function(e) short_id(e$id %||% NA_character_)),
    funder_rors  = join_map(fu, function(e) e$ror %||% NA_character_),
    
    # --- Institutions paired with country codes (one row per work) ---
    institutions_with_countries = if (!nrow(inst_rows)) NA_character_ else
      paste(sprintf("%s (%s)",
                    inst_rows$name,
                    ifelse(is.na(inst_rows$cc), "??", inst_rows$cc)),
            collapse = "; "),
    
    institution_names   = if (!nrow(inst_rows)) NA_character_ else
      paste(unique(stats::na.omit(inst_rows$name)),    collapse = "; "),
    institution_ids     = if (!nrow(inst_rows)) NA_character_ else
      paste(unique(stats::na.omit(inst_rows$inst_id)), collapse = "; "),
    institution_rors    = if (!nrow(inst_rows)) NA_character_ else
      paste(unique(stats::na.omit(inst_rows$ror)),     collapse = "; "),
    institution_types   = if (!nrow(inst_rows)) NA_character_ else
      paste(unique(stats::na.omit(inst_rows$itype)),   collapse = "; "),
    
    # --- Country codes ---
    # From institution country_code only -- will be narrower than
    institution_country_codes = if (!nrow(inst_rows)) NA_character_ else
      paste(sort(unique(stats::na.omit(inst_rows$cc))), collapse = "; "),
    
    # ---------- classification ----------
    # primary_topic carries field / subfield / domain directly,
    # so there is no need to dig through the topics list.
    topic_id        = short_id(purrr::pluck(pt, "id")),
    topic_name      = s_chr(pt, "display_name"),
    topic_score     = s_dbl(pt, "score"),
    topic_subfield  = s_chr(pt, "subfield", "display_name"),
    topic_field     = s_chr(pt, "field", "display_name"),
    topic_domain    = s_chr(pt, "domain", "display_name"),
    all_topics      = join_map(w$topics, function(e) e$display_name),
    all_fields      = join_map(w$topics, function(e) e$field$display_name),
    keywords        = join_map(w$keywords, function(e) e$display_name),
    concepts        = join_map(w$concepts, function(e) e$display_name),
    sdgs            = join_map(w$sustainable_development_goals,
                               function(e) e$display_name),
    # ---------- impact ----------
    cited_by_count         = s_int(w, "cited_by_count"),
    fwci                   = s_dbl(w, "fwci"),
    cnp_value              = s_dbl(w, "citation_normalized_percentile", "value"),
    referenced_works_count = s_int(w, "referenced_works_count"),
    
    # ---------- flags ----------
    is_retracted = s_lgl(w, "is_retracted"),
    is_paratext  = s_lgl(w, "is_paratext"),
    
    # ---------- optional abstract ----------
    abstract = if (INCLUDE_ABSTRACT)
      undo_inverted(w$abstract_inverted_index) else NA_character_
  )
}

# ============================================================
# STEP 5 -- Build the rectangular frame + integrity checks
# ============================================================
message("\nSTEP 5  Flattening ", format(length(works_raw), big.mark = ","),
        " records...")

ua <- purrr::map_dfr(works_raw, flatten_work)



# Derived convenience columns
ua <- ua |>
  mutate(
    is_oa = dplyr::coalesce(is_oa, oa_status != "closed"),
    oa_binary = if_else(oa_status == "closed", "Not OA", "OA"),
    best_oa_venue_class = case_when(
      !best_oa_exists                     ~ "none (closed)",
      is.na(best_oa_source_type)           ~ "OA, source unknown",
      best_oa_source_type == "repository"  ~ "repository",
      best_oa_source_type %in% c("journal", "conference",
                                 "book series", "ebook platform")
      ~ "publisher",
      TRUE                                 ~ "other"
    ),
    publication_date = as.Date(publication_date),
    pub_month = format(publication_date, "%Y-%m")
  )


# =========================================================
# Collapse any remaining list columns to delimited strings
# This handles fields that flatten_work() returned as lists
# rather than scalars (e.g., nested arrays that join_map
# missed, or fields with unexpected structure)
# =========================================================
list_cols <- names(ua)[vapply(ua, is.list, logical(1))]

if (length(list_cols) > 0) {
  message("Collapsing ", length(list_cols), " remaining list column(s): ",
          paste(list_cols, collapse = ", "))
  
  collapse_to_string <- function(x, sep = "; ") {
    if (is.null(x) || length(x) == 0) return(NA_character_)
    if (is.data.frame(x)) {
      # Nested tibble: prefer display_name, fall back to first column
      name_col <- intersect(
        c("display_name", "name", "title", "id"),
        names(x)
      )
      if (length(name_col) > 0) {
        vals <- unique(stats::na.omit(as.character(x[[name_col[1]]])))
      } else if (ncol(x) > 0) {
        vals <- unique(stats::na.omit(as.character(x[[1]])))
      } else {
        return(NA_character_)
      }
    } else if (is.list(x)) {
      vals <- unique(stats::na.omit(as.character(unlist(x, use.names = FALSE))))
    } else {
      vals <- unique(stats::na.omit(as.character(x)))
    }
    if (length(vals) == 0) return(NA_character_)
    # Excel cells cap at 32,767 characters
    out <- paste(vals, collapse = sep)
    if (nchar(out) > 32000) out <- paste0(substr(out, 1, 32000), " ...[TRUNCATED]")
    out
  }
  
  for (lc in list_cols) {
    ua[[lc]] <- vapply(ua[[lc]], collapse_to_string, character(1))
    message("  collapsed: ", lc)
  }
}

# Now verify — should pass
remaining <- names(ua)[vapply(ua, is.list, logical(1))]
if (length(remaining) > 0) {
  stop("Still have list columns after collapsing: ",
       paste(remaining, collapse = ", "))
} else {
  message("PASS: all columns are scalar. Frame is fully rectangular.")
}
# --- Hard checks ---
stopifnot(!any(vapply(ua, is.list, logical(1))))   # fully rectangular


reconciliation <- tibble(
  check = c("API count_only", "Records fetched", "Difference",
            "Rows flattened", "Unique ids", "Duplicate ids",
            "countries_distinct_count present",
            "institutions_distinct_count present",
            "best_oa_location present (non-null)"),
  value = c(
    expected_n,
    length(works_raw),
    expected_n - length(works_raw),
    nrow(ua),
    dplyr::n_distinct(ua$id),
    nrow(ua) - dplyr::n_distinct(ua$id),
    sum(!is.na(ua$countries_distinct_count)),
    sum(!is.na(ua$institutions_distinct_count)),
    sum(ua$best_oa_exists)
  )
)

message("\nReconciliation:")
print(reconciliation, n = Inf)

if (reconciliation$value[3] != 0)
  warning("Fetched count != API count. Works may have been added/removed mid-pull.")

if (any(duplicated(ua$id))) {
  message("Removing ", sum(duplicated(ua$id)), " duplicate id(s).")
  ua <- distinct(ua, id, .keep_all = TRUE)
}

# The three fields must be populated, or the pull is not fit for purpose
stopifnot(
  sum(!is.na(ua$countries_distinct_count))    > 0.95 * nrow(ua),
  sum(!is.na(ua$institutions_distinct_count)) > 0.95 * nrow(ua)
)
message("PASS: all three target fields populated directly from OpenAlex.")

# --- Sanity: best_oa_location null should track 'closed' ---
message("\nbest_oa_location vs oa_status:")
print(ua |> count(oa_status, best_oa_exists) |> arrange(oa_status))

# --- Sanity: countries != institutions (they are different measures) ---
message("\ncountries vs institutions (first 10 combos):")
print(ua |> count(countries_distinct_count, institutions_distinct_count,
                  sort = TRUE) |> head(10))

# ============================================================
# STEP 6 -- Author-level long table (from raw JSON)
# ============================================================
authors_long <- purrr::map_dfr(works_raw, function(w) {
  au <- w$authorships %||% list()
  if (!length(au)) return(NULL)
  purrr::imap_dfr(au, function(a, i) {
    ins <- a$institutions %||% list()
    tibble(
      work_id          = short_id(w$id),
      doi              = sub("^https?://doi\\.org/", "", s_chr(w, "doi")),
      oa_status        = s_chr(w$open_access, "oa_status"),
      type             = s_chr(w, "type"),
      author_position  = a$author_position %||% NA_character_,
      author_order     = i,
      author_id        = short_id(purrr::pluck(a, "author", "id")),
      author_name      = s_chr(a, "author", "display_name"),
      orcid            = s_chr(a, "author", "orcid"),
      is_corresponding = isTRUE(a$is_corresponding),
      raw_affiliation  = join_map(a$raw_affiliation_strings, function(e) e),
      institution_ids  = join_map(ins, function(e) short_id(e$id)),
      institution_names= join_map(ins, function(e) e$display_name),
      country_codes    = join_map(ins, function(e) e$country_code),
      is_ua            = any(vapply(ins,
                                    function(e) identical(short_id(e$id), UA_ID),
                                    logical(1)))
    )
  })
})

message("\nAuthor-level rows: ", format(nrow(authors_long), big.mark = ","))
message("  UA-affiliated author rows: ", sum(authors_long$is_ua))

# ============================================================
# STEP 7 -- Summaries
# ============================================================
message("\n", strrep("=", 62))
message("SUMMARY STATISTICS")
message(strrep("=", 62))

total_works <- nrow(ua)

# --- 7a: overall OA ---
oa_summary <- ua |>
  count(oa_status, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1))

overall_oa_rate <- round(sum(ua$oa_status != "closed", na.rm = TRUE) /
                           total_works * 100, 1)

message("\nOA status, ALL work types (n = ", format(total_works, big.mark = ","), "):")
print(oa_summary)
message("OVERALL OA RATE: ", overall_oa_rate, "%")

# --- 7b: work types ---
type_summary <- ua |>
  count(type, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1))

message("\nWork types:")
print(type_summary, n = Inf)

# --- 7c: OA rate BY work type ---
oa_by_type <- ua |>
  group_by(type) |>
  summarise(
    n_works  = n(),
    n_oa     = sum(oa_status != "closed", na.rm = TRUE),
    oa_rate  = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
    n_gold    = sum(oa_status == "gold",    na.rm = TRUE),
    n_diamond = sum(oa_status == "diamond", na.rm = TRUE),
    n_hybrid  = sum(oa_status == "hybrid",  na.rm = TRUE),
    n_green   = sum(oa_status == "green",   na.rm = TRUE),
    n_bronze  = sum(oa_status == "bronze",  na.rm = TRUE),
    n_closed  = sum(oa_status == "closed",  na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(n_works))

message("\nOA rate by work type:")
print(oa_by_type, n = Inf)

# --- 7d: THE HEADLINE -- articles only ---
articles <- ua |> filter(type == "article")
n_articles <- nrow(articles)

articles_oa_summary <- articles |>
  count(oa_status, sort = TRUE) |>
  mutate(pct = round(n / sum(n) * 100, 1))

article_oa_rate <- round(sum(articles$oa_status != "closed", na.rm = TRUE) /
                           n_articles * 100, 1)

message("\n", strrep("-", 62))
message("HEADLINE: UA ARTICLES, ", YR)
message(strrep("-", 62))
message("  Total articles  : ", format(n_articles, big.mark = ","))
message("  OA articles     : ",
        format(sum(articles$oa_status != "closed", na.rm = TRUE), big.mark = ","))
message("  ARTICLE OA RATE : ", article_oa_rate, "%")
print(articles_oa_summary)

# --- 7e: discipline (topic_field, straight from primary_topic) ---
oa_by_field <- ua |>
  filter(!is.na(topic_field)) |>
  group_by(discipline = topic_field) |>
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
    .groups = "drop"
  ) |>
  arrange(desc(n_works))

message("\nOA rate by discipline (field), all work types:")
print(oa_by_field, n = 30)

oa_by_field_articles <- ua |>
  filter(type == "article", !is.na(topic_field)) |>
  group_by(discipline = topic_field) |>
  summarise(
    n_articles = n(),
    n_oa       = sum(oa_status != "closed", na.rm = TRUE),
    oa_rate    = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
    .groups = "drop"
  ) |>
  filter(n_articles >= 10) |>       # suppress noise from tiny cells
  arrange(desc(oa_rate))

message("\nARTICLE OA rate by discipline (>=10 articles, ranked):")
print(oa_by_field_articles, n = 30)

# --- 7f: domain (4 buckets, cleaner for exec summary) ---
oa_by_domain <- ua |>
  filter(!is.na(topic_domain)) |>
  group_by(domain = topic_domain) |>
  summarise(
    n_works = n(),
    n_oa    = sum(oa_status != "closed", na.rm = TRUE),
    oa_rate = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
    n_articles = sum(type == "article", na.rm = TRUE),
    article_oa_rate = round(
      sum(type == "article" & oa_status != "closed", na.rm = TRUE) /
        pmax(sum(type == "article", na.rm = TRUE), 1) * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(n_works))

message("\nOA rate by domain:")
print(oa_by_domain)

# --- 7g: work type mix within discipline ---
type_by_field <- ua |>
  filter(!is.na(topic_field)) |>
  count(discipline = topic_field, type) |>
  group_by(discipline) |>
  mutate(pct_within_discipline = round(n / sum(n) * 100, 1)) |>
  ungroup() |>
  arrange(discipline, desc(n))

message("\nWork type mix by discipline (first 25 rows):")
print(type_by_field, n = 25)

# --- 7h: collaboration breadth vs OA (API counts, uncapped) ---
oa_by_collaboration <- ua |>
  dplyr::mutate(band = dplyr::case_when(
    is.na(countries_distinct_count) ~ "unknown",
    countries_distinct_count <= 1   ~ "1 country",
    countries_distinct_count == 2   ~ "2 countries",
    countries_distinct_count <= 5   ~ "3-5 countries",
    countries_distinct_count <= 10  ~ "6-10 countries",
    TRUE                            ~ "11+ countries"
  )) |>
  dplyr::group_by(band) |>
  dplyr::summarise(
    n_works = dplyr::n(),
    oa_rate = round(sum(oa_status != "closed", na.rm = TRUE) / dplyr::n() * 100, 1),
    .groups = "drop"
  )

message("\nOA rate by international collaboration breadth:")
print(oa_by_collaboration)

# --- 7i: institutional collaboration bands ---
oa_by_institutions <- ua |>
  mutate(inst_band = case_when(
    is.na(institutions_distinct_count) ~ "unknown",
    institutions_distinct_count <= 1   ~ "1 institution",
    institutions_distinct_count <= 3   ~ "2-3",
    institutions_distinct_count <= 10  ~ "4-10",
    institutions_distinct_count <= 25  ~ "11-25",
    TRUE                               ~ "26+"
  )) |>
  group_by(inst_band) |>
  summarise(
    n_works = n(),
    oa_rate = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
    .groups = "drop"
  )

message("\nOA rate by number of contributing institutions:")
print(oa_by_institutions)

# --- 7j: where the OA copies live (best_oa_location) ---
best_oa_venue_summary <- ua |>
  count(best_oa_venue_class, sort = TRUE) |>
  mutate(pct_of_all_works = round(n / sum(n) * 100, 1))

message("\nBest OA location: publisher vs repository:")
print(best_oa_venue_summary)

best_oa_version_summary <- ua |>
  filter(best_oa_exists) |>
  count(best_oa_version, best_oa_venue_class, sort = TRUE) |>
  mutate(pct_of_oa = round(n / sum(n) * 100, 1))

message("\nBest OA location: version x venue:")
print(best_oa_version_summary, n = 20)

best_oa_license_summary <- ua |>
  filter(best_oa_exists) |>
  mutate(lic = tidyr::replace_na(best_oa_license, "(none recorded)")) |>
  count(lic, sort = TRUE) |>
  rename(best_oa_license = lic) |>
  mutate(pct_of_oa = round(n / sum(n) * 100, 1))

message("\nBest OA location: licenses (blank = bronze-style, no licence):")
print(best_oa_license_summary, n = 20)

best_oa_source_summary <- ua |>
  filter(!is.na(best_oa_source_name)) |>
  count(best_oa_source_name, best_oa_source_type, sort = TRUE) |>
  head(50)

message("\nTop 50 best-OA sources:")
print(best_oa_source_summary, n = 50)

# --- 7k: APC spend where recorded ---
apc_summary <- ua |>
  filter(!is.na(apc_paid_value_usd)) |>
  group_by(oa_status) |>
  summarise(
    n_with_apc     = n(),
    total_usd      = sum(apc_paid_value_usd, na.rm = TRUE),
    median_usd     = median(apc_paid_value_usd, na.rm = TRUE),
    mean_usd       = round(mean(apc_paid_value_usd, na.rm = TRUE)),
    max_usd        = max(apc_paid_value_usd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_usd))

message("\nAPC paid (where OpenAlex records a value):")
print(apc_summary)
message("  NOTE: apc_paid coverage is partial -- treat as a floor, not a total.")

# --- 7l: monthly volume, for indexing-lag context ---
by_month <- ua |>
  filter(!is.na(pub_month)) |>
  count(pub_month) |>
  arrange(pub_month) |>
  mutate(cumulative = cumsum(n))

message("\nWorks per publication month:")
print(by_month, n = Inf)

# --- 7m: UA authorship role ---
ua_role_summary <- ua |>
  summarise(
    n_works                 = n(),
    ua_first_author         = sum(ua_is_first_author, na.rm = TRUE),
    ua_corresponding        = sum(ua_is_corresponding, na.rm = TRUE),
    median_ua_authors       = median(ua_author_count, na.rm = TRUE),
    zero_ua_authors         = sum(ua_author_count == 0),
    recovered_by_lineage    = sum(ua_author_count > 0 & ua_exact_parent_count == 0),
 
  )

message("\nUA authorship role:")
print(ua_role_summary)

message("\nRemaining zeros by reason:")
print(ua |> filter(ua_author_count == 0) |> count(ua_detect_zero_reason))


# ============================================================
# STEP 8 -- Provenance tables
# ============================================================

run_metadata <- tibble(
  field = c(
    "date_pulled",
    "time_pulled_local",
    "timezone",
    "data_source",
    "api_base_url",
    "institution_openalex_id",
    "institution_name",
    "publication_year",
    "date_filter",
    "full_filter_string",
    "oa_filter_applied",
    "api_count_only",
    "records_fetched",
    "rows_in_final_table",
    "unique_openalex_ids",
    "fields_requested",
    "fields_rejected_by_api",
    "countries_distinct_count_source",
    "institutions_distinct_count_source",
    "best_oa_location_source",
    "discipline_column",
    "abstracts_included",
    "per_page",
    "pagination_method",
    "overall_oa_rate_pct",
    "total_articles",
    "article_oa_rate_pct",
    "mailto_used",
    "api_key_used",
    "R_version",
    "platform",
    "script_name",
    "caveat_author_cap",
    "caveat_partial_year",
    "caveat_apc_coverage",
    "notes"
  ),
  value = c(
    format(as.Date(date_pulled), "%Y-%m-%d"),
    format(date_pulled, "%Y-%m-%d %H:%M:%S"),
    Sys.timezone(),
    "OpenAlex REST API, direct JSON (openalexR NOT used)",
    BASE,
    UA_ID,
    "University of Arizona",
    as.character(YR),
    paste0(YR, "-01-01 to ", YR, "-12-31"),
    FILTER,
    "NONE - all works returned, OA and closed",
    format(expected_n, big.mark = ","),
    format(length(works_raw), big.mark = ","),
    format(nrow(ua), big.mark = ","),
    format(dplyr::n_distinct(ua$id), big.mark = ","),
    paste(FIELDS, collapse = ", "),
    if (nrow(rejected)) paste(rejected$field, collapse = ", ") else "none",
    "OpenAlex countries_distinct_count (NOT calculated locally)",
    "OpenAlex institutions_distinct_count (NOT calculated locally)",
    "OpenAlex best_oa_location, flattened from raw JSON",
    "topic_field / topic_domain, from primary_topic",
    as.character(INCLUDE_ABSTRACT),
    as.character(PER_PAGE),
    "cursor paging (cursor=* then meta.next_cursor)",
    as.character(overall_oa_rate),
    format(n_articles, big.mark = ","),
    as.character(article_oa_rate),
    MAILTO,
    if (nzchar(API_KEY)) "yes" else "no (polite pool via mailto)",
    R.version.string,
    R.version$platform,
    "ua_all_works_2025_direct.R",
    paste0("OpenAlex caps the authorships array at the first 100 authors. ",
           "This affects authors_listed_in_payload and the ua_* flags only. ",
           "countries_distinct_count and institutions_distinct_count are ",
           "computed upstream by OpenAlex and are NOT capped."),
    paste0(YR, " is recent: indexing and OA status (especially green OA ",
           "via repositories) continue to accrue after this pull. ",
           "The OA rate here is a FLOOR, not a final figure."),
    paste0("apc_paid is populated for only a subset of works. ",
           "Treat APC totals as a lower bound."),
    paste0("Full-universe pull. Denominator = all UA-affiliated works, ",
           "so OA percentages are valid. Raw JSON preserved in ",
           prefix, "_raw_json.rds for re-derivation without re-querying.")
  )
)

message("\nRun metadata:")
print(run_metadata, n = Inf)

# --- Data dictionary / column inventory ---
column_inventory <- tibble(
  column    = names(ua),
  class     = vapply(ua, function(x) class(x)[1], character(1)),
  n_missing = vapply(ua, function(x) sum(is.na(x)), integer(1)),
  n_unique  = vapply(ua, function(x) dplyr::n_distinct(x), integer(1))
) |>
  mutate(pct_missing = round(n_missing / nrow(ua) * 100, 1)) |>
  arrange(desc(pct_missing))

message("\nColumns with >50% missing (check these are expected):")
print(column_inventory |> filter(pct_missing > 50), n = Inf)

# --- Field probe log: evidence of what the API accepted ---
field_probe_log <- probe_tbl |>
  mutate(
    note = case_when(
      !accepted                ~ "rejected by API",
      accepted & !key_returned ~ "accepted; null on sampled record",
      TRUE                     ~ "accepted and populated"
    )
  )

# ============================================================
# STEP 9 -- Write outputs
# ============================================================
# =========================================================
# Build the data dictionary as a data frame
# =========================================================

data_dictionary <- tibble::tribble(
  ~column_name, ~display_name, ~data_type, ~source, ~definition, ~null_means,
  
  # ================ IDENTITY ================
  "id", "OpenAlex Work ID", "string", "API direct",
  "Unique OpenAlex identifier, e.g. W4415739261.",
  "Should not be null",
  
  "openalex_url", "OpenAlex URL", "string", "API direct",
  "Full URL to this work on openalex.org.",
  "Should not be null",
  
  "doi", "DOI", "string", "API direct, prefix stripped",
  "Digital Object Identifier without the https://doi.org/ prefix. Where a work has both preprint and published DOIs, this is the published one.",
  "No DOI registered (common for datasets, dissertations, repository records)",
  
  "pmid", "PubMed ID", "string", "API via ids object",
  "PubMed identifier, URL prefix stripped.",
  "Not indexed in PubMed",
  
  "pmcid", "PubMed Central ID", "string", "API via ids object",
  "PubMed Central identifier.",
  "No PMC deposit exists",
  
  "title", "Title", "string", "API direct",
  "Title of the work as recorded by OpenAlex.",
  "Rare; some repository records lack titles",
  
  "publication_year", "Publication Year", "integer", "API direct",
  "Year of publication. All records in this dataset are 2025.",
  "Should not be null in this dataset",
  
  "publication_date", "Publication Date", "date", "API direct",
  "Full publication date (ISO 8601). Where several dates exist, OpenAlex generally uses the earliest electronic publication date.",
  "Date could not be determined beyond the year",
  
  "language", "Language", "string", "API direct",
  "Auto-detected language of title and abstract, ISO 639-1 two-letter code.",
  "Language could not be detected (insufficient text)",
  
  "type", "Work Type", "string", "API direct",
  "Type of scholarly output (article, book-chapter, dataset, preprint, review, dissertation, etc.), assigned by OpenAlex's classifier.",
  "Should not be null",
  
  "raw_type", "Raw Type (from source)", "string", "API direct",
  "Type label as supplied by the registering source (Crossref, DataCite, repository), before OpenAlex reclassification.",
  "Source did not supply a type",
  
  # ================ OPEN ACCESS ================
  "is_oa", "Is Open Access", "logical", "API via open_access",
  "Whether any free, legal full-text copy exists anywhere. OpenAlex uses a broad definition: readable without paying or logging in.",
  "Should not be null",
  
  "oa_status", "OA Status", "string", "API via open_access",
  "OA classification. diamond = fully OA journal, no APC. gold = fully OA journal. hybrid = open-licensed article in a toll journal. green = toll at publisher, free copy in a repository. bronze = free at publisher, no open license. closed = no free copy found.",
  "Should not be null",
  
  "oa_binary", "OA Binary", "string", "Derived in script",
  "Simplified two-value grouping: everything except 'closed' becomes 'OA'.",
  "Should not be null",
  
  "oa_url", "Best OA URL", "string", "API via open_access",
  "URL of the best available OA copy. Same target as best_oa_location.",
  "Work is closed",
  
  "any_repository_has_fulltext", "Repository Has Fulltext", "logical", "API via open_access",
  "Whether at least one repository holds a full text. Surfaces repository deposits that oa_status hides when a publisher copy also exists.",
  "Should not be null",
  
  # ================ BEST OA LOCATION ================
  
  "best_oa_exists", "Best OA Location Exists", "logical", "Derived from API",
  "Whether best_oa_location was non-null in the API response. FALSE for closed works by design — not missing data.",
  "Never null (TRUE/FALSE only)",
  
  "best_oa_is_oa", "Best OA Is OA", "logical", "API via best_oa_location",
  "Should always be TRUE when a best OA location exists.",
  "No OA location (work is closed)",
  
  "best_oa_version", "Best OA Version", "string", "API via best_oa_location",
  "Manuscript version at the best OA copy. Scoring prefers publishedVersion, then acceptedVersion, then submittedVersion.",
  "No OA location (work is closed)",
  
  "best_oa_license", "Best OA License", "string", "API via best_oa_location",
  "License on the best OA copy. OpenAlex counts even restrictive Creative Commons variants (cc-by-nc-nd) as open.",
  "No license identified. For bronze works this is INFORMATIVE, not missing: free to read with no open license is precisely what defines bronze.",
  
  "best_oa_license_id", "Best OA License ID", "string", "API via best_oa_location",
  "The license identifier string (e.g. 'cc-by-4.0').",
  "No license identified",
  
  "best_oa_is_accepted", "Best OA Is Accepted Version", "logical", "API via best_oa_location",
  "Whether the best OA location holds an accepted manuscript.",
  "No OA location",
  
  "best_oa_is_published", "Best OA Is Published Version", "logical", "API via best_oa_location",
  "Whether the best OA location holds the published version.",
  "No OA location",
  
  "best_oa_landing_page_url", "Best OA Landing Page", "string", "API via best_oa_location",
  "Landing page URL of the best OA copy.",
  "No OA location",
  
  "best_oa_pdf_url", "Best OA PDF URL", "string", "API via best_oa_location",
  "Direct PDF link for the best OA copy.",
  "No direct PDF link (landing page may still provide access via embedded reader)",
  
  "best_oa_source_id", "Best OA Source ID", "string", "API via best_oa_location.source",
  "OpenAlex ID of the source (journal or repository) hosting the best OA copy.",
  "Source could not be identified",
  
  "best_oa_source_name", "Best OA Source Name", "string", "API via best_oa_location.source",
  "Display name of the source hosting the best OA copy (e.g. 'PubMed Central', 'arXiv', 'PLOS ONE').",
  "Source could not be identified",
  
  "best_oa_source_type", "Best OA Source Type", "string", "API via best_oa_location.source",
  "Whether the best OA copy is at a publisher venue or in a repository. Preprint servers count as 'repository'.",
  "Source could not be identified",
  
  "best_oa_source_issn_l", "Best OA Source ISSN-L", "string", "API via best_oa_location.source",
  "Linking ISSN of the source hosting the best OA copy.",
  "Source is not a serial (e.g. a repository)",
  
  "best_oa_source_is_in_doaj", "Best OA Source in DOAJ", "logical", "API via best_oa_location.source",
  "Whether the source is listed in the Directory of Open Access Journals.",
  "Source could not be identified",
  
  "best_oa_host_org", "Best OA Host Organization", "string", "API via best_oa_location.source",
  "Publisher or organization operating the source hosting the best OA copy.",
  "Not recorded",
  
  "best_oa_venue_class", "Best OA Venue Class", "string", "Derived in script",
  "Simplified grouping of best_oa_source_type: 'publisher', 'repository', 'OA source unknown', 'none (closed)', or 'other'.",
  "Should not be null",
  
  # ================ PRIMARY LOCATION ================
  
  "primary_source_id", "Primary Source ID", "string", "API via primary_location.source",
  "OpenAlex ID of the journal, conference, or repository where the work was published.",
  "Source not identified",
  
  "primary_source_name", "Primary Source Name (Journal Title)", "string", "API via primary_location.source",
  "Display name of the primary source — THIS IS THE JOURNAL TITLE (e.g. 'Nature', 'PLOS ONE', 'arXiv').",
  "Source not identified",
  
  "source_display_name", "Journal Title (alias)", "string", "Alias for primary_source_name",
  "Alias for primary_source_name. Equivalent to 'so' or 'source_display_name' in openalexR output. Included for backward compatibility with earlier data pulls.",
  "Same as primary_source_name",
  
  "primary_source_type", "Primary Source Type", "string", "API via primary_location.source",
  "Type of the primary venue: journal, repository, conference, book series, ebook platform.",
  "Source not identified",
  
  "primary_source_issn_l", "Primary Source ISSN-L", "string", "API via primary_location.source",
  "Linking ISSN of the primary source.",
  "Not a serial",
  
  "primary_source_is_in_doaj", "Primary Source in DOAJ", "logical", "API via primary_location.source",
  "Whether the primary source is listed in DOAJ. TRUE for gold/diamond journals.",
  "Source not identified",
  
  "primary_publisher", "Publisher", "string", "API via primary_location.source",
  "Name of the host organization — THIS IS THE PUBLISHER (e.g. 'Elsevier', 'Springer Nature', 'MDPI'). Extracted from primary_location.source.host_organization_name.",
  "Not recorded",
  
  "host_organization_name", "Publisher (alias)", "string", "Alias for primary_publisher",
  "Alias for primary_publisher. Equivalent to 'host_organization' in openalexR output. Included for backward compatibility with earlier data pulls.",
  "Same as primary_publisher",
  
  "primary_is_oa", "Primary Location Is OA", "logical", "API via primary_location",
  "Whether the primary (publisher) copy itself is freely available.",
  "No primary location",
  
  "primary_version", "Primary Location Version", "string", "API via primary_location",
  "Version type at the primary location: publishedVersion, acceptedVersion, submittedVersion.",
  "Not recorded",
  
  "primary_license", "Primary Location License", "string", "API via primary_location",
  "License at the primary location.",
  "No license identified",
  
  "primary_landing_page_url", "Primary Landing Page", "string", "API via primary_location",
  "Publisher landing page URL.",
  "Not recorded",
  
  "primary_pdf_url", "Primary PDF URL", "string", "API via primary_location",
  "Direct PDF link at the publisher.",
  "No direct PDF available",
  
  "locations_count", "Total Locations Count", "integer", "API direct",
  "How many distinct locations (publisher + repositories) OpenAlex found for this work.",
  "Should not be null",
  
  "indexed_in", "Indexed In", "string", "Derived: collapsed list",
  "Which indexes carry this work, e.g. 'crossref; doaj; pubmed'.",
  "No indexing information",
  
  "has_fulltext", "Has Fulltext", "logical", "API direct",
  "Whether OpenAlex has been able to access the full text of this work (for abstract extraction, topic classification, etc.).",
  "Should not be null",
  

  # ================ INSTITUTIONS & COUNTRIES ================
  
  "institutions_distinct_count", "Distinct Author Institutions", "integer", "API direct — AUTHORITATIVE",
  "Number of distinct institutions across all authorships on this work, as computed by OpenAlex. Verified against local extraction on all 9,740 works: 96.8% exact agreement. Disagreements concentrate on works with 100+ listed authors, where the served authorships array is capped but this count appears to be computed over the complete record (136 of 137 large gaps are at the cap). OpenAlex documentation does not state whether this count inherits the cap; the behaviour was determined empirically. Twelve works show the local count exceeding the API count, consistent with umbrella organizations flagged is_super_system being excluded from certain analyses. Use this column for any reported figure.",
  "No authorship data available",
  
  "countries_distinct_count", "Distinct Author Countries", "integer", "API direct — AUTHORITATIVE",
  "Number of distinct author country codes across all authorships, as computed by OpenAlex. 98.5% exact agreement with local extraction. Same cap behaviour as institutions_distinct_count.",
  "No authorship data available",
  
  "institutions_with_countries", "Institutions with Country Codes", "string", "Derived: paired from authorships[].institutions[]",
  "Every distinct institution on the work paired with its ISO 3166-1 alpha-2 country code, semicolon-delimited. Example: 'University of Arizona (US); ETH Zurich (CH); Kyoto University (JP)'. Built as PAIRED strings deliberately — deduplicating names and codes into separate columns loses the mapping. '(??)' marks an institution with no country code.",
  "No institutions matched on any authorship",
  
  "institution_names", "Institution Names", "string", "Derived: collapsed from authorships[].institutions[]",
  "Distinct institution display names, semicolon-delimited. Deduplicated on institution ID, so an institution appearing under slightly different display names is counted once.",
  "No institutions matched on any authorship",
  
  "institution_ids", "Institution OpenAlex IDs", "string", "Derived: collapsed from authorships[].institutions[]",
  "Distinct OpenAlex institution IDs (I-prefixed), semicolon-delimited. Useful for joining to the institutions endpoint or for programmatic filtering.",
  "No institutions matched on any authorship",
  
  "institution_rors", "Institution ROR IDs", "string", "Derived: collapsed from authorships[].institutions[]",
  "Distinct Research Organization Registry identifiers, semicolon-delimited. ROR is the external registry OpenAlex inherits institutional identity and hierarchy from.",
  "No institutions matched, or matched institutions carry no ROR record",
  
  "institution_types", "Institution Types", "string", "Derived: collapsed from authorships[].institutions[]",
  "Distinct institution types on this work: education, healthcare, company, nonprofit, government, facility, archive, funder, other. Useful for separating academic from industry or clinical collaboration.",
  "No institutions matched on any authorship",
  
  "institution_country_codes", "Institution Country Codes", "string", "Derived: collapsed from authorships[].institutions[].country_code",
  "Distinct country codes taken ONLY from matched institutions, semicolon-delimited and sorted. Deliberately narrower than authorship_countries. Where this is blank but authorship_countries is populated, OpenAlex read a country from the address but could not place the institution.",
  "No institutions matched, or matched institutions carry no country code",
  
  "inst_count_gap", "Institution Count Gap (QA)", "integer", "Derived in script",
  "institutions_distinct_count minus n_institutions_listed. Zero on 96.8% of works. Large positive = hyperauthorship truncation. Small negative = possible super-system exclusion. QA column.",
  "Should not be null",
  
  # ================ AUTHORSHIP ================
  
  "authors_listed_in_payload", "Authors in Payload", "integer", "Derived: length of authorships array",
  "Number of authors present in the served JSON. CAPPED AT 100 — OpenAlex states the authorships list is capped at the first 100 authors.",
  "Should not be null",
  
  "authors_count_api", "Authors Count (API)", "integer", "API filter-only attribute",
  "Total author count as reported by the API. Note: this is a filter-only attribute and may not populate via select.",
  "Not returned by API for this work",
  
  "ua_author_count", "UA Author Count", "integer", "Derived in script via inst_is_ua()",
  "Number of authors matched to UA via institution ID, ROR, or lineage. CAN BE 0 even though the work matched the UA filter — see ua_detect_zero_reason. Causes include parser misses (~92% recall), country-only resolutions, and over-merges. OpenAlex's blog uses UA's Optical Sciences Center as a worked example of the over-merge problem.",
  "Should not be null (but can be 0)",
  
  "ua_exact_parent_count", "UA Exact Parent Match Count", "integer", "Derived in script",
  "Number of authors whose institution ID is exactly I138006243 (parent UA), not via lineage or ROR fallback. Useful for distinguishing parent from child-institution matches.",
  "Should not be null",
  
  "n_raw_affil_strings", "Raw Affiliation Strings Count", "integer", "Derived in script",
  "How many raw affiliation text strings were present across all authorships. When 0, there is no text for the regex fallback to search.",
  "Should not be null",
  
  "ua_in_raw_text", "UA in Raw Affiliation Text", "logical", "Derived in script via regex",
  "Whether a regex pattern for UA name variants matched anywhere in the raw affiliation strings. TRUE with ua_author_count == 0 means 'UA is printed on the paper but OpenAlex did not link it' — a directly actionable parser miss.",
  "Should not be null",
  
  "ua_detect_zero_reason", "UA Zero-Count Reason", "string", "Derived in script",
  "Classifies why ua_author_count is 0. Values: 'no authorships in payload', 'no affiliation text in payload', 'UA in raw string, not linked (parser miss/over-merge)', 'affiliation resolved to country only', 'no UA signal in payload'. NA when count >= 1.",
  "ua_author_count >= 1 (column is NA when count is positive)",
  
  "ua_is_first_author", "UA Is First Author", "logical", "Derived in script",
  "Whether the first-listed author is affiliated with UA.",
  "Should not be null",
  
  "ua_is_corresponding", "UA Is Corresponding", "logical", "Derived in script",
  "Whether UA appears in the work's corresponding_institution_ids.",
  "Should not be null",
  
  # ================ FUNDING ================
  
  "apc_list_value_usd", "APC List Price (USD)", "integer", "API via apc_list",
  "The journal's listed article processing charge in USD. Source is DOAJ only. VALUE OF 0 = DIAMOND OA (journal charges no APC). NULL does NOT mean free — it means the price is unknown because the journal is not in DOAJ.",
  "Journal not in DOAJ; list price unknown",
  
  "apc_list_currency", "APC List Currency", "string", "API via apc_list",
  "Original currency of the list APC.",
  "Not available",
  
  "apc_paid_value_usd", "APC Paid (USD)", "integer", "API via apc_paid",
  "The APC actually paid, in USD. Prefers article-level data from OpenAPC; falls back to list price. COVERAGE IS PARTIAL — treat as a lower bound. VALUE OF 0 = waiver, diamond, or covered by agreement. NULL does not mean free — for gold/hybrid works an APC was almost certainly paid but the amount is unknown.",
  "No payment data from any source (most common case)",
  
  "apc_paid_provenance", "APC Paid Provenance", "string", "API via apc_paid",
  "Where the apc_paid figure came from: 'openapc' (article-level, most reliable) or 'doaj' (fell back to journal list price). For verified data only, filter to provenance == 'openapc'.",
  "No data from any source",
  
  "n_awards", "Number of Awards", "integer", "Derived: length of awards array",
  "How many distinct awards (grants) are linked to this work in OpenAlex. The awards array replaced the retired grants field on the Work object.",
  "Should not be null (0 means no awards linked)",
  
  "award_ids", "Award/Grant IDs", "string", "Derived: collapsed from awards[].funder_award_id",
  "The funder's own grant/award numbers (e.g. 'R01-AG018016; CDS9923G0006'), semicolon-delimited. Extracted from awards[].funder_award_id, which replaced the retired grants[].award_id. Only ~32% of works in this dataset carry award data — this reflects OpenAlex's current coverage, not a code issue. Blank means no funding metadata was deposited by the publisher or mined from the full text, not that the work was unfunded.",
  "No award numbers recorded",
  
  "award_openalex_ids", "Award OpenAlex IDs", "string", "Derived: collapsed from awards[].id",
  "OpenAlex award IDs (G-prefixed), semicolon-delimited. Useful for joining to the awards endpoint (api.openalex.org/awards), which carries additional fields including institution_awarded and amount_usd.",
  "No awards linked",
  
  "award_dois", "Award DOIs", "string", "Derived: collapsed from awards[].doi",
  "DOIs registered for awards/grants, where the funder has registered one.",
  "No award DOIs registered (most common case)",
  
  "award_funder_names", "Award Funder Names", "string", "Derived: collapsed from awards[].funder_display_name",
  "Funder names as recorded on each award, semicolon-delimited. Note: funder attribution on awards is subject to ongoing disambiguation; the National Natural Science Foundation of China (NNSFC) in particular has a documented history of over-attribution in OpenAlex.",
  "No awards linked",
  
  "award_funder_ids", "Award Funder IDs", "string", "Derived: collapsed from awards[].funder_id",
  "OpenAlex funder IDs (F-prefixed) from the awards array.",
  "No awards linked",
  
  "n_funders", "Number of Funders", "integer", "Derived: length of funders array",
  "How many distinct funding organizations are credited on this work. The funders array is SEPARATE from awards — funders lists organizations, awards lists specific grants.",
  "Should not be null (0 means no funders credited)",
  
  "funder_names", "Funder Names", "string", "Derived: collapsed from funders[].display_name",
  "Display names of all funding organizations, semicolon-delimited.",
  "No funder information",
  
  "funder_ids", "Funder IDs", "string", "Derived: collapsed from funders[].id",
  "OpenAlex funder IDs (F-prefixed), semicolon-delimited.",
  "No funder information",
  
  "funder_rors", "Funder ROR IDs", "string", "Derived: collapsed from funders[].ror",
  "ROR identifiers for funding organizations.",
  "No funder information, or funders carry no ROR record",
  
  # ================ CLASSIFICATION ================
  
  "topic_id", "Primary Topic ID", "string", "API via primary_topic",
  "OpenAlex ID of the work's primary (highest-scoring) topic. Topics are assigned by a machine-learning model from the work's title, abstract, citations, and journal — they are predictions, not lookups.",
  "Work could not be classified (~12% of works lack sufficient metadata)",
  
  "topic_name", "Primary Topic Name", "string", "API via primary_topic",
  "Human-readable name of the primary topic, generated by an LLM from the topic's citation cluster.",
  "Work could not be classified",
  
  "topic_score", "Primary Topic Score", "numeric", "API via primary_topic",
  "Confidence score of the primary topic assignment (0.0-1.0).",
  "Work could not be classified",
  
  "topic_subfield", "Subfield", "string", "API via primary_topic.subfield",
  "The subfield the primary topic maps to — one level up in the 4-level hierarchy (~252 subfields).",
  "Work could not be classified",
  
  "topic_field", "Field (Discipline)", "string", "API via primary_topic.field",
  "The field (discipline) the primary topic maps to — used for discipline-level OA rate comparisons (~26 fields). Multi-disciplinary works are assigned to one field only via their primary topic; use all_fields for a fuller picture.",
  "Work could not be classified",
  
  "topic_domain", "Domain", "string", "API via primary_topic.domain",
  "The broadest level of the topic hierarchy: Health Sciences, Life Sciences, Physical Sciences, or Social Sciences.",
  "Work could not be classified",
  
  "all_topics", "All Assigned Topics", "string", "Derived: collapsed from topics list",
  "Names of all topics assigned (up to 3), semicolon-delimited.",
  "No topics assigned",
  
  "all_fields", "All Assigned Fields", "string", "Derived: collapsed from topics list",
  "Fields from all assigned topics — useful for identifying multi-disciplinary works.",
  "No topics assigned",
  
  "keywords", "Keywords", "string", "Derived: collapsed from keywords list",
  "Work-level keywords scored per work, distinct from topic-level keywords.",
  "No keywords assigned",
  
  "concepts", "Concepts (Deprecated)", "string", "Derived: collapsed from concepts list",
  "Legacy classification inherited from Microsoft Academic Graph. DEPRECATED — no longer maintained; use topics instead.",
  "No concepts assigned",
  
  "sdgs", "Sustainable Development Goals", "string", "Derived: collapsed from SDGs list",
  "UN Sustainable Development Goals this work is relevant to, assigned by a classification model.",
  "No SDGs assigned",
  
  # ================ IMPACT ================
  
  "cited_by_count", "Times Cited", "integer", "API direct",
  "Total number of works in OpenAlex that cite this work. Where a publisher does not deposit open references with Crossref, those citations are invisible to OpenAlex.",
  "Should not be null (0 means no citations yet)",
  
  "fwci", "Field-Weighted Citation Impact", "numeric", "API direct",
  "Normalized citation score: citations received divided by expected citations for works of the same type, field, and year. 1.0 = world average. Recalculated inline by OpenAlex.",
  "Too recent or insufficient data to calculate",
  
  "cnp_value", "Citation Normalized Percentile", "numeric", "API via citation_normalized_percentile",
  "Where this work falls in the citation distribution for its type, field, and year. 0.95 means more cited than 95% of comparable works.",
  "Too recent or insufficient data to calculate",
  
  "referenced_works_count", "References Count", "integer", "API direct",
  "Number of works this work cites in its reference list.",
  "Should not be null (0 means no references recorded)",
  
  # ================ FLAGS ================
  
  "is_retracted", "Is Retracted", "logical", "API direct",
  "Whether this work has been retracted, per the Retraction Watch database. Distinct from type = 'retraction', which identifies retraction notices.",
  "Should not be null",
  
  "is_paratext", "Is Paratext", "logical", "API direct",
  "Whether this work is supplementary material (table of contents, editorial board listing, etc.) rather than substantive research. Deprecated — now derived from type.",
  "Should not be null",
  
  # ================ ABSTRACT ================
  
  "abstract", "Abstract", "string", "Derived: reconstructed from abstract_inverted_index",
  "Full abstract text, reconstructed from the inverted index. Only populated when INCLUDE_ABSTRACT = TRUE in the script config (default is FALSE, which roughly triples payload size). Not all works have abstracts — coverage is approximately 60% for recent works.",
  "INCLUDE_ABSTRACT was FALSE (default), or the work has no abstract in OpenAlex",
  
  # ================ TEMPORAL ================
  
  "pub_month", "Publication Month", "string", "Derived from publication_date",
  "Year-month derived from publication_date (YYYY-MM format). Useful for time-series and detecting indexing lag: if volume tails off in recent months, that is lag, not a real publishing decline.",
  "publication_date is null"
)

message("Data dictionary built: ", nrow(data_dictionary), " columns documented")

# --- 9a: Column ordering, analysis columns first ---
front <- c(
  "id", "doi", "title", "publication_date", "publication_year",
  "type", "language",
  "is_oa", "oa_status", "oa_binary", "oa_url",
  "best_oa_exists", "best_oa_venue_class", "best_oa_version",
  "best_oa_license", "best_oa_source_name", "best_oa_source_type",
  "best_oa_pdf_url", "best_oa_landing_page_url",
  "institutions_with_countries", "institution_names",
  "institution_country_codes", 
  
  "institutions_distinct_count",
  "countries_distinct_count",

  "topic_domain", "topic_field", "topic_subfield", "topic_name",
  "primary_source_name", "primary_publisher", "primary_source_is_in_doaj",
  "cited_by_count", "fwci",
  "ua_author_count", "ua_exact_parent_count", "ua_detect_zero_reason",
  "ua_is_first_author", "ua_is_corresponding",
  "authors_listed_in_payload", 
  "apc_paid_value_usd", "apc_list_value_usd"
)
front <- intersect(front, names(ua))

ua_out <- ua |> select(all_of(front), everything())

# Validate here
documented <- data_dictionary$column_name
actual     <- names(ua_out)

missing_from_data <- setdiff(documented, actual)
undocumented      <- setdiff(actual, documented)

if (length(missing_from_data)) {
  message("WARNING — documented but not in data: ",
          paste(missing_from_data, collapse = ", "))
}
if (length(undocumented)) {
  message("WARNING — in data but undocumented: ",
          paste(undocumented, collapse = ", "))
}
if (!length(missing_from_data) && !length(undocumented)) {
  message("PASS — data dictionary and dataset columns match exactly")
}


# --- 9b: Lossless saves ---
saveRDS(ua,           paste0(prefix, "_works.rds"))
saveRDS(authors_long, paste0(prefix, "_authors_long.rds"))
message("\nSaved: ", prefix, "_works.rds")
message("Saved: ", prefix, "_authors_long.rds")
message("Saved earlier: ", prefix, "_raw_json.rds  (raw API response)")

# --- 9c: CSVs ---
csv_targets <- list(
  works                = ua_out,
  authors_long         = authors_long,
  metadata             = run_metadata,
  reconciliation       = reconciliation,
  field_probe_log      = field_probe_log,
  column_inventory     = column_inventory,
  oa_summary           = oa_summary,
  type_summary         = type_summary,
  oa_by_type           = oa_by_type,
  articles_oa          = articles_oa_summary,
  oa_by_field          = oa_by_field,
  oa_by_field_articles = oa_by_field_articles,
  oa_by_domain         = oa_by_domain,
  type_by_field        = type_by_field,
  oa_by_collaboration  = oa_by_collaboration,
  oa_by_institutions   = oa_by_institutions,
  best_oa_venue        = best_oa_venue_summary,
  best_oa_version      = best_oa_version_summary,
  best_oa_license      = best_oa_license_summary,
  best_oa_sources      = best_oa_source_summary,
  apc_summary          = apc_summary,
  by_month             = by_month,
  ua_role_summary      = ua_role_summary
)

csv_targets <- csv_targets[
  !vapply(csv_targets, function(x) is.null(x) || nrow(x) == 0, logical(1))
]

message("\nWriting ", length(csv_targets), " CSVs...")
for (nm in names(csv_targets)) {
  f <- paste0(prefix, "_", nm, ".csv")
  readr::write_csv(csv_targets[[nm]], f, na = "")
  message("  ", f, "  (", format(nrow(csv_targets[[nm]]), big.mark = ","), " rows)")
}

# --- 9d: Multi-sheet XLSX, provenance tab first ---
xlsx_file <- paste0(prefix, ".xlsx")

sheets <- list(
  "README_date_pulled"   = run_metadata,
  "data_dictionary"      = data_dictionary, 
  "all_works"            = ua_out,
  "oa_summary"           = oa_summary,
  "type_summary"         = type_summary,
  "oa_by_type"           = oa_by_type,
  "articles_oa"          = articles_oa_summary,
  "oa_by_domain"         = oa_by_domain,
  "oa_by_field"          = oa_by_field,
  "oa_by_field_articles" = oa_by_field_articles,
  "type_by_field"        = type_by_field,
  "oa_by_collaboration"  = oa_by_collaboration,
  "oa_by_institutions"   = oa_by_institutions,
  "best_oa_venue"        = best_oa_venue_summary,
  "best_oa_version"      = best_oa_version_summary,
  "best_oa_license"      = best_oa_license_summary,
  "best_oa_sources"      = best_oa_source_summary,
  "apc_summary"          = apc_summary,
  "ua_role_summary"      = ua_role_summary
)

sheets <- sheets[
  !vapply(sheets, function(x) is.null(x) || nrow(x) == 0, logical(1))
]

# Excel: 31-char sheet names, no : \ / ? * [ ]
names(sheets) <- make.unique(
  substr(gsub("[:\\\\/?*\\[\\]]", "_", names(sheets)), 1, 31), sep = "_"
)

# authors_long can exceed Excel's 1,048,576-row limit; keep it CSV-only
if (nrow(authors_long) < 1e6) {
  sheets[["authors_long"]] <- authors_long
} else {
  message("\nauthors_long has ", format(nrow(authors_long), big.mark = ","),
          " rows -- exceeds Excel limits, CSV only.")
}

if (requireNamespace("openxlsx", quietly = TRUE)) {
  
  wb <- openxlsx::createWorkbook()
  hdr <- openxlsx::createStyle(
    fontColour = "#FFFFFF", fgFill = "#0C234B",   # UA blue
    textDecoration = "bold", halign = "left",
    valign = "center", border = "Bottom", wrapText = TRUE
  )
  
  for (nm in names(sheets)) {
    dat <- sheets[[nm]]
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, dat, withFilter = TRUE)
    openxlsx::addStyle(wb, nm, hdr, rows = 1,
                       cols = seq_len(ncol(dat)), gridExpand = TRUE)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    # "auto" is slow on big sheets
    # ...continuing inside the `for (nm in names(sheets))` loop
    # "auto" width calculation is slow on large sheets
    if (nrow(dat) <= 2000) {
      openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(dat)),
                             widths = "auto", ignoreMergedCells = TRUE)
    } else {
      openxlsx::setColWidths(wb, nm, cols = seq_len(ncol(dat)), widths = 20)
    }
  }
  
  # Highlight the two headline OA-rate cells on the summary sheets
  pct_style <- openxlsx::createStyle(numFmt = "0.0", halign = "right")
  for (nm in intersect(names(sheets),
                       c("oa_by_type", "oa_by_field", "oa_by_field_articles",
                         "oa_by_domain", "oa_by_collaboration",
                         "oa_by_institutions"))) {
    dat <- sheets[[nm]]
    rate_cols <- which(names(dat) %in% c("oa_rate", "article_oa_rate"))
    if (length(rate_cols)) {
      openxlsx::addStyle(wb, nm, pct_style,
                         rows = 2:(nrow(dat) + 1), cols = rate_cols,
                         gridExpand = TRUE, stack = TRUE)
    }
  }
  
  openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)
  message("\nWrote workbook via openxlsx: ", xlsx_file,
          "  (", length(sheets), " sheets)")
  
} else if (requireNamespace("writexl", quietly = TRUE)) {
  
  message("\n'openxlsx' not installed; using 'writexl' (no styling).")
  writexl::write_xlsx(sheets, path = xlsx_file, format_headers = TRUE)
  message("Wrote workbook via writexl: ", xlsx_file,
          "  (", length(sheets), " sheets)")
  
} else {
  
  warning(
    "Neither 'openxlsx' nor 'writexl' is installed, so no XLSX was written.\n",
    "  install.packages(\"openxlsx\")   # styling, frozen headers, filters\n",
    "  install.packages(\"writexl\")    # lighter, no styling\n",
    "The CSVs from Step 9c already contain everything."
  )
  xlsx_file <- NA_character_
  
}

# ============================================================
# STEP 10 -- Session info, run log, final console summary
# ============================================================

# --- 10a: Session info ---
session_txt <- paste0(prefix, "_sessionInfo.txt")

session_lines <- c(
  paste0("UA ALL WORKS ", YR, " -- session information"),
  paste0("Written: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", Sys.timezone()),
  "Data source: OpenAlex REST API, direct JSON (openalexR not used)",
  strrep("-", 62),
  ""
)

if (requireNamespace("sessioninfo", quietly = TRUE)) {
  session_lines <- c(session_lines, capture.output(sessioninfo::session_info()))
} else {
  session_lines <- c(
    session_lines,
    capture.output(utils::sessionInfo()),
    "",
    "NOTE: install.packages('sessioninfo') for richer provenance."
  )
}

writeLines(session_lines, session_txt)
message("Wrote session info: ", session_txt)

# --- 10b: Human-readable run log ---
log_txt <- paste0(prefix, "_runlog.txt")

log_lines <- c(
  paste0("UA ALL WORKS ", YR, " -- RUN LOG"),
  strrep("=", 62),
  "",
  "PROVENANCE",
  paste0("  Date pulled        : ", format(as.Date(date_pulled), "%Y-%m-%d")),
  paste0("  Time pulled        : ", format(date_pulled, "%H:%M:%S"), " ", Sys.timezone()),
  paste0("  Data source        : OpenAlex REST API, direct JSON"),
  paste0("  Institution        : University of Arizona (", UA_ID, ")"),
  paste0("  Publication window : ", YR, "-01-01 to ", YR, "-12-31"),
  paste0("  OA filter          : NONE (all works, OA and closed)"),
  paste0("  Pagination         : cursor paging, per-page=", PER_PAGE),
  "",
  "COUNTS",
  paste0("  API count_only     : ", format(expected_n, big.mark = ",")),
  paste0("  Records fetched    : ", format(length(works_raw), big.mark = ",")),
  paste0("  Rows in table      : ", format(nrow(ua), big.mark = ",")),
  paste0("  Unique OpenAlex ids: ", format(dplyr::n_distinct(ua$id), big.mark = ",")),
  paste0("  Author-level rows  : ", format(nrow(authors_long), big.mark = ",")),
  "",
  "HEADLINE RESULTS",
  paste0("  Overall OA rate    : ", overall_oa_rate, "%  (all work types, n=",
         format(nrow(ua), big.mark = ","), ")"),
  paste0("  Total articles     : ", format(n_articles, big.mark = ",")),
  paste0("  ARTICLE OA RATE    : ", article_oa_rate, "%"),
  "",
  "TARGET FIELDS -- all three direct from OpenAlex, none calculated",
  paste0("  countries_distinct_count    : populated on ",
         sum(!is.na(ua$countries_distinct_count)), " / ", nrow(ua), " works"),
  paste0("  institutions_distinct_count : populated on ",
         sum(!is.na(ua$institutions_distinct_count)), " / ", nrow(ua), " works"),
  paste0("  best_oa_location            : non-null on ",
         sum(ua$best_oa_exists), " / ", nrow(ua),
         " works (null = closed, by design)"),
  "",
  "FILES WRITTEN",
  paste0("  ", prefix, "_raw_json.rds       (raw API response, re-derivable)"),
  paste0("  ", prefix, "_works.rds          (flattened works table)"),
  paste0("  ", prefix, "_authors_long.rds   (author-level table)"),
  if (!is.na(xlsx_file)) paste0("  ", xlsx_file, "   (multi-sheet workbook)")
  else "  (no XLSX -- openxlsx/writexl unavailable)",
  paste0("  ", prefix, "_*.csv             (", length(csv_targets), " files)"),
  paste0("  ", session_txt),
  "",
  "CAVEATS",
  "  1. OpenAlex caps the authorships array at the first 100 authors.",
  "     Affects: authors_listed_in_payload, ua_author_count, ua_is_first_author.",
  "     Does NOT affect: countries_distinct_count, institutions_distinct_count",
  "     (computed upstream by OpenAlex). Works at the cap are flagged in",
  "     the authorships_maybe_capped column: ",
  paste0("     ", sum(ua$authorships_maybe_capped, na.rm = TRUE), " work(s)."),
  "",
  paste0("  2. ", YR, " is a recent year. Indexing continues, and green OA",
         " accrues"),
  "     as accepted manuscripts land in repositories and embargoes lapse.",
  "     The OA rates above are a FLOOR. Re-running later will yield higher",
  "     figures -- which is why date_pulled is recorded on every output.",
  "",
  "  3. best_oa_location is null for closed works by design. A missing value",
  "     there is informative, not an error.",
  "",
  "  4. apc_paid is populated for only a subset of works. APC totals are a",
  "     lower bound, not a complete accounting.",
  "",
  "  5. topic_field / topic_domain come from primary_topic, i.e. the single",
  "     highest-scoring topic. Multi-disciplinary works are assigned to one",
  "     field only. Use the all_fields column for a fuller picture.",
  "",
  "REPRODUCING WITHOUT RE-QUERYING THE API",
  paste0("  works_raw <- readRDS(\"", prefix, "_raw_json.rds\")"),
  "  # then re-run Step 4 onward",
  ""
)

writeLines(log_lines, log_txt)
message("Wrote run log: ", log_txt)

# --- 10c: Final console summary ---
elapsed <- round(as.numeric(difftime(Sys.time(), date_pulled, units = "mins")), 1)

message("\n", strrep("=", 62))
message("DONE -- UA ALL WORKS ", YR)
message(strrep("=", 62))
message("Source            : OpenAlex REST API, direct JSON")
message("Works fetched     : ", format(nrow(ua), big.mark = ","),
        "  (API said ", format(expected_n, big.mark = ","), ")")
message("Overall OA rate   : ", overall_oa_rate, "%")
message("Article OA rate   : ", article_oa_rate, "%  (n = ",
        format(n_articles, big.mark = ","), ")")
message("")
message("Target fields, all direct from OpenAlex:")
message("  countries_distinct_count    : ",
        sum(!is.na(ua$countries_distinct_count)), "/", nrow(ua))
message("  institutions_distinct_count : ",
        sum(!is.na(ua$institutions_distinct_count)), "/", nrow(ua))
message("  best_oa_location            : ", sum(ua$best_oa_exists), "/", nrow(ua),
        " non-null")
message("")
message("Workbook          : ", if (!is.na(xlsx_file)) xlsx_file else "not written")
message("Run log           : ", log_txt)
message("Elapsed           : ", elapsed, " min")
message("")
message("Reload without re-querying:")
message("  ua <- readRDS(\"", prefix, "_works.rds\")")
message("  works_raw <- readRDS(\"", prefix, "_raw_json.rds\")")

####

works_raw <- readRDS(paste0(prefix, "_raw_json.rds"))

# Identify the 122 works missing I138006243 in institution_ids
missing_ua <- ua |>
  dplyr::filter(!grepl("I138006243", institution_ids, fixed = TRUE) |
                  is.na(institution_ids)) |>
  dplyr::pull(id)

message("Works missing I138006243 in institution_ids: ", length(missing_ua))

# Diagnose each one from the raw JSON
diag <- purrr::map_dfr(works_raw, function(w) {
  wid <- short_id(w$id)
  if (!wid %in% missing_ua) return(NULL)
  
  au <- w$authorships %||% list()
  
  # All institution IDs in the served array
  all_inst_ids <- unique(unlist(lapply(au, function(a)
    vapply(a$institutions %||% list(),
           function(i) short_id(i$id), character(1))),
    use.names = FALSE))
  
  # All lineage IDs in the served array
  all_lineage_ids <- unique(unlist(lapply(au, function(a)
    unlist(lapply(a$institutions %||% list(),
                  function(i) vapply(i$lineage %||% list(),
                                     short_id, character(1))))),
    use.names = FALSE))
  
  # Check raw affiliation strings for UA
  raw_strings <- unlist(lapply(au, function(a)
    unlist(a$raw_affiliation_strings %||% list())), use.names = FALSE)
  ua_in_raw <- any(grepl("University of Arizona|UArizona|Tucson.*AZ|AZ ?857",
                         raw_strings, ignore.case = TRUE))
  
  # Check affiliations mapping
  ua_in_affiliations <- any(unlist(lapply(au, function(a)
    vapply(a$affiliations %||% list(), function(f)
      "I138006243" %in% vapply(f$institution_ids %||% list(),
                               short_id, character(1)),
      logical(1))), use.names = FALSE))
  
  tibble::tibble(
    id                    = wid,
    n_authorships         = length(au),
    at_cap                = length(au) >= 100,
    ua_in_inst_ids        = "I138006243" %in% all_inst_ids,
    ua_in_lineage         = "I138006243" %in% all_lineage_ids,
    ua_in_raw_text        = ua_in_raw,
    ua_in_affiliations    = ua_in_affiliations,
    n_institutions        = length(all_inst_ids),
    sample_inst_ids       = paste(head(all_inst_ids, 5), collapse = "; ")
  )
})

message("\n--- Cause breakdown ---")
print(diag |> dplyr::count(ua_in_inst_ids, ua_in_lineage,
                           ua_in_raw_text, at_cap, sort = TRUE))

######### Deeper check: fetch the full record 
cap_ids <- diag |> dplyr::filter(at_cap, !ua_in_lineage) |> dplyr::pull(id)

for (wid in head(cap_ids, 3)) {
  full <- jsonlite::fromJSON(
    paste0("https://api.openalex.org/works/", wid,
           "?select=id,authorships&mailto=", MAILTO),
    simplifyVector = FALSE
  )
  cat("\n==== ", wid, " | served:", length(diag$n_authorships[diag$id == wid]),
      " | full:", length(full$authorships), " ====\n")
  
  # Find UA in the full list
  for (i in seq_along(full$authorships)) {
    a <- full$authorships[[i]]
    for (inst in a$institutions %||% list()) {
      if (identical(short_id(inst$id), "I138006243")) {
        cat("  FOUND at position ", i, ": ",
            a$author$display_name %||% "?", "\n")
      }
    }
  }
}


child_cases <- diag |> dplyr::filter(!ua_in_inst_ids, ua_in_lineage) |>
  dplyr::pull(id)

for (wid in head(child_cases, 5)) {
  w <- Filter(function(x) short_id(x$id) == wid, works_raw)[[1]]
  for (a in w$authorships %||% list()) {
    for (inst in a$institutions %||% list()) {
      lin <- vapply(inst$lineage %||% list(), short_id, character(1))
      if ("I138006243" %in% lin && !identical(short_id(inst$id), "I138006243")) {
        cat(wid, "| child:", short_id(inst$id), "|",
            inst$display_name %||% "NA", "\n")
      }
    }
  }
}


# ============================================================
# SELF-CONTAINED RAW_TYPE CHECK
# Required existing object: works_raw
# No packages required
# ============================================================

if (!exists("works_raw", inherits = TRUE)) {
  stop("Object `works_raw` was not found.")
}

if (!is.list(works_raw) || length(works_raw) == 0L) {
  stop("`works_raw` must be a nonempty list of OpenAlex works.")
}

# Return the first nonblank character value.
first_text <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  x <- as.character(unlist(
    x,
    recursive = TRUE,
    use.names = FALSE
  ))
  
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  
  if (length(x) == 0L) {
    return(NA_character_)
  }
  
  x[[1L]]
}

# Safely obtain a named list element.
list_value <- function(x, field) {
  if (
    is.null(x) ||
    !is.list(x) ||
    is.null(names(x)) ||
    !(field %in% names(x))
  ) {
    return(NULL)
  }
  
  x[[field]]
}

get_work_id <- function(work) {
  first_text(list_value(work, "id"))
}

get_title <- function(work) {
  value <- first_text(list_value(work, "title"))
  
  if (is.na(value)) {
    value <- first_text(list_value(work, "display_name"))
  }
  
  value
}

get_type <- function(work) {
  first_text(list_value(work, "type"))
}

get_root_raw_type <- function(work) {
  first_text(list_value(work, "raw_type"))
}

get_type_crossref <- function(work) {
  first_text(list_value(work, "type_crossref"))
}

get_primary_raw_type <- function(work) {
  primary_location <- list_value(work, "primary_location")
  
  if (is.null(primary_location) || !is.list(primary_location)) {
    return(NA_character_)
  }
  
  first_text(list_value(primary_location, "raw_type"))
}

get_location_raw_types <- function(work) {
  locations <- list_value(work, "locations")
  
  if (is.null(locations) || length(locations) == 0L) {
    return(NA_character_)
  }
  
  values <- vapply(
    locations,
    function(location) {
      if (is.null(location) || !is.list(location)) {
        return(NA_character_)
      }
      
      first_text(list_value(location, "raw_type"))
    },
    character(1)
  )
  
  values <- unique(values[!is.na(values) & nzchar(values)])
  
  if (length(values) == 0L) {
    return(NA_character_)
  }
  
  paste(values, collapse = "; ")
}

# Use one raw_type for the work export.
#
# Preference:
# 1. Work-level raw_type
# 2. type_crossref
# 3. Primary-location raw_type
#
# All-location values remain diagnostic only because one work can
# have locations from multiple sources with different raw types.
get_export_raw_type <- function(work) {
  candidates <- c(
    get_root_raw_type(work),
    get_type_crossref(work),
    get_primary_raw_type(work)
  )
  
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  
  if (length(candidates) == 0L) {
    return(NA_character_)
  }
  
  candidates[[1L]]
}

# Build diagnostic data.
raw_type_check <- data.frame(
  row_number = seq_along(works_raw),
  
  openalex_id = vapply(
    works_raw,
    get_work_id,
    character(1)
  ),
  
  title = vapply(
    works_raw,
    get_title,
    character(1)
  ),
  
  type = vapply(
    works_raw,
    get_type,
    character(1)
  ),
  
  root_raw_type = vapply(
    works_raw,
    get_root_raw_type,
    character(1)
  ),
  
  type_crossref = vapply(
    works_raw,
    get_type_crossref,
    character(1)
  ),
  
  primary_location_raw_type = vapply(
    works_raw,
    get_primary_raw_type,
    character(1)
  ),
  
  all_location_raw_types = vapply(
    works_raw,
    get_location_raw_types,
    character(1)
  ),
  
  export_raw_type = vapply(
    works_raw,
    get_export_raw_type,
    character(1)
  ),
  
  stringsAsFactors = FALSE
)

# Summarize where raw_type was found.
raw_type_summary <- data.frame(
  measure = c(
    "Total works",
    "Root raw_type populated",
    "type_crossref populated",
    "Primary-location raw_type populated",
    "Any location raw_type populated",
    "Export raw_type populated",
    "Export raw_type missing"
  ),
  
  count = c(
    nrow(raw_type_check),
    sum(!is.na(raw_type_check$root_raw_type)),
    sum(!is.na(raw_type_check$type_crossref)),
    sum(!is.na(raw_type_check$primary_location_raw_type)),
    sum(!is.na(raw_type_check$all_location_raw_types)),
    sum(!is.na(raw_type_check$export_raw_type)),
    sum(is.na(raw_type_check$export_raw_type))
  ),
  
  stringsAsFactors = FALSE
)

raw_type_summary$percent <- round(
  100 * raw_type_summary$count / nrow(raw_type_check),
  2
)

cat("\n================ RAW_TYPE SUMMARY ================\n")
print(raw_type_summary, row.names = FALSE)

# Display sample values.
raw_type_found <- raw_type_check[
  !is.na(raw_type_check$export_raw_type),
  c(
    "openalex_id",
    "type",
    "export_raw_type",
    "root_raw_type",
    "type_crossref",
    "primary_location_raw_type",
    "all_location_raw_types",
    "title"
  ),
  drop = FALSE
]

cat("\nFirst 20 works with raw_type:\n")

if (nrow(raw_type_found) == 0L) {
  cat("No raw_type values were found.\n")
} else {
  print(utils::head(raw_type_found, 20L), row.names = FALSE)
}

# Frequency table.
raw_type_frequency <- as.data.frame(
  sort(
    table(
      raw_type_check$export_raw_type,
      useNA = "no"
    ),
    decreasing = TRUE
  ),
  stringsAsFactors = FALSE
)

names(raw_type_frequency) <- c("raw_type", "count")

cat("\nMost frequent raw_type values:\n")

if (nrow(raw_type_frequency) == 0L) {
  cat("No raw_type values were available to count.\n")
} else {
  print(
    utils::head(raw_type_frequency, 30L),
    row.names = FALSE
  )
}

# Save diagnostics.
utils::write.csv(
  raw_type_check,
  "raw_type_check.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  raw_type_summary,
  "raw_type_summary.csv",
  row.names = FALSE,
  na = ""
)

message("Saved raw_type_check.csv and raw_type_summary.csv")

# ============================================================
# CHECK WHY any_repository_has_fulltext IS BLANK
# Required existing object: works_raw
# No packages required
# ============================================================

if (!exists("works_raw", inherits = TRUE)) {
  stop("Object `works_raw` was not found.")
}

if (!is.list(works_raw) || length(works_raw) == 0L) {
  stop("`works_raw` must be a nonempty list.")
}

check_repository_value <- function(work) {
  
  # Is open_access present?
  if (
    is.null(work) ||
    !is.list(work) ||
    is.null(work$open_access) ||
    !is.list(work$open_access)
  ) {
    return(list(
      oa_present = FALSE,
      key_present = FALSE,
      raw_value = NA_character_,
      value = NA
    ))
  }
  
  oa <- work$open_access
  
  # Is the specific key present?
  key_present <-
    !is.null(names(oa)) &&
    "any_repository_has_fulltext" %in% names(oa)
  
  if (!key_present) {
    return(list(
      oa_present = TRUE,
      key_present = FALSE,
      raw_value = NA_character_,
      value = NA
    ))
  }
  
  raw <- oa[["any_repository_has_fulltext"]]
  
  # Is the key present but null?
  if (is.null(raw) || length(raw) == 0L) {
    return(list(
      oa_present = TRUE,
      key_present = TRUE,
      raw_value = NA_character_,
      value = NA
    ))
  }
  
  raw_first <- raw[[1L]]
  raw_text <- as.character(raw_first)
  
  # Convert only recognized Boolean representations.
  if (is.logical(raw_first)) {
    logical_value <- raw_first
  } else if (
    tolower(trimws(raw_text)) %in% c("true", "1")
  ) {
    logical_value <- TRUE
  } else if (
    tolower(trimws(raw_text)) %in% c("false", "0")
  ) {
    logical_value <- FALSE
  } else {
    logical_value <- NA
  }
  
  list(
    oa_present = TRUE,
    key_present = TRUE,
    raw_value = raw_text,
    value = logical_value
  )
}

checked <- lapply(works_raw, check_repository_value)

repository_check <- data.frame(
  row_number = seq_along(works_raw),
  
  openalex_id = vapply(
    works_raw,
    function(w) {
      if (is.null(w$id) || length(w$id) == 0L) {
        NA_character_
      } else {
        as.character(w$id[[1L]])
      }
    },
    character(1)
  ),
  
  open_access_present = vapply(
    checked,
    function(x) x$oa_present,
    logical(1)
  ),
  
  key_present = vapply(
    checked,
    function(x) x$key_present,
    logical(1)
  ),
  
  raw_value = vapply(
    checked,
    function(x) x$raw_value,
    character(1)
  ),
  
  any_repository_has_fulltext = vapply(
    checked,
    function(x) x$value,
    logical(1)
  ),
  
  stringsAsFactors = FALSE
)

repository_summary <- data.frame(
  measure = c(
    "Total works",
    "open_access object present",
    "Field key present",
    "TRUE values",
    "FALSE values",
    "Missing values",
    "Key absent",
    "Key present but value missing/unrecognized"
  ),
  
  count = c(
    nrow(repository_check),
    
    sum(
      repository_check$open_access_present,
      na.rm = TRUE
    ),
    
    sum(
      repository_check$key_present,
      na.rm = TRUE
    ),
    
    sum(
      repository_check$any_repository_has_fulltext %in% TRUE
    ),
    
    sum(
      repository_check$any_repository_has_fulltext %in% FALSE
    ),
    
    sum(
      is.na(repository_check$any_repository_has_fulltext)
    ),
    
    sum(
      !repository_check$key_present
    ),
    
    sum(
      repository_check$key_present &
        is.na(repository_check$any_repository_has_fulltext)
    )
  ),
  
  stringsAsFactors = FALSE
)

repository_summary$percent <- round(
  100 * repository_summary$count / nrow(repository_check),
  2
)

cat(
  "\n===== ANY_REPOSITORY_HAS_FULLTEXT SUMMARY =====\n"
)

print(repository_summary, row.names = FALSE)

cat("\nRaw-value frequency:\n")

print(
  table(
    repository_check$raw_value,
    useNA = "always"
  )
)

cat("\nFirst 20 records producing blanks:\n")

print(
  utils::head(
    repository_check[
      is.na(repository_check$any_repository_has_fulltext),
      ,
      drop = FALSE
    ],
    20L
  ),
  row.names = FALSE
)


# Rebuild flattened data from the current raw records
ua <- purrr::map_dfr(works_raw, flatten_work)

# Verify immediately
table(
  ua$any_repository_has_fulltext,
  useNA = "always"
)



# ============================================================
# BEST_OA_LICENSE CHECK
# Required existing object: ua
# No helper functions required
# ============================================================

if (!exists("ua", inherits = TRUE)) {
  stop("Object `ua` was not found.")
}

if (!is.data.frame(ua)) {
  stop("`ua` must be a data frame or tibble.")
}

required <- c(
  "id",
  "title",
  "oa_status",
  "best_oa_exists",
  "best_oa_license",
  "best_oa_source_type",
  "best_oa_version",
  "best_oa_landing_page_url"
)

missing_columns <- setdiff(required, names(ua))

if (length(missing_columns) > 0L) {
  stop(
    "`ua` is missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Normalize blank licenses to NA for analysis.
license_check <- ua

license_check$license_normalized <- tolower(
  trimws(as.character(license_check$best_oa_license))
)

license_check$license_normalized[
  is.na(license_check$license_normalized) |
    license_check$license_normalized == ""
] <- NA_character_

# Exact CC BY family, such as cc-by or cc-by-4.0.
# This deliberately excludes cc-by-nc, cc-by-nd, etc.
license_check$is_cc_by <- !is.na(
  license_check$license_normalized
) & grepl(
  "^cc-by(?:-[0-9.]+)?$",
  license_check$license_normalized,
  perl = TRUE
)

# ------------------------------------------------------------
# 1. License counts by OA status
# ------------------------------------------------------------

license_by_status <- as.data.frame(
  with(
    license_check,
    table(
      oa_status,
      license_normalized,
      useNA = "ifany"
    )
  ),
  stringsAsFactors = FALSE
)

names(license_by_status) <- c(
  "oa_status",
  "best_oa_license",
  "count"
)

license_by_status <- license_by_status[
  license_by_status$count > 0L,
  ,
  drop = FALSE
]

license_by_status <- license_by_status[
  order(
    license_by_status$oa_status,
    -license_by_status$count
  ),
  ,
  drop = FALSE
]

cat("\n===== BEST OA LICENSE BY OA STATUS =====\n")
print(license_by_status, row.names = FALSE)

# ------------------------------------------------------------
# 2. Focused bronze and green summary
# ------------------------------------------------------------

focused_summary <- do.call(
  rbind,
  lapply(
    c("bronze", "green"),
    function(status_value) {
      x <- license_check[
        license_check$oa_status == status_value &
          !is.na(license_check$oa_status),
        ,
        drop = FALSE
      ]
      
      data.frame(
        oa_status = status_value,
        total_works = nrow(x),
        license_present = sum(
          !is.na(x$license_normalized)
        ),
        license_missing = sum(
          is.na(x$license_normalized)
        ),
        exact_cc_by = sum(
          x$is_cc_by,
          na.rm = TRUE
        ),
        repository_best_location = sum(
          x$best_oa_source_type == "repository",
          na.rm = TRUE
        ),
        non_repository_best_location = sum(
          !is.na(x$best_oa_source_type) &
            x$best_oa_source_type != "repository"
        ),
        stringsAsFactors = FALSE
      )
    }
  )
)

focused_summary$pct_with_license <- round(
  100 *
    focused_summary$license_present /
    pmax(focused_summary$total_works, 1L),
  2
)

focused_summary$pct_exact_cc_by <- round(
  100 *
    focused_summary$exact_cc_by /
    pmax(focused_summary$total_works, 1L),
  2
)

cat("\n===== BRONZE AND GREEN LICENSE SUMMARY =====\n")
print(focused_summary, row.names = FALSE)

# ------------------------------------------------------------
# 3. Green works reported as exact CC BY
# ------------------------------------------------------------

green_cc_by <- license_check[
  license_check$oa_status == "green" &
    license_check$is_cc_by,
  c(
    "id",
    "title",
    "best_oa_license",
    "best_oa_source_type",
    "best_oa_version",
    "best_oa_landing_page_url"
  ),
  drop = FALSE
]

cat("\n===== GREEN WORKS WITH EXACT CC BY =====\n")
cat("Count:", nrow(green_cc_by), "\n")

if (nrow(green_cc_by) > 0L) {
  print(
    utils::head(green_cc_by, 30L),
    row.names = FALSE
  )
}

# ------------------------------------------------------------
# 4. Potential consistency issues
# ------------------------------------------------------------

# Bronze normally has no identified open license.
bronze_with_license <- license_check[
  license_check$oa_status == "bronze" &
    !is.na(license_check$license_normalized),
  c(
    "id",
    "title",
    "best_oa_license",
    "best_oa_source_type",
    "best_oa_version",
    "best_oa_landing_page_url"
  ),
  drop = FALSE
]

# Green OA should normally have a repository OA location.
green_non_repository <- license_check[
  license_check$oa_status == "green" &
    (
      is.na(license_check$best_oa_source_type) |
        license_check$best_oa_source_type != "repository"
    ),
  c(
    "id",
    "title",
    "best_oa_license",
    "best_oa_source_type",
    "best_oa_version",
    "best_oa_landing_page_url"
  ),
  drop = FALSE
]

cat("\nBronze works with a recorded license:",
    nrow(bronze_with_license), "\n")

cat("Green works whose best OA source is not identified as a repository:",
    nrow(green_non_repository), "\n")

# ------------------------------------------------------------
# 5. Save review files
# ------------------------------------------------------------

utils::write.csv(
  license_by_status,
  "best_oa_license_by_status.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  focused_summary,
  "best_oa_license_bronze_green_summary.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  green_cc_by,
  "green_oa_cc_by_review.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  bronze_with_license,
  "bronze_with_license_review.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  green_non_repository,
  "green_non_repository_review.csv",
  row.names = FALSE,
  na = ""
)

message("Best OA license check completed.")


# ============================================================
# UA INSTITUTION-ID DISCREPANCY CHECK
#
# Required existing object:
#   works_raw
#
# No packages or external helper functions required.
# ============================================================

if (!exists("works_raw", inherits = TRUE)) {
  stop("Object `works_raw` was not found.")
}

if (!is.list(works_raw) || length(works_raw) == 0L) {
  stop("`works_raw` must be a nonempty list of OpenAlex works.")
}

UA_ID <- "I138006243"

short_openalex_id <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  x <- as.character(x[[1L]])
  
  if (is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }
  
  sub(
    "^https?://openalex\\.org/",
    "",
    x,
    ignore.case = TRUE
  )
}

first_text <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  values <- as.character(
    unlist(x, recursive = TRUE, use.names = FALSE)
  )
  
  values <- trimws(values)
  values <- values[!is.na(values) & nzchar(values)]
  
  if (length(values) == 0L) {
    return(NA_character_)
  }
  
  values[[1L]]
}

get_authorships <- function(work) {
  if (
    is.null(work) ||
    !is.list(work) ||
    is.null(work$authorships)
  ) {
    return(list())
  }
  
  work$authorships
}

get_institution_ids <- function(work) {
  authorships <- get_authorships(work)
  
  values <- unlist(
    lapply(
      authorships,
      function(authorship) {
        institutions <- authorship$institutions
        
        if (is.null(institutions) || length(institutions) == 0L) {
          return(character(0))
        }
        
        vapply(
          institutions,
          function(institution) {
            short_openalex_id(institution$id)
          },
          character(1)
        )
      }
    ),
    use.names = FALSE
  )
  
  unique(values[!is.na(values) & nzchar(values)])
}

get_lineage_ids <- function(work) {
  authorships <- get_authorships(work)
  
  values <- unlist(
    lapply(
      authorships,
      function(authorship) {
        institutions <- authorship$institutions
        
        if (is.null(institutions) || length(institutions) == 0L) {
          return(character(0))
        }
        
        unlist(
          lapply(
            institutions,
            function(institution) {
              lineage <- institution$lineage
              
              if (is.null(lineage) || length(lineage) == 0L) {
                return(character(0))
              }
              
              vapply(
                lineage,
                short_openalex_id,
                character(1)
              )
            }
          ),
          use.names = FALSE
        )
      }
    ),
    use.names = FALSE
  )
  
  unique(values[!is.na(values) & nzchar(values)])
}

get_raw_affiliations <- function(work) {
  authorships <- get_authorships(work)
  
  values <- unlist(
    lapply(
      authorships,
      function(authorship) {
        authorship$raw_affiliation_strings
      }
    ),
    recursive = TRUE,
    use.names = FALSE
  )
  
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(trimws(values))]
  
  unique(values)
}

get_corresponding_institution_ids <- function(work) {
  values <- work$corresponding_institution_ids
  
  if (is.null(values) || length(values) == 0L) {
    return(character(0))
  }
  
  values <- vapply(
    values,
    short_openalex_id,
    character(1)
  )
  
  unique(values[!is.na(values) & nzchar(values)])
}

ua_raw_pattern <- paste(
  c(
    "University of Arizona",
    "UArizona",
    "Univ\\.? of Arizona",
    "Arizona, Tucson",
    "Tucson, Arizona",
    "Tucson, AZ",
    "AZ[[:space:]]*857"
  ),
  collapse = "|"
)

diagnose_ua_work <- function(work) {
  authorships <- get_authorships(work)
  institution_ids <- get_institution_ids(work)
  lineage_ids <- get_lineage_ids(work)
  raw_affiliations <- get_raw_affiliations(work)
  
  ua_in_raw_text <- length(raw_affiliations) > 0L &&
    any(
      grepl(
        ua_raw_pattern,
        raw_affiliations,
        ignore.case = TRUE,
        perl = TRUE
      )
    )
  
  exact_ua <- UA_ID %in% institution_ids
  lineage_ua <- UA_ID %in% lineage_ids
  corresponding_ua <- UA_ID %in%
    get_corresponding_institution_ids(work)
  
  n_authorships <- length(authorships)
  
  reason <- if (exact_ua) {
    "Exact UA institution ID present"
  } else if (n_authorships >= 100L) {
    "UA absent; authorships payload is at the 100-author cap"
  } else if (lineage_ua) {
    "UA absent as exact ID; present only in institution lineage"
  } else if (ua_in_raw_text) {
    "UA appears in raw affiliation text but was not linked to UA ID"
  } else if (corresponding_ua) {
    "UA appears only in corresponding_institution_ids"
  } else {
    "No UA signal in returned payload"
  }
  
  data.frame(
    openalex_id = short_openalex_id(work$id),
    title = first_text(work$title),
    n_authorships = n_authorships,
    at_100_author_cap = n_authorships >= 100L,
    n_returned_institutions = length(institution_ids),
    exact_ua_id_present = exact_ua,
    ua_in_lineage = lineage_ua,
    ua_in_raw_affiliation = ua_in_raw_text,
    ua_in_corresponding_institutions = corresponding_ua,
    reason = reason,
    stringsAsFactors = FALSE
  )
}

ua_institution_check <- do.call(
  rbind,
  lapply(works_raw, diagnose_ua_work)
)

# ------------------------------------------------------------
# Overall summary
# ------------------------------------------------------------

ua_institution_summary <- data.frame(
  measure = c(
    "Total works",
    "Exact UA ID present in returned authorships",
    "Exact UA ID absent from returned authorships",
    "Absent and authorships at 100-author cap",
    "Absent but UA appears in institution lineage",
    "Absent but UA appears in raw affiliation text",
    "Absent but UA appears in corresponding institutions",
    "Absent with no returned UA signal"
  ),
  
  count = c(
    nrow(ua_institution_check),
    
    sum(
      ua_institution_check$exact_ua_id_present
    ),
    
    sum(
      !ua_institution_check$exact_ua_id_present
    ),
    
    sum(
      !ua_institution_check$exact_ua_id_present &
        ua_institution_check$at_100_author_cap
    ),
    
    sum(
      !ua_institution_check$exact_ua_id_present &
        ua_institution_check$ua_in_lineage
    ),
    
    sum(
      !ua_institution_check$exact_ua_id_present &
        ua_institution_check$ua_in_raw_affiliation
    ),
    
    sum(
      !ua_institution_check$exact_ua_id_present &
        ua_institution_check$
        ua_in_corresponding_institutions
    ),
    
    sum(
      !ua_institution_check$exact_ua_id_present &
        !ua_institution_check$at_100_author_cap &
        !ua_institution_check$ua_in_lineage &
        !ua_institution_check$ua_in_raw_affiliation &
        !ua_institution_check$
        ua_in_corresponding_institutions
    )
  ),
  
  stringsAsFactors = FALSE
)

ua_institution_summary$percent <- round(
  100 *
    ua_institution_summary$count /
    nrow(ua_institution_check),
  2
)

cat("\n================ UA INSTITUTION-ID SUMMARY ================\n")
print(ua_institution_summary, row.names = FALSE)

cat("\n================ REASON BREAKDOWN ================\n")
print(
  sort(
    table(ua_institution_check$reason),
    decreasing = TRUE
  )
)

# ------------------------------------------------------------
# Records needing explanation
# ------------------------------------------------------------

ua_id_missing <- ua_institution_check[
  !ua_institution_check$exact_ua_id_present,
  ,
  drop = FALSE
]

cat("\nFirst 30 works without exact I138006243:\n")

print(
  utils::head(ua_id_missing, 30L),
  row.names = FALSE
)

utils::write.csv(
  ua_institution_check,
  "ua_institution_id_check.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  ua_institution_summary,
  "ua_institution_id_summary.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  ua_id_missing,
  "ua_institution_id_missing_review.csv",
  row.names = FALSE,
  na = ""
)

message("Saved UA institution-ID diagnostic files.")

# ============================================================
# SELF-CONTAINED UA ROR DISCREPANCY CHECK
#
# Required existing object:
#   works_raw
#
# No packages or external helper functions required
# ============================================================

if (!exists("works_raw", inherits = TRUE)) {
  stop("Object `works_raw` was not found.")
}

if (!is.list(works_raw) || length(works_raw) == 0L) {
  stop("`works_raw` must be a nonempty list of OpenAlex works.")
}

UA_ID  <- "I138006243"
UA_ROR <- "03m2x1q45"

# Normalize OpenAlex IDs to I-prefixed form.
normalize_openalex_id <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  x <- as.character(x[[1L]])
  
  if (is.na(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }
  
  sub(
    "^https?://openalex\\.org/",
    "",
    trimws(x),
    ignore.case = TRUE
  )
}

# Normalize ROR URLs to the short ROR identifier.
normalize_ror <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  x <- as.character(x[[1L]])
  
  if (is.na(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }
  
  x <- sub(
    "^https?://(?:www\\.)?ror\\.org/",
    "",
    trimws(x),
    ignore.case = TRUE,
    perl = TRUE
  )
  
  sub("/$", "", x)
}

first_text <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  
  x <- as.character(unlist(
    x,
    recursive = TRUE,
    use.names = FALSE
  ))
  
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  
  if (length(x) == 0L) {
    return(NA_character_)
  }
  
  x[[1L]]
}

get_authorships <- function(work) {
  if (
    is.null(work) ||
    !is.list(work) ||
    is.null(work$authorships)
  ) {
    return(list())
  }
  
  work$authorships
}

# Collect the institution objects served in authorships.
get_institutions <- function(work) {
  authorships <- get_authorships(work)
  
  institutions <- unlist(
    lapply(
      authorships,
      function(authorship) {
        authorship$institutions %||% list()
      }
    ),
    recursive = FALSE
  )
  
  institutions
}

# Local null helper used only by this module.
`%||%` <- function(x, replacement) {
  if (is.null(x) || length(x) == 0L) {
    replacement
  } else {
    x
  }
}

get_institution_ids <- function(work) {
  institutions <- get_institutions(work)
  
  if (length(institutions) == 0L) {
    return(character(0))
  }
  
  values <- vapply(
    institutions,
    function(institution) {
      normalize_openalex_id(institution$id)
    },
    character(1)
  )
  
  unique(values[!is.na(values) & nzchar(values)])
}

get_institution_rors <- function(work) {
  institutions <- get_institutions(work)
  
  if (length(institutions) == 0L) {
    return(character(0))
  }
  
  values <- vapply(
    institutions,
    function(institution) {
      normalize_ror(institution$ror)
    },
    character(1)
  )
  
  unique(values[!is.na(values) & nzchar(values)])
}

get_lineage_ids <- function(work) {
  institutions <- get_institutions(work)
  
  values <- unlist(
    lapply(
      institutions,
      function(institution) {
        lineage <- institution$lineage %||% list()
        
        if (length(lineage) == 0L) {
          return(character(0))
        }
        
        vapply(
          lineage,
          normalize_openalex_id,
          character(1)
        )
      }
    ),
    use.names = FALSE
  )
  
  unique(values[!is.na(values) & nzchar(values)])
}

# Some OpenAlex authorships have a separate affiliations mapping.
get_affiliation_institution_ids <- function(work) {
  authorships <- get_authorships(work)
  
  values <- unlist(
    lapply(
      authorships,
      function(authorship) {
        affiliations <- authorship$affiliations %||% list()
        
        unlist(
          lapply(
            affiliations,
            function(affiliation) {
              ids <- affiliation$institution_ids %||% list()
              
              if (length(ids) == 0L) {
                return(character(0))
              }
              
              vapply(
                ids,
                normalize_openalex_id,
                character(1)
              )
            }
          ),
          use.names = FALSE
        )
      }
    ),
    use.names = FALSE
  )
  
  unique(values[!is.na(values) & nzchar(values)])
}

get_raw_affiliations <- function(work) {
  authorships <- get_authorships(work)
  
  values <- unlist(
    lapply(
      authorships,
      function(authorship) {
        authorship$raw_affiliation_strings %||% list()
      }
    ),
    recursive = TRUE,
    use.names = FALSE
  )
  
  values <- as.character(values)
  unique(values[!is.na(values) & nzchar(trimws(values))])
}

has_ua_raw_text <- function(work) {
  values <- get_raw_affiliations(work)
  
  if (length(values) == 0L) {
    return(FALSE)
  }
  
  any(
    grepl(
      paste(
        "University of Arizona",
        "UArizona",
        "Univ(?:ersity)?\\.? of Ariz",
        "Tucson.*(?:Arizona|AZ)",
        "AZ\\s*857",
        sep = "|"
      ),
      values,
      ignore.case = TRUE,
      perl = TRUE
    )
  )
}

diagnose_ua_ror <- function(work) {
  authorships <- get_authorships(work)
  
  institution_ids <- get_institution_ids(work)
  institution_rors <- get_institution_rors(work)
  lineage_ids <- get_lineage_ids(work)
  affiliation_ids <- get_affiliation_institution_ids(work)
  
  exact_ua_id <- UA_ID %in% institution_ids
  exact_ua_ror <- UA_ROR %in% institution_rors
  ua_in_lineage <- UA_ID %in% lineage_ids
  ua_in_affiliations <- UA_ID %in% affiliation_ids
  ua_in_raw <- has_ua_raw_text(work)
  at_cap <- length(authorships) >= 100L
  
  reason <- if (exact_ua_ror) {
    "Exact UA ROR present"
  } else if (exact_ua_id) {
    "UA ID present, but UA ROR missing from institution object"
  } else if (ua_in_lineage) {
    "Child institution present; UA appears only in lineage"
  } else if (ua_in_affiliations) {
    "UA appears only in authorship affiliations mapping"
  } else if (ua_in_raw) {
    "UA appears in raw affiliation text but not linked to UA institution"
  } else if (at_cap) {
    "No returned UA object; authorships payload is at 100-author cap"
  } else {
    "No UA signal in returned authorship payload"
  }
  
  data.frame(
    openalex_id = normalize_openalex_id(work$id),
    title = first_text(work$title),
    n_authorships = length(authorships),
    at_100_author_cap = at_cap,
    exact_ua_id_present = exact_ua_id,
    exact_ua_ror_present = exact_ua_ror,
    ua_in_lineage = ua_in_lineage,
    ua_in_affiliations = ua_in_affiliations,
    ua_in_raw_text = ua_in_raw,
    any_institution_ror_present = length(institution_rors) > 0L,
    reason = reason,
    institution_rors = if (length(institution_rors)) {
      paste(institution_rors, collapse = "; ")
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
}

ua_ror_check <- do.call(
  rbind,
  lapply(works_raw, diagnose_ua_ror)
)

# Overall summary
ua_ror_summary <- data.frame(
  measure = c(
    "Total works",
    "Exact UA ROR present",
    "Exact UA ROR absent",
    "Any institutional ROR present",
    "No institutional ROR present",
    "UA ID present but UA ROR absent",
    "UA absent as exact ID but present in lineage",
    "UA absent as exact ID but present in affiliations",
    "UA absent as exact ID but present in raw text",
    "UA ROR absent and authorships at 100-author cap"
  ),
  count = c(
    nrow(ua_ror_check),
    sum(ua_ror_check$exact_ua_ror_present),
    sum(!ua_ror_check$exact_ua_ror_present),
    sum(ua_ror_check$any_institution_ror_present),
    sum(!ua_ror_check$any_institution_ror_present),
    sum(
      ua_ror_check$exact_ua_id_present &
        !ua_ror_check$exact_ua_ror_present
    ),
    sum(
      !ua_ror_check$exact_ua_id_present &
        ua_ror_check$ua_in_lineage
    ),
    sum(
      !ua_ror_check$exact_ua_id_present &
        ua_ror_check$ua_in_affiliations
    ),
    sum(
      !ua_ror_check$exact_ua_id_present &
        ua_ror_check$ua_in_raw_text
    ),
    sum(
      !ua_ror_check$exact_ua_ror_present &
        ua_ror_check$at_100_author_cap
    )
  ),
  stringsAsFactors = FALSE
)

ua_ror_summary$percent <- round(
  100 * ua_ror_summary$count / nrow(ua_ror_check),
  2
)

cat("\n================ UA ROR SUMMARY ================\n")
print(ua_ror_summary, row.names = FALSE)

cat("\n================ CAUSE BREAKDOWN ================\n")
print(
  sort(
    table(
      ua_ror_check$reason[
        !ua_ror_check$exact_ua_ror_present
      ]
    ),
    decreasing = TRUE
  )
)

# Check whether the UA ID and UA ROR always occur together.
id_ror_comparison <- with(
  ua_ror_check,
  table(
    exact_ua_id_present,
    exact_ua_ror_present,
    useNA = "always"
  )
)

cat("\n================ UA ID vs UA ROR ================\n")
print(id_ror_comparison)

ua_ror_missing <- ua_ror_check[
  !ua_ror_check$exact_ua_ror_present,
  ,
  drop = FALSE
]

utils::write.csv(
  ua_ror_check,
  "ua_ror_check.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  ua_ror_summary,
  "ua_ror_summary.csv",
  row.names = FALSE,
  na = ""
)

utils::write.csv(
  ua_ror_missing,
  "ua_ror_missing_review.csv",
  row.names = FALSE,
  na = ""
)

message("Saved UA ROR diagnostic files.")

