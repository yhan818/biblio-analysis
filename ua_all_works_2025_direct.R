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
YR          <- 2025
MAILTO      <- "you@arizona.edu"     # <-- EDIT: polite pool
API_KEY     <- Sys.getenv("OPENALEX_KEY", unset = "")  # optional
PER_PAGE    <- 200                   # API maximum
INCLUDE_ABSTRACT <- FALSE            # TRUE roughly triples payload size
BASE        <- "https://api.openalex.org/works"

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


UA_ROR <- "https://ror.org/03m2x1q45"   # verify against /institutions/I138006243
UA_ID  <- "I138006243"

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
 
  
  tibble(
    # ---------- identity ----------
    id                = short_id(w$id),
    openalex_url      = s_chr(w, "id"),
    doi               = sub("^https?://doi\\.org/", "", s_chr(w, "doi")),
    pmid              = sub("^https?://pubmed\\.ncbi\\.nlm\\.nih\\.gov/", "",
                            s_chr(w, "ids", "pmid")),
    pmcid             = s_chr(w, "ids", "pmcid"),
    title             = s_chr(w, "title"),
    publication_year  = s_int(w, "publication_year"),
    publication_date  = s_chr(w, "publication_date"),
    language          = s_chr(w, "language"),
    type              = s_chr(w, "type"),
    raw_type          = dplyr::coalesce(s_chr(w, "raw_type"),
                                        s_chr(w, "type_crossref")),
    
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
    primary_source_type       = s_chr(pl, "source", "type"),
    primary_source_issn_l     = s_chr(pl, "source", "issn_l"),
    primary_source_is_in_doaj = s_lgl(pl, "source", "is_in_doaj"),
    primary_publisher         = s_chr(pl, "source", "host_organization_name"),
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
    authorships_maybe_capped  = n_listed >= 100,
    
    # UA-specific flags derived from the authorships array
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
    funder_names = join_map(w$funders %||% w$grants,
                            function(e) e$display_name %||% e$funder_display_name),
    funder_ids   = join_map(w$funders %||% w$grants,
                            function(e) short_id(e$id %||% e$funder)),
    award_ids    = join_map(w$awards %||% w$grants,
                            function(e) e$award_id),
    
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
    collab_class = case_when(
      is.na(countries_distinct_count) ~ "unknown",
      countries_distinct_count <= 1   ~ "1 country (domestic)",
      countries_distinct_count == 2   ~ "2 countries",
      countries_distinct_count <= 5   ~ "3-5 countries",
      countries_distinct_count <= 10  ~ "6-10 countries",
      TRUE                            ~ "11+ countries"
    ),
    collab_class = factor(collab_class, levels = c(
      "1 country (domestic)", "2 countries", "3-5 countries",
      "6-10 countries", "11+ countries", "unknown")),
    publication_date = as.Date(publication_date),
    pub_month = format(publication_date, "%Y-%m")
  )

# --- Hard checks ---
stopifnot(!any(vapply(ua, is.list, logical(1))))   # fully rectangular

reconciliation <- tibble(
  check = c("API count_only", "Records fetched", "Difference",
            "Rows flattened", "Unique ids", "Duplicate ids",
            "countries_distinct_count present",
            "institutions_distinct_count present",
            "best_oa_location present (non-null)",
            "Works at/over 100-author cap"),
  value = c(
    expected_n,
    length(works_raw),
    expected_n - length(works_raw),
    nrow(ua),
    dplyr::n_distinct(ua$id),
    nrow(ua) - dplyr::n_distinct(ua$id),
    sum(!is.na(ua$countries_distinct_count)),
    sum(!is.na(ua$institutions_distinct_count)),
    sum(ua$best_oa_exists),
    sum(ua$authorships_maybe_capped, na.rm = TRUE)
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
  group_by(collab_class) |>
  summarise(
    n_works      = n(),
    n_oa         = sum(oa_status != "closed", na.rm = TRUE),
    oa_rate      = round(sum(oa_status != "closed", na.rm = TRUE) / n() * 100, 1),
    median_insts = median(institutions_distinct_count, na.rm = TRUE),
    max_insts    = max(institutions_distinct_count, na.rm = TRUE),
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
    works_capped_authorship = sum(authorships_maybe_capped, na.rm = TRUE)
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

# --- 9a: Column ordering, analysis columns first ---
front <- c(
  "id", "doi", "title", "publication_date", "publication_year",
  "type", "language",
  "is_oa", "oa_status", "oa_binary", "oa_url",
  "best_oa_exists", "best_oa_venue_class", "best_oa_version",
  "best_oa_license", "best_oa_source_name", "best_oa_source_type",
  "best_oa_pdf_url", "best_oa_landing_page_url",
  "countries_distinct_count", "institutions_distinct_count",
  "collab_class",
  "topic_domain", "topic_field", "topic_subfield", "topic_name",
  "primary_source_name", "primary_publisher", "primary_source_is_in_doaj",
  "cited_by_count", "fwci",
  "ua_author_count", "ua_exact_parent_count", "ua_detect_zero_reason",
  "ua_is_first_author", "ua_is_corresponding",
  "authors_listed_in_payload", "authorships_maybe_capped",
  "apc_paid_value_usd", "apc_list_value_usd"
)
front <- intersect(front, names(ua))

ua_out <- ua |> select(all_of(front), everything())

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

