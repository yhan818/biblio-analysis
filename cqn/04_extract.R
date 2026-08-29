## ============================================================
## CQN — added metadata fields + user-friendly fields
## Input:  works_raw_full  (list of 1194 raw OpenAlex work records)
## ============================================================

library(dplyr)
library(purrr)
library(tibble)

## ---- helpers ----------------------------------------------

strip_oa <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  sub("^https?://openalex\\.org/", "", as.character(x)[[1]])
}


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

chr1 <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  as.character(x)[[1]]
}

int1 <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(x)[[1]])
}

lgl1 <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA)
  }
  as.logical(x)[[1]]
}

## collapse a vector into one delimited cell
cc <- function(x, sep = "; ", dedupe = FALSE) {
  x <- as.character(unlist(x, use.names = FALSE))
  x <- x[!is.na(x)]
  x <- x[nzchar(x)]
  if (isTRUE(dedupe)) {
    x <- unique(x)
  }
  if (length(x) == 0) {
    return(NA_character_)
  }
  paste(x, collapse = sep)
}

## ---- per-authorship accessors -----------------------------

author_name_of <- function(au) {
  chr1(au$author$display_name)
}

inst_names_of <- function(au) {
  insts <- au$institutions %||% list()
  if (length(insts) == 0) {
    return(character(0))
  }
  out <- vapply(insts, function(i) chr1(i$display_name), character(1))
  out <- out[!is.na(out)]
  out[nzchar(out)]
}

## prefer authorship-level `countries`; fall back to institution country_code
countries_of <- function(au) {
  native <- as.character(unlist(au$countries %||% list(), use.names = FALSE))
  native <- native[!is.na(native)]
  native <- native[nzchar(native)]
  if (length(native) > 0) {
    return(native)
  }
  insts <- au$institutions %||% list()
  if (length(insts) == 0) {
    return(character(0))
  }
  fb <- vapply(insts, function(i) chr1(i$country_code), character(1))
  fb <- fb[!is.na(fb)]
  fb[nzchar(fb)]
}

## ---- USER-FRIENDLY + ADDED fields -------------------------

extract_people <- function(w) {
  
  a <- w$authorships %||% list()
  n <- length(a)
  
  if (n == 0) {
    author_names   <- character(0)
    inst_by_author <- list()
    ctry_by_author <- list()
    corr_idx       <- integer(0)
    native_ctry    <- FALSE
  } else {
    author_names   <- vapply(a, author_name_of, character(1))
    inst_by_author <- lapply(a, inst_names_of)
    ctry_by_author <- lapply(a, countries_of)
    is_corr        <- vapply(a, function(au) isTRUE(lgl1(au$is_corresponding)), logical(1))
    corr_idx       <- which(is_corr)
    native_len     <- vapply(a, function(au) length(au$countries %||% list()), integer(1))
    native_ctry    <- any(native_len > 0)
  }
  
  ## provenance of the country values
  ## provenance of the country values
  if (isTRUE(native_ctry)) {
    ctry_src <- "authorship.countries"
  } else if (length(unlist(ctry_by_author, use.names = FALSE)) > 0) {
    ctry_src <- "institution.country_code"
  } else {
    ctry_src <- NA_character_
  }
  
  ## ADDED: authorships.countries
  ## " | " between authors (order preserved); "," within one author
  if (n == 0) {
    authorships_countries <- NA_character_
  } else {
    per_author_ctry <- vapply(
      ctry_by_author,
      function(x) {
        if (length(x) == 0) {
          return("")
        }
        paste(x, collapse = ",")
      },
      character(1)
    )
    authorships_countries <- paste(per_author_ctry, collapse = " | ")
  }
  
  ## institutions per author, positionally aligned
  ## " + " within an author, because institution names contain commas
  if (n == 0) {
    authorships_institutions <- NA_character_
  } else {
    per_author_inst <- vapply(
      inst_by_author,
      function(x) {
        if (length(x) == 0) {
          return("")
        }
        paste(x, collapse = " + ")
      },
      character(1)
    )
    authorships_institutions <- paste(per_author_inst, collapse = " | ")
  }
  
  ## first / last author
  if (n == 0) {
    first_author <- NA_character_
    last_author  <- NA_character_
  } else {
    first_author <- author_names[[1]]
    last_author  <- author_names[[n]]
  }
  
  ## corresponding authors
  if (length(corr_idx) == 0) {
    corr_names <- NA_character_
    corr_insts <- NA_character_
  } else {
    corr_names <- cc(author_names[corr_idx])
    corr_insts <- cc(unlist(inst_by_author[corr_idx], use.names = FALSE), dedupe = TRUE)
  }
  
  ## flattened vectors for the deduped list fields
  ctry_flat <- unlist(ctry_by_author, use.names = FALSE)
  inst_flat <- unlist(inst_by_author, use.names = FALSE)
  ctry_flat <- ctry_flat[!is.na(ctry_flat) & nzchar(ctry_flat)]
  inst_flat <- inst_flat[!is.na(inst_flat) & nzchar(inst_flat)]
  
  tibble::tibble(
    requested_work_id = strip_oa(w$id),
    
    ## USER-FRIENDLY 1 -- names only, no IDs, no affiliations, order preserved
    authors_count      = as.integer(n),
    author_names_clean = cc(author_names),
    first_author_name  = first_author,
    last_author_name   = last_author,
    
    ## USER-FRIENDLY 2 -- institution names only, no IDs
    institution_names_clean  = cc(inst_flat, dedupe = TRUE),
    authorships_institutions = authorships_institutions,
    
    ## ADDED -- authorships.countries
    authorships_countries     = authorships_countries,
    authorships_countries_src = ctry_src,
    distinct_countries_list   = cc(ctry_flat, dedupe = TRUE),
    
    ## ADDED -- OpenAlex's own counts (authoritative)
    countries_distinct_count    = int1(w$countries_distinct_count),
    institutions_distinct_count = int1(w$institutions_distinct_count),
    
    ## locally recomputed, so any disagreement is visible
    countries_distinct_computed    = length(unique(ctry_flat)),
    institutions_distinct_computed = length(unique(inst_flat)),
    
    corresponding_author_names      = corr_names,
    corresponding_institution_names = corr_insts,
    authorships_truncated_flag      = n >= 100
  )
}
  

## ---- ADDED: funder / award fields -------------------------
## Requested as grants.funder_display_name and grants.award_id.
## This payload carries `awards` and `funders` (no `grants`),
## so these read from awards[].funder_display_name and
## awards[].funder_award_id, labeled with your field names.

AWARD_SEP <- " | "   # safe: grant IDs contain ";" but not "|"

extract_awards <- function(w) {
  
  aw <- w$awards  %||% list()
  fu <- w$funders %||% list()
  
  if (length(aw) == 0) {
    f_from_aw <- character(0); ids_aw <- character(0)
    names_aw  <- character(0); doi_aw <- character(0)
  } else {
    f_from_aw <- vapply(aw, function(a) chr1(a$funder_display_name), character(1))
    ids_aw    <- vapply(aw, function(a) chr1(a$funder_award_id),     character(1))
    names_aw  <- vapply(aw, function(a) chr1(a$display_name),        character(1))
    doi_aw    <- vapply(aw, function(a) chr1(a$doi),                 character(1))
  }
  
  if (length(fu) == 0) {
    f_from_fu <- character(0)
  } else if (is.character(fu)) {
    f_from_fu <- as.character(fu)
  } else {
    f_from_fu <- vapply(fu, function(f) chr1(f$display_name), character(1))
  }
  
  all_funders <- c(f_from_aw, f_from_fu)
  all_funders <- all_funders[!is.na(all_funders) & nzchar(all_funders)]
  
  ids_clean <- ids_aw[!is.na(ids_aw) & nzchar(ids_aw)]
  
  tibble::tibble(
    ## ADDED (requested as grants.funder_display_name)
    funder_display_names = cc(all_funders, dedupe = TRUE),
    ## ADDED (requested as grants.award_id) -- " | " delimited
    award_ids            = cc(ids_aw, sep = AWARD_SEP),
    award_display_names  = cc(names_aw, sep = AWARD_SEP),
    award_dois           = cc(doi_aw,   sep = AWARD_SEP),
    award_count          = as.integer(length(aw)),
    award_ids_count      = as.integer(length(ids_clean)),   # from vector, not string
    award_title_count    = as.integer(sum(!is.na(names_aw))),
    funder_count         = as.integer(length(unique(all_funders))),
    has_funding_data     = length(all_funders) > 0,
    has_awards           = length(aw) > 0,
    award_id_has_delim   = any(grepl(";", ids_clean, fixed = TRUE))
  )
}






awards_long_of <- function(w) {
  aw <- w$awards %||% list()
  if (length(aw) == 0) {
    return(NULL)
  }
  wid <- strip_oa(w$id)
  rows <- lapply(seq_along(aw), function(i) {
    a <- aw[[i]]
    tibble::tibble(
      requested_work_id   = wid,
      award_position      = as.integer(i),
      award_openalex_id   = strip_oa(chr1(a$id)),
      award_display_name  = chr1(a$display_name),
      funder_award_id     = chr1(a$funder_award_id),
      funder_openalex_id  = strip_oa(chr1(a$funder_id)),
      funder_display_name = chr1(a$funder_display_name),
      award_doi           = chr1(a$doi)
    )
  })
  dplyr::bind_rows(rows)
}


## ---- extra helpers ----------------------------------------

dbl1 <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x)[[1]])
}

rebuild_abstract <- function(aii) {
  if (is.null(aii) || length(aii) == 0) return(NA_character_)
  pos <- unlist(aii, use.names = FALSE)
  wrd <- rep(names(aii), lengths(aii))
  paste(wrd[order(pos)], collapse = " ")
}

## pull display_name from a list of topic/keyword/SDG objects
names_of <- function(lst, field = "display_name", n = Inf) {
  if (is.null(lst) || length(lst) == 0) return(character(0))
  out <- vapply(lst, function(x) chr1(x[[field]]), character(1))
  out <- out[!is.na(out) & nzchar(out)]
  if (is.finite(n)) out <- utils::head(out, n)
  out
}

## ---- bibliographic / venue / OA / impact / topics ---------
## ---- bibliographic / venue / OA / impact / topics ---------

extract_meta <- function(w) {
  
  pl  <- w$primary_location  %||% list()
  pls <- pl$source           %||% list()
  bol <- w$best_oa_location  %||% list()
  bos <- bol$source          %||% list()
  oa  <- w$open_access       %||% list()
  bib <- w$biblio            %||% list()
  ids <- w$ids               %||% list()
  pt  <- w$primary_topic     %||% list()
  cnp <- w$citation_normalized_percentile %||% list()
  cby <- w$cited_by_percentile_year       %||% list()
  apl <- w$apc_list          %||% list()
  apd <- w$apc_paid          %||% list()
  hc  <- w$has_content       %||% list()
  
  wtype <- chr1(w$type)
  
  ## paratext: deprecated field, now derived from type
  para_derived <- NA
  if (!is.na(wtype)) {
    para_derived <- identical(wtype, "paratext")
  }
  
  tibble::tibble(
    requested_work_id    = strip_oa(w$id),
    returned_openalex_id = strip_oa(w$id),
    
    ## --- identifiers
    doi   = chr1(ids$doi),
    pmid  = chr1(ids$pmid),
    pmcid = chr1(ids$pmcid),
    mag   = chr1(ids$mag),
    
    ## --- bibliographic
    title            = chr1(w$display_name),
    type             = wtype,
    raw_type         = chr1(w$raw_type),
    type_crossref    = chr1(w$type_crossref),
    language         = chr1(w$language),
    publication_year = int1(w$publication_year),
    publication_date = chr1(w$publication_date),
    volume     = chr1(bib$volume),
    issue      = chr1(bib$issue),
    first_page = chr1(bib$first_page),
    last_page  = chr1(bib$last_page),
    indexed_in   = cc(unlist(w$indexed_in %||% list(), use.names = FALSE)),
    created_date = chr1(w$created_date),
    updated_date = chr1(w$updated_date),
    
    ## --- venue (primary_location)
    source_display_name    = chr1(pls$display_name),
    source_id              = strip_oa(chr1(pls$id)),
    source_type            = chr1(pls$type),
    issn_l                 = chr1(pls$issn_l),
    issn_all               = cc(unlist(pls$issn %||% list(), use.names = FALSE)),
    host_organization_id   = strip_oa(chr1(pls$host_organization)),
    host_organization_name = chr1(pls$host_organization_name),
    primary_location_version      = chr1(pl$version),
    primary_location_license      = chr1(pl$license),
    primary_location_is_oa        = lgl1(pl$is_oa),
    primary_location_landing_page = chr1(pl$landing_page_url),
    primary_location_pdf_url      = chr1(pl$pdf_url),
    locations_count               = int1(w$locations_count),
    
    ## --- open access
    is_oa       = lgl1(oa$is_oa),
    oa_status   = chr1(oa$oa_status),
    oa_url      = chr1(oa$oa_url),
    any_repository_has_fulltext = lgl1(oa$any_repository_has_fulltext),
    best_oa_source_name    = chr1(bos$display_name),
    best_oa_source_type    = chr1(bos$type),
    best_oa_license        = chr1(bol$license),
    best_oa_version        = chr1(bol$version),
    best_oa_landing_page   = chr1(bol$landing_page_url),
    best_oa_pdf_url        = chr1(bol$pdf_url),
    apc_list_value_usd     = int1(apl$value_usd),
    apc_paid_value_usd     = int1(apd$value_usd),
    apc_paid_provenance    = chr1(apd$provenance),
    
    ## --- fulltext availability
    has_fulltext        = lgl1(w$has_fulltext),
    has_content_pdf     = lgl1(hc$pdf),
    has_content_grobid  = lgl1(hc$grobid_xml),
    
    ## --- impact
    cited_by_count         = int1(w$cited_by_count),
    fwci                   = dbl1(w$fwci),
    cnp_value              = dbl1(cnp$value),
    is_in_top_1_percent    = lgl1(cnp$is_in_top_1_percent),
    is_in_top_10_percent   = lgl1(cnp$is_in_top_10_percent),
    cited_by_pctile_min    = dbl1(cby$min),
    cited_by_pctile_max    = dbl1(cby$max),
    referenced_works_count = int1(w$referenced_works_count),
    related_works_count    = as.integer(length(w$related_works %||% list())),
    
    ## --- flags
    is_retracted    = lgl1(w$is_retracted),
    is_paratext     = para_derived,
    is_paratext_raw = lgl1(w$is_paratext),
    is_xpac         = lgl1(w$is_xpac),
    
    ## --- aboutness
    ## --- aboutness
    primary_topic       = chr1(pt$display_name),
    primary_topic_id    = strip_oa(chr1(pt$id)),
    primary_topic_score = dbl1(pt$score),
    primary_subfield    = chr1((pt$subfield %||% list())$display_name),
    primary_field       = chr1((pt$field    %||% list())$display_name),
    primary_domain      = chr1((pt$domain   %||% list())$display_name),
    
    topics_all   = cc(names_of(w$topics, "display_name", 3)),
    topics_count = as.integer(length(w$topics %||% list())),
    
    keywords       = cc(names_of(w$keywords, "display_name")),
    keywords_count = as.integer(length(w$keywords %||% list())),
    
    concepts       = cc(names_of(w$concepts, "display_name", 5)),
    concepts_count = as.integer(length(w$concepts %||% list())),
    
    sdg_names  = cc(names_of(w$sustainable_development_goals, "display_name")),
    sdg_count  = as.integer(length(w$sustainable_development_goals %||% list())),
    
    ## --- abstract
    abstract     = rebuild_abstract(w$abstract_inverted_index),
    has_abstract = !is.null(w$abstract_inverted_index) &&
      length(w$abstract_inverted_index) > 0
  )
}

