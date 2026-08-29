## ============================================================
## CQN — Step 8: verification of the delivered files
## ============================================================
library(dplyr); library(stringr); library(tibble); library(purrr)

RESULTS <- list()

check <- function(label, pass, detail = "") {
  pass <- isTRUE(all(pass))
  RESULTS[[length(RESULTS) + 1L]] <<- tibble::tibble(
    check = label, pass = pass, detail = as.character(detail)
  )
  message(sprintf("[%s] %s%s", if (pass) "PASS" else "FAIL", label,
                  if (nzchar(detail)) paste0(" -- ", detail) else ""))
  invisible(pass)
}

AUTH_SEP <- " | "; INST_SEP <- " + "; LIST_SEP <- "; "

## segment count = separators + 1, NA-safe
n_seg <- function(x, sep) {
  out <- stringr::str_count(x, stringr::fixed(sep)) + 1L
  out[is.na(x)] <- NA_integer_
  as.integer(out)
}

## ---- read both deliverables back as pure character ----------
csv_back <- readr::read_csv("cqn/out/CQN_works_wide.csv",
                            col_types = readr::cols(.default = "c"),
                            progress = FALSE)
xl_back  <- readxl::read_excel("cqn/out/CQN_works.xlsx", sheet = "works",
                               col_types = "text")
aw_back  <- readr::read_csv("cqn/out/CQN_awards_long.csv",
                            col_types = readr::cols(.default = "c"),
                            progress = FALSE)

check("CSV row count == 1194", nrow(csv_back) == 1194L, nrow(csv_back))
check("XLSX row count == 1194", nrow(xl_back) == 1194L, nrow(xl_back))
check("CSV/XLSX same column count", ncol(csv_back) == ncol(xl_back),
      paste(ncol(csv_back), "vs", ncol(xl_back)))
check("Column names identical", identical(names(csv_back), names(xl_back)))

## ---- 8b. Round-trip fidelity --------------------------------

## keys match the deduped input list from the source workbook [1]
check("All requested IDs present in CSV",
      length(setdiff(work_ids, csv_back$requested_work_id)) == 0)
check("No extra IDs in CSV",
      length(setdiff(csv_back$requested_work_id, work_ids)) == 0)
check("CSV keys unique", sum(duplicated(csv_back$requested_work_id)) == 0)
check("XLSX keys match CSV keys",
      setequal(xl_back$requested_work_id, csv_back$requested_work_id))

## type-aware cell comparison: in-memory vs on-disk
compare_tbl <- function(mem, disk, label) {
  mem  <- dplyr::arrange(mem,  requested_work_id)
  disk <- dplyr::arrange(disk, requested_work_id)
  common <- intersect(names(mem), names(disk))
  
  purrr::map_dfr(common, function(cl) {
    a <- mem[[cl]]
    b <- disk[[cl]]
    
    if (is.numeric(a)) {
      an <- as.numeric(a)
      bn <- suppressWarnings(as.numeric(b))
      bad <- !((is.na(an) & is.na(bn)) |
                 (!is.na(an) & !is.na(bn) & abs(an - bn) < 1e-8))
      
    } else if (is.logical(a)) {
      al <- as.logical(a)
      bl <- suppressWarnings(as.logical(toupper(trimws(as.character(b)))))
      bad <- !((is.na(al) & is.na(bl)) |
                 (!is.na(al) & !is.na(bl) & al == bl))
      
    } else {
      ac <- trimws(as.character(a))
      bc <- trimws(as.character(b))
      ac[!is.na(ac) & !nzchar(ac)] <- NA_character_   # na="" round-trip
      bc[!is.na(bc) & !nzchar(bc)] <- NA_character_
      bad <- !((is.na(ac) & is.na(bc)) |
                 (!is.na(ac) & !is.na(bc) & ac == bc))
    }
    
    tibble::tibble(
      source  = label,
      field   = cl,
      r_class = class(a)[1],
      n_diff  = sum(bad)
    )
  })
}

cmp_csv <- compare_tbl(cqn_wide, csv_back, "csv")
cmp_xl  <- compare_tbl(cqn_wide, xl_back,  "xlsx")

check("CSV round-trips with zero cell differences",
      sum(cmp_csv$n_diff) == 0,
      paste(sum(cmp_csv$n_diff), "differing cells"))
check("XLSX round-trips with zero cell differences",
      sum(cmp_xl$n_diff) == 0,
      paste(sum(cmp_xl$n_diff), "differing cells"))

## inspect any offenders
dplyr::filter(cmp_csv, n_diff > 0)
dplyr::filter(cmp_xl,  n_diff > 0)

## ---- 8c. Delimiter integrity --------------------------------
## ============================================================
## 8c. Delimiter integrity
## ============================================================

AWARD_SEP <- " | "

d <- cqn_wide |>
  dplyr::mutate(
    n_ctry_slot   = n_seg(authorships_countries,    AUTH_SEP),
    n_inst_slot   = n_seg(authorships_institutions, AUTH_SEP),
    n_names       = n_seg(author_names_clean,       LIST_SEP),
    n_inst_uniq   = n_seg(institution_names_clean,  LIST_SEP),
    n_ctry_uniq   = n_seg(distinct_countries_list,  LIST_SEP),
    n_funders     = n_seg(funder_display_names,     LIST_SEP),
    n_award_ids   = n_seg(award_ids,                AWARD_SEP),
    n_award_names = n_seg(award_display_names,      AWARD_SEP),
    n_award_dois  = n_seg(award_dois,               AWARD_SEP)
  )

## ---- positional fields: exactly one slot per author ----------

check("authorships_countries slots == authors_count",
      all(dplyr::if_else(d$authors_count == 0,
                         is.na(d$authorships_countries),
                         d$n_ctry_slot == d$authors_count)),
      sum(!dplyr::if_else(d$authors_count == 0,
                          is.na(d$authorships_countries),
                          d$n_ctry_slot == d$authors_count), na.rm = TRUE))

check("authorships_institutions slots == authors_count",
      all(dplyr::if_else(d$authors_count == 0,
                         is.na(d$authorships_institutions),
                         d$n_inst_slot == d$authors_count)))

check("positional fields share one authorship spine",
      all(dplyr::if_else(d$authors_count == 0,
                         is.na(d$n_ctry_slot) & is.na(d$n_inst_slot),
                         d$n_ctry_slot == d$n_inst_slot)))

## ---- author names: one per author -----------------------------
## (fails only if a display_name literally contains "; ")

check("author_names_clean items == authors_count",
      all(dplyr::if_else(d$authors_count == 0,
                         is.na(d$author_names_clean),
                         d$n_names == d$authors_count)))

## ---- deduplicated lists: match their OWN distinct counts ------
## (must NOT be compared to authors_count -- these are deduped)

check("institution_names_clean items == institutions_distinct_computed",
      all(dplyr::if_else(d$institutions_distinct_computed == 0,
                         is.na(d$institution_names_clean),
                         d$n_inst_uniq == d$institutions_distinct_computed)))

check("distinct_countries_list items == countries_distinct_computed",
      all(dplyr::if_else(d$countries_distinct_computed == 0,
                         is.na(d$distinct_countries_list),
                         d$n_ctry_uniq == d$countries_distinct_computed)))

check("funder_display_names items == funder_count",
      all(dplyr::if_else(d$funder_count == 0,
                         is.na(d$funder_display_names),
                         d$n_funders == d$funder_count)))

## ---- award fields: each checked against the count it actually matches ----

check("award_ids items == award_ids_count",
      all(dplyr::if_else(d$award_ids_count == 0,
                         is.na(d$award_ids),
                         d$n_award_ids == d$award_ids_count)))

check("award_ids items == award_count (100% grant-ID fill)",
      all(dplyr::if_else(d$award_count == 0,
                         is.na(d$award_ids),
                         d$n_award_ids == d$award_count)))

## EXPECTED TO FAIL: titles are dropped when NA by cc(), so award_display_names
## is NOT positionally aligned with award_count -- this documents that gap
check("award_display_names items == award_count (positional)",
      all(dplyr::if_else(d$award_count == 0,
                         is.na(d$award_display_names),
                         d$n_award_names == d$award_count)),
      "expected FAIL: NA titles dropped by cc(), field is not positional")

check("award_display_names items == award_title_count",
      all(dplyr::if_else(d$award_title_count == 0,
                         is.na(d$award_display_names),
                         d$n_award_names == d$award_title_count)))

## ---- how bad is the title misalignment, concretely -----------

misaligned <- d |>
  dplyr::filter(award_count > 0, award_title_count > 0,
                award_title_count < award_count) |>
  dplyr::select(requested_work_id, award_count, award_title_count,
                n_award_ids, n_award_names)

nrow(misaligned)
print(head(misaligned, 10), width = Inf)

## ---- delimiter collisions: scan SOURCE values, not collapsed cells -------
## (checking the collapsed cell for a stray delimiter is circular --
##  you can't tell a separator from a literal character once merged)

collide_scan <- purrr::map_dfr(works_raw_full, function(w) {
  a  <- w$authorships %||% list()
  nm <- if (length(a)) vapply(a, author_name_of, character(1)) else character(0)
  it <- if (length(a)) unlist(lapply(a, inst_names_of), use.names = FALSE) else character(0)
  ct <- if (length(a)) unlist(lapply(a, countries_of),  use.names = FALSE) else character(0)
  
  aw  <- w$awards  %||% list()
  ids <- if (length(aw)) vapply(aw, function(x) chr1(x$funder_award_id), character(1)) else character(0)
  fnm <- if (length(aw)) vapply(aw, function(x) chr1(x$funder_display_name), character(1)) else character(0)
  
  hs <- function(v, pat) any(grepl(pat, v, fixed = TRUE), na.rm = TRUE)
  
  tibble::tibble(
    requested_work_id = strip_oa(w$id),
    name_semi   = hs(nm,  ";"),      # would break author_names_clean ("; ")
    inst_semi   = hs(it,  ";"),      # would break institution_names_clean ("; ")
    inst_plus   = hs(it,  " + "),    # would break authorships_institutions
    ctry_pipe   = hs(ct,  "|"),      # would break authorships_countries
    ctry_comma  = hs(ct,  ","),      # would break authorships_countries
    award_pipe  = hs(ids, "|"),      # would break award_ids
    award_semi  = hs(ids, ";"),      # harmless: award_ids uses " | ", not "; "
    funder_semi = hs(fnm, ";")       # would break funder_display_names
  )
})

check("no author name contains ';'",        !any(collide_scan$name_semi))
check("no institution name contains ';'",   !any(collide_scan$inst_semi))
check("no institution name contains ' + '", !any(collide_scan$inst_plus))
check("no country code contains '|'",       !any(collide_scan$ctry_pipe))
check("no country code contains ','",       !any(collide_scan$ctry_comma))
check("no grant ID contains '|'",           !any(collide_scan$award_pipe))
check("no funder name contains ';'",        !any(collide_scan$funder_semi))

## expected: 13 works with ';' inside a grant ID -- benign, since award_ids
## uses " | " as its delimiter, not "; "
check("grant IDs containing ';' are isolated and benign",
      sum(collide_scan$award_semi) == 13L,
      paste(sum(collide_scan$award_semi), "works affected"))

## ---- roll-up ---------------------------------------------------

results_8c <- dplyr::bind_rows(RESULTS)
print(results_8c, n = 30)
dplyr::filter(results_8c, !pass)

results_all <- dplyr::bind_rows(RESULTS)

readr::write_csv(results_all, "cqn/out/CQN_validation_log.csv", na = "")

message(sprintf(
  "\nValidation: %d/%d checks passed (%d expected-fail documented).",
  sum(results_all$pass), nrow(results_all),
  sum(!results_all$pass & grepl("^expected FAIL", results_all$detail))
))