# authors_diff.R
options(warn = 1) # Print warnings immediately

library(readr)
library(readxl)
library(dplyr)
library(stringr)

# --- 1. Read Old CSV ---
csv_file <- "ua_asu_unm_grant_authors2.csv"
if (!file.exists(csv_file)) stop("CSV missing")

# Simple manual parse since format is tricky
lines <- readLines(csv_file, warn = FALSE)
# Remove header
if (length(lines) > 0 && grepl("Center Member:Institute", lines[1])) {
    lines <- lines[-1]
}
lines <- lines[lines != ""]

# Parse "Last, First:Institute"
# Be careful with spacing
old_names <- character(0)
for (L in lines) {
    # Split at colon
    parts <- strsplit(L, ":")[[1]]
    # Name part is first
    raw_name <- trimws(parts[1])

    # "Last, First" -> "first last" for key
    name_parts <- strsplit(raw_name, ",")[[1]]
    if (length(name_parts) >= 2) {
        last <- trimws(name_parts[1])
        first <- trimws(name_parts[2])
        key <- paste(tolower(last), tolower(first))
        old_names <- c(old_names, key)
    }
}

cat(sprintf("Loaded %d old authors.\n", length(old_names)))

# --- 2. Read New Excel ---
excel_file <- "New list for faculty collaborations.xlsx"
if (!file.exists(excel_file)) stop("Excel missing")

# Read col 1 (Name) and col 4 (Inst)
# Col 1 name format: "LAST, FIRST MIDDLE"
# We'll rely on readxl to start at row 1 (header=FALSE to be safe or TRUE?)
# Let's try read_excel with default
raw_df <- read_excel(excel_file, col_names = FALSE)
# Extract name column (1)
names_col <- raw_df[[1]]
# Filter out NA
names_col <- names_col[!is.na(names_col)]

cat(sprintf("Loaded %d names from Excel.\n", length(names_col)))

new_authors_found <- data.frame(Name = character(), Institution = character(), stringsAsFactors = FALSE)

for (raw_name in names_col) {
    # Parse "LAST, FIRST MIDDLE"
    parts <- strsplit(raw_name, ",")[[1]]
    last <- trimws(parts[1])
    if (length(parts) > 1) {
        first_full <- trimws(parts[2])
        # Key uses first word of first name to match "First" in CSV?
        # Or full string? CSV has "First Name".
        # Let's clean it up: take the whole string as first name for key to match specific logic?
        # Actually, to match "key <- paste(tolower(last), tolower(first))"
        # we should use the same logic.
        key <- paste(tolower(last), tolower(first_full))
    } else {
        key <- tolower(last)
    }

    if (!(key %in% old_names)) {
        # It's new!
        inst_val <- "UA" # Defaulting for now as per file inspection
        new_authors_found <- rbind(new_authors_found, data.frame(Name = raw_name, Institution = inst_val))
    }
}

if (nrow(new_authors_found) > 0) {
    cat("\n=== NEW AUTHORS FOUND ===\n")
    print(new_authors_found)

    # Also write to CSV for user to see
    write_csv(new_authors_found, "new_authors_only.csv")
    cat("\nSaved new authors to 'new_authors_only.csv'.\n")
} else {
    cat("\nNo new authors found.\n")
}
