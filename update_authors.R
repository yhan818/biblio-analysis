# update_authors.R
# This script standardizes the author list and merges new authors from the Excel file.

library(readr)
library(readxl)
library(dplyr)
library(stringr)

# --- 1. Read and Parse the Old CSV (ua_asu_unm_grant_authors2.csv) ---
csv_file <- "ua_asu_unm_grant_authors2.csv"

# Function to parse "Last, First:Institution" format
parse_old_csv <- function(file_path) {
    if (!file.exists(file_path)) {
        stop("CSV file not found: ", file_path)
    }

    # Read as lines to handle the custom format
    lines <- read_lines(file_path)

    # Remove header if it exists and looks like "Center Member:Institute"
    if (length(lines) > 0 && grepl("Center Member:Institute", lines[1])) {
        lines <- lines[-1]
    }

    parsed_data <- data.frame(
        `First Name` = character(),
        `Last Name` = character(),
        Institution = character(),
        stringsAsFactors = FALSE,
        check.names = FALSE
    )

    for (line in lines) {
        if (trimws(line) == "") next

        parts <- str_split(line, ":", simplify = TRUE)
        if (ncol(parts) < 2) {
            warning("Skipping malformed line: ", line)
            next
        }

        full_name_part <- trimws(parts[1, 1])
        institution <- trimws(parts[1, 2])

        name_parts <- str_split(full_name_part, ",", simplify = TRUE)
        if (ncol(name_parts) < 2) {
            # Fallback if no comma: assume "First Last" or handle as is?
            # Based on file inspection, it is "Last, First"
            # If only one part, treat as Last Name? Or skip?
            # Let's try to be robust.
            last_name <- trimws(name_parts[1, 1])
            first_name <- ""
        } else {
            last_name <- trimws(name_parts[1, 1])
            first_name <- trimws(name_parts[1, 2])
        }

        parsed_data <- bind_rows(parsed_data, data.frame(
            `First Name` = first_name,
            `Last Name` = last_name,
            Institution = institution,
            check.names = FALSE
        ))
    }
    return(parsed_data)
}

message("Reading existing CSV...")
df_old <- parse_old_csv(csv_file)
message(sprintf("Found %d authors in old CSV.", nrow(df_old)))

# --- 2. Read and Parse the New Excel File ---
excel_file <- "New list for faculty collaborations.xlsx"
message("Reading new Excel file...")

# Locate the correct sheet/data. Based on inspection, it starts at row 1.
# Columns were: [Name], ...2, ...3, [Institution (UA)]
# We need column 1 (Name) and column 4 (Institution)
# Name format in Excel: "LAST, FIRST MIDDLE" (e.g., RODGERS, KATHLEEN E.)

df_new_raw <- read_excel(excel_file, col_names = FALSE)

# Filter rows that look like data (e.g., have a name in col 1)
# Inspecting the previous `head` output, it seems row 1 is data too?
#   `STERN, JENNIFER HELENE` ...2 ...3 UA
# Wait, `read_excel` used first row as header in the inspection.
# Let's re-read with col_names = FALSE to be safe and process all rows.

# Extract Name and Institution
df_new_parsed <- df_new_raw %>%
    select(1, 4) %>%
    rename(RawName = 1, RawInst = 2) %>%
    filter(!is.na(RawName)) %>%
    # Remove header row if it exists (unlikely given the file name content seen)
    # But if "STERN, JENNIFER HELENE" was the header, it is a person.
    # So assume all rows are people.
    mutate(
        RawName = trimws(RawName),
        RawInst = trimws(RawInst)
    )

parse_new_names <- function(raw_name) {
    # Format: LAST, FIRST MIDDLE
    parts <- str_split(raw_name, ",", simplify = TRUE)
    last_name <- trimws(parts[1, 1])

    if (ncol(parts) > 1) {
        first_part <- trimws(parts[1, 2])
        # Take first word as First Name? Or keep full string?
        # Original CSV had "First Name" and "Last Name".
        # Let's keep the full string after comma as "First Name" (including middle)
        first_name <- first_part
    } else {
        first_name <- ""
    }
    return(c(first_name, last_name))
}

# Apply parsing
parsed_names <- t(sapply(df_new_parsed$RawName, parse_new_names))
df_new <- data.frame(
    `First Name` = parsed_names[, 1],
    `Last Name` = parsed_names[, 2],
    Institution = df_new_parsed$RawInst,
    check.names = FALSE
)

# Clean Institution names if needed
# The excel file showed "UA" for all in the sample.
# Ensure mapping is consistent (UA, ASU, UNM)
df_new$Institution <- ifelse(is.na(df_new$Institution), "UA", df_new$Institution) # Default or check?
# NOTE: The sample showed "UA". I will assume column 4 is valid.

message(sprintf("Found %d authors in new Excel file.", nrow(df_new)))


# --- 3. Merge and Identify New Authors ---

# Create a unique key for comparison (Last + First lowercase)
create_key <- function(first, last) {
    paste(tolower(trimws(last)), tolower(trimws(first)))
}

df_old <- df_old %>% mutate(key = create_key(`First Name`, `Last Name`))
df_new <- df_new %>% mutate(key = create_key(`First Name`, `Last Name`))

# Find new authors
new_authors <- df_new %>%
    filter(!key %in% df_old$key)

message("\n--------------------------------------------------------")
if (nrow(new_authors) > 0) {
    message(sprintf("Found %d NEW authors:", nrow(new_authors)))
    print(new_authors %>% select(`First Name`, `Last Name`, Institution))
} else {
    message("No new authors found.")
}
message("--------------------------------------------------------\n")

# Combine
combined_df <- bind_rows(df_old, new_authors) %>%
    select(`First Name`, `Last Name`, Institution) %>%
    distinct() %>% # Safety check
    arrange(`Last Name`, `First Name`)

# --- 4. Write Result ---
output_file <- "ua_asu_unm_grant_authors2.csv"
write_csv(combined_df, output_file)
message(sprintf("Successfully updated '%s' with %d total authors.", output_file, nrow(combined_df)))
