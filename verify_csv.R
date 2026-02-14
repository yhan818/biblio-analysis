library(readr)
library(dplyr)

csv_file <- "ua_asu_unm_grant_authors2.csv"
if (file.exists(csv_file)) {
    df <- read_csv(csv_file, show_col_types = FALSE)
    print("Columns found:")
    print(colnames(df))
    print(sprintf("Total rows: %d", nrow(df)))
    print(head(df))
} else {
    stop("File not found")
}
