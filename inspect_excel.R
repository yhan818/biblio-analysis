library(readxl)
library(readr)

new_file <- "New list for faculty collaborations.xlsx"
print(paste("Reading:", new_file))
df_new <- read_excel(new_file)
print("Columns in new file:")
print(colnames(df_new))
print("First few rows:")
print(head(df_new))

old_file <- "ua_asu_unm_grant_authors2.csv"
print(paste("Reading:", old_file))
if (file.exists(old_file)) {
    df_old <- read_csv(old_file, show_col_types = FALSE)
    print("Columns in old file:")
    print(colnames(df_old))
} else {
    print(paste("File not found:", old_file))
}
