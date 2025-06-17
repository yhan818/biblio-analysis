#### This script is to combine all CSVs into one XLSX. One CSV per tab

import pandas as pd
import os

# Specify the directory containing the CSV files and the output file name
directory = './'
output_file = 'combined_csvs.xlsx'

# List to hold filenames
filenames = []

# Collect CSV filenames
for filename in os.listdir(directory):
    if filename.endswith('.csv') and not filename.startswith('~$'):
        filenames.append(filename)

# Sort the filenames alphabetically
sorted_filenames = sorted(filenames)

# Create a Pandas Excel writer using the openpyxl engine
with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
    # Iterate over each file in the sorted order
    for filename in sorted_filenames:
        file_path = os.path.join(directory, filename)
        # Read the CSV file into a DataFrame
        df = pd.read_csv(file_path, keep_default_na=False)
        
        # Use the filename without the .csv extension as the sheet name, truncated if necessary
        sheet_name = os.path.splitext(filename)[0][:31]  # Excel sheet names are limited to 31 characters
        
        # Write the DataFrame to a specific sheet in the Excel file
        df.to_excel(writer, sheet_name=sheet_name, index=False)

print(f"All CSV files have been combined into '{output_file}' with each in a separate sheet.")

