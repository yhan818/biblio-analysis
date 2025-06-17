## Combine all the Excel files (each Excel has one sheet) into one Excel (whose each tab is the individual Excel File)
## Tested: each excel contains one sheet for Banner. 160 sheets in one Excel
## Author: Yan Han
## Date: 2024-04-15

import pandas as pd
import os
from openpyxl import load_workbook
import zipfile

# Specify the directory containing the Excel files and the output file name
directory = './'
output_file = 'all_works_in_individual_sheet.xlsx'

# Dictionary to hold filename and its corresponding sheet name (truncated if necessary)
sheet_names = {}

# Collect and prepare sheet names
for filename in os.listdir(directory):
    if filename.endswith('.xlsx') and not filename.startswith('~$'):
        # Get the filename without the .xlsx extension to use as the sheet name
        sheet_name = os.path.splitext(filename)[0][:31]  # Truncate to 31 chars to avoid warning msg
        sheet_names[filename] = sheet_name

# Sort the sheet names alphabetically based on sheet_name values
sorted_filenames = sorted(sheet_names, key=lambda x: sheet_names[x])

# Create a Pandas Excel writer using the openpyxl engine
with pd.ExcelWriter(output_file, engine='openpyxl') as writer:
    # Iterate over each file in the sorted order
    for filename in sorted_filenames:
        file_path = os.path.join(directory, filename)
        try:
            # Attempt to read the Excel file
            df = pd.read_excel(file_path, engine='openpyxl')
            # Write the DataFrame to a specific sheet, using the pre-prepared and sorted sheet name
            df.to_excel(writer, sheet_name=sheet_names[filename], index=False)
        except zipfile.BadZipFile:
            print(f"Skipping file (not a valid .xlsx file): {filename}")
        except Exception as e:
            print(f"An error occurred with file {filename}: {e}")

print(f"All Excel files have been combined into '{output_file}' with separate sheets.")

