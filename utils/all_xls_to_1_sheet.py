## This program combines all the individual Excels (each excel has one sheet) into one Excel  
##  
## Tested: each excel contains one sheet
## Author: Yan Han
## Date: 2024-02-14

import os
import pandas as pd

# Define directory and output file
directory = "./"
output_file = "all_works_in_one_sheet.xlsx"

# Create an empty DataFrame
combined_data = pd.DataFrame()

# Loop through files and read data
for filename in os.listdir(directory):
    if filename.endswith(".xlsx"):
    	file_path = os.path.join(directory, filename)
    	df = pd.read_excel(file_path)
    	combined_data = pd.concat([combined_data, df], ignore_index=True)

# Save the combined data
combined_data.to_excel(output_file, index=False)

print(f"All Excels have been combined into {output_file}")

