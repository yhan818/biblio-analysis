## This program merges an Excle (with multiple Tabs) into one Excel   
##  
## Tested: each excel contains one sheet
## Author: Yan Han
## Date: 2024-04-15
import os
import pandas as pd

def merge_excel_sheets(input_file, output_suffix="_combined.xlsx"):
    # Load the Excel file
    xls = pd.ExcelFile(input_file)
    
    # List to hold all dataframes
    df_list = []
    
    # Read each sheet into a list of dataframes
    for sheet_name in xls.sheet_names:
        df = xls.parse(sheet_name, na_values=[], keep_default_na=False)
        df['Sheet'] = sheet_name  # Optional: Keep track of data origin
        df_list.append(df)
    
    # Concatenate all dataframes into one
    combined_df = pd.concat(df_list, ignore_index=True)
    
    # the output file name based on the input file name
    base_name = os.path.splitext(os.path.basename(input_file))[0]
    output_file = base_name + output_suffix
    
    
    # Write the combined dataframe to a new Excel file
    combined_df.to_excel(output_file, index=False)
    print(f"Data combined into {output_file}")

# Usage
input_file = input("Enter the Excel file name: ")   
#output_file = 'one_big_combined.xlsx' 

merge_excel_sheets(input_file)

