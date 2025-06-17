## De-duplication by title (which is "display_name" in openAlex's data
## Certain different works may have the same title. 

import pandas as pd
import sys

def de_duplicates(file_path):
    try:
        # Step 1: Load the Excel file
        df = pd.read_excel(file_path)

        # Step 2: Identify duplicates by 'display_name'
        df['display_name_lower'] = df['display_name'].str.lower()
        
        duplicates = df[df.duplicated('display_name_lower', keep=False)]


        # Step 3: Display duplicates and output to a log
        log_path = file_path.replace('.xlsx', '_duplicates_title.log')
        with open(log_path, 'w') as log_file:
            for display_name in duplicates['display_name_lower'].unique():
                log_file.write(f"{display_name}\n")



        print(f"Duplicate rows logged in '{log_path}'.")
        print("Duplicate Rows based on 'display_name' column (case-insensitive):")
        print(duplicates[['display_name', 'display_name_lower']])
        
	# Step 4: Remove duplicates, keeping the first occurrence based on 'display_name_lower'
        cleaned_df = df.drop_duplicates('display_name_lower', keep='first').drop(columns=['display_name_lower'])

        # Step 5: Write to a new Excel file
        output_path = file_path.replace('.xlsx', '_cleaned_title.xlsx')
        cleaned_df.to_excel(output_path, index=False)

        print(f"Cleaned Excel file saved as '{output_path}'.")
    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py <path_to_your_excel_file.xlsx>")
    else:
        file_path = sys.argv[1]
        de_duplicates(file_path)

