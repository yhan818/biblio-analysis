## De-duplication by "ID"


import pandas as pd
import sys

def de_duplicates(file_path):
    try:
        # Step 1: Load the Excel file
        df = pd.read_excel(file_path)

        # Step 2: Identify duplicates by 'id'. 
        # Note: OpenAlex has same work, different id! 
        duplicates = df[df.duplicated('id', keep=False)]
        
        # or Identify duplicated by 'display_name'. This shall get highest precision
        # duplicates = df[df.duplicated('display_name', keep=False)]

        # Step 3: Display duplicates and output to a log
        log_path = file_path.replace('.xlsx', '_duplicates_id.log')
        with open(log_path, 'w') as log_file:
        	for id_value in duplicates['id'].unique():
        		log_file.write(f"{id_value}\n")

        print(f"Duplicate rows logged in '{log_path}'.")
        print("Duplicate Rows based on 'id' column:")
        print(duplicates)

        # Step 4: Remove duplicates, keeping the first occurrence
        cleaned_df = df.drop_duplicates('id', keep='first')

        # Step 5: Write to a new Excel file
        output_path = file_path.replace('.xlsx', '_cleaned_id.xlsx')
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

