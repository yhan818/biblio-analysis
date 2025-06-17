import pandas as pd
import csv
import ldif

def parse_ldif_to_csv(ldif_file_path, csv_file_path):
    with open(ldif_file_path, 'r') as ldif_file, open(csv_file_path, 'w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(['surname', 'cn']) 

        current_entry = {'cn': '', 'sn': ''}
        for line in ldif_file:
            line = line.strip()
            if line.startswith('dn:'):
                # If we're at a new entry and the current entry has data, write it to the CSV
                if current_entry['sn'] or current_entry['cn']:
                    csv_writer.writerow([current_entry['sn'], current_entry['cn']])
                    current_entry = {'cn': '', 'sn': ''}

                # Extract the cn value from the dn line
                cn_part = line.split('+')[0]  # Get the part before the '+' sign
                current_entry['cn'] = cn_part.split('=')[1].split(',')[0].strip()

            elif line.startswith('sn:'):
                # Extract the sn value
                current_entry['sn'] = line.split(':')[1].strip()

        # Write the last entry if it exists
        if current_entry['sn'] or current_entry['cn']:
            csv_writer.writerow([current_entry['sn'], current_entry['cn']])

# Merges two CSV files on a common column.
def merge_csv_files(csv_file_path1, csv_file_path2, common_column, output_file_path ):

    # Reading the CSV files into DataFrames
    df1 = pd.read_csv(csv_file_path1)
    df2 = pd.read_csv(csv_file_path2)

    # Merging the DataFrames on the specified common column
    merged_df = pd.merge(df1, df2, on=common_column, how='inner')
	
    merged_df.to_csv(output_file_path, index=False)



# Ask the user for the department code
dept_code = input("Enter the department code (e.g., 'dept0713'): ")

# Construct file paths based on the user input
input_csv_path = f"{dept_code}_LDAP.csv"
output_csv_path = f"{dept_code}_common.csv"


# Specify the LDIF file path and the CSV file path
ldif_file_path  = f"{dept_code}.ldif"
csv_file_path   = f"{dept_code}.csv"
common_file_path= f"{dept_code}_common.csv"

# Parse LDIF file to CSV
parse_ldif_to_csv(ldif_file_path, csv_file_path)


# Find common entries 
merged_df = merge_csv_files( 'Funk_faculty_list_UA.csv', csv_file_path, 'surname' , common_file_path)

