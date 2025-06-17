import csv

# Function to convert LDIF to CSV
def ldif_to_csv(ldif_path, csv_path):
    with open(ldif_path, 'r') as ldif_file, open(csv_path, 'w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        # Write CSV header including the new 'cn' column
        csv_writer.writerow(['firstName', 'sn', 'cn'])
        
        current_givenName = ''
        current_sn = ''
        
        for line in ldif_file:
            # Check if we've reached a new entry or the end of the file
            if line.strip() == '' or line.startswith('dn:'):
                # Write the previous entry to the CSV if it had both givenName and sn
                if current_givenName and current_sn:
                    first_name = current_givenName.split()[0]  # Split and take the first name
                    cn = f"{first_name} {current_sn}"  # Combine firstName and sn to form cn
                    csv_writer.writerow([first_name, current_sn, cn])
                    # Reset variables for the next entry
                    current_givenName = ''
                    current_sn = ''
            
            # Extract givenName
            if line.startswith('givenName:'):
                current_givenName = line.split(':', 1)[1].strip()
            # Extract sn
            elif line.startswith('sn:'):
                current_sn = line.split(':', 1)[1].strip()
        
        # Check for and write the last entry in the file
        if current_givenName and current_sn:
            first_name = current_givenName.split()[0]  # Again, split and take the first name
            cn = f"{first_name} {current_sn}"  # Combine firstName and sn to form cn
            csv_writer.writerow([first_name, current_sn, cn])

dept_code = input("Enter the department code (e.g., 'dept0713'): ")
ldif_path = f"{dept_code}.ldif"
csv_path = f"{dept_code}.csv"

ldif_to_csv(ldif_path, csv_path)

print("Conversion completed. Data saved to:", csv_path)

