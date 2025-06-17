import csv
import os

# Function to convert a log file to CSV with only Name and ID
# sample lines from the log : 
# type 1: Xingnan Li https://openalex.org/A5060041080
# type 2: James Kuang-Jan Liao  
# type 2: No id

def log_to_csv(log_path):
    # Extract the directory and base name of the log file
    dir_name = os.path.dirname(log_path)
    base_name = os.path.basename(log_path)
    
    # Change the extension from .log to .csv
    csv_file_name = os.path.splitext(base_name)[0] + ".csv"
    
    # Construct the full path for the output CSV file
    csv_file_path = os.path.join(dir_name, csv_file_name)

    with open(log_path, 'r') as log_file, open(csv_file_path, 'w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        # Write CSV header for Name and ID
        csv_writer.writerow(['Name', 'ID'])
        
        for line in log_file:
            line = line.strip()
            # Check if the line contains "https://"
            if "https://" in line:
                # Split the line into name and ID URL based on "https://"
                name, id_url = line.rsplit("https://", 1)
                # Prepend "https://" back to the ID URL
                id_url = "https://" + id_url
            else:
                # If "https://" is not found, the line only contains the name
                name = line
                id_url = 'NA'
            
            # Write to CSV
            csv_writer.writerow([name.strip(), id_url])

    print("Conversion completed. Data was saved to:", csv_file_path)

log_file_path = input("Enter the path of the log file: ")

# Convert the log file to CSV
log_to_csv(log_file_path)

