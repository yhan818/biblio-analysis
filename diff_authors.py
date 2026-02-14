import pandas as pd
import sys

# 1. Read Old CSV (ua_asu_unm_grant_authors2.csv)
csv_file = "ua_asu_unm_grant_authors2.csv"
old_names = set()

try:
    with open(csv_file, 'r', encoding='utf-8-sig', errors='replace') as f:
        lines = f.readlines()
        
    print(f"Debug: Read {len(lines)} lines from CSV.")
    if len(lines) > 0: print(f"Debug first line: {lines[0].strip()}")
        
    for line in lines:
        line = line.strip()
        if not line or "Center Member:Institute" in line:
            continue
        
        parts = line.split(':')
        if len(parts) >= 2:
            name_part = parts[0].strip()
            # "Last, First"
            name_tokens = name_part.split(',')
            if len(name_tokens) >= 2:
                last = name_tokens[0].strip().lower()
                first = name_tokens[1].strip().lower()
                old_names.add(f"{last} {first}")
            else:
                 # Fallback: Treat as just last name or skip?
                 old_names.add(name_part.lower())
except FileNotFoundError:
    print(f"Error: {csv_file} not found.")

print(f"Loaded {len(old_names)} old authors.")

# 2. Read New Excel (New list for faculty collaborations.xlsx)
excel_file = "New list for faculty collaborations.xlsx"
try:
    df = pd.read_excel(excel_file, header=None)
    # Assuming Column 0 is Name, Column 3 is Institution (based on previous inspection which showed Col 1 and 4 in R 1-based index, so 0 and 3 in Python 0-based)
    # Sample: "STERN, JENNIFER HELENE", ..., ..., "UA"
    
    new_authors = []
    seen_new = set()

    for index, row in df.iterrows():
        raw_name = str(row[0]).strip()
        if raw_name == "nan": continue
        
        # Parse "LAST, FIRST MIDDLE"
        parts = raw_name.split(',')
        last = parts[0].strip()
        first_full = ""
        if len(parts) > 1:
            first_full = parts[1].strip()
        
        # Create key to check against old_names
        key = f"{last.lower()} {first_full.lower()}"
        
        # Check against simple first name too match "First" in CSV
        first_simple = first_full.split(' ')[0] if first_full else ""
        key_simple = f"{last.lower()} {first_simple.lower()}"
        
        if key not in old_names and key_simple not in old_names:
             # Deduplicate within new list
             if key not in seen_new:
                 inst = str(row[3]).strip() if len(row) > 3 else "UA"
                 # Check if Inst is nan
                 if inst == "nan": inst = "UA"
                 
                 new_authors.append((raw_name, inst))
                 seen_new.add(key)

    print(f"\nFound {len(new_authors)} NEW authors from Excel.")
    
    if new_authors:
        print("\n--- New Authors List ---")
        for name, inst in new_authors:
            print(f"{name} : {inst}")
            
except ImportError:
    print("Error: pandas or openpyxl not installed.")
except Exception as e:
    print(f"Error reading Excel: {e}")

