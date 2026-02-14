import pandas as pd
import sys

import os

# Use absolute paths to avoid CWD ambiguity
base_dir = "/home/yhan/Documents/biblio-analysis"
csv1 = os.path.join(base_dir, "ua_asu_unm_grant_authors.csv")
csv2 = os.path.join(base_dir, "ua_asu_unm_grant_authors2.csv")

import difflib

def normalize_inst(inst):
    inst = str(inst).strip()
    if "Arizona" in inst and "State" not in inst: return "UA"
    if "Arizona State" in inst: return "ASU"
    if "New Mexico" in inst or "UNM" in inst: return "UNM"
    if inst == "nan": return "UA"
    return inst

def is_same_person(last1, first1, inst1, last2, first2, inst2):
    # Normalize
    l1, f1 = last1.lower(), first1.lower()
    l2, f2 = last2.lower(), first2.lower()
    i1, i2 = normalize_inst(inst1), normalize_inst(inst2)
    
    # Strict Institution Check? User examples suggest same institution.
    if i1 != i2 and i1 != "nan" and i2 != "nan":
        return False

    # Last Name Check
    if l1 == l2:
        last_match = True
    else:
        # Fuzzy Last Name (Moffet vs Moffett)
        ratio = difflib.SequenceMatcher(None, l1, l2).ratio()
        last_match = ratio > 0.9 or (l1 in l2 and len(l1)>3) or (l2 in l1 and len(l2)>3)
    
    if not last_match:
        return False
        
    # First Name Check
    if f1 == f2: 
        return True
    
    # Substring / Nickname check (Vin vs Vincent, E. Fiona vs Fiona)
    if f1 in f2 or f2 in f1:
        return True
        
    # Similarity (Julie vs Julia, Jordon vs Jordan)
    ratio = difflib.SequenceMatcher(None, f1, f2).ratio()
    if ratio > 0.8:
        return True
    
    # Initials check (Clark, Ross vs Clark, f?) - User prompted this.
    # If one is just 1 letter and matches start of other
    if len(f1) == 1 and f2.startswith(f1): return True
    if len(f2) == 1 and f1.startswith(f2): return True
    
    return False

# Load ALL old authors into a list of dicts
old_authors = []
try:
    df1 = pd.read_csv(csv1)
    df1.columns = [c.strip() for c in df1.columns]
    if 'First Name' in df1.columns: df1.rename(columns={'First Name': 'First'}, inplace=True)
    if 'Last Name' in df1.columns: df1.rename(columns={'Last Name': 'Last'}, inplace=True)
    
    for idx, row in df1.iterrows():
        if 'Last' in row:
            old_authors.append({
                'Last': str(row['Last']).strip(),
                'First': str(row.get('First', '')).strip(),
                'Institution': str(row.get('Institution', '')).strip()
            })
except Exception as e:
    print(f"Error reading old CSV: {e}")

# Load New CSV
try:
    df2 = pd.read_csv(csv2)
    df2.columns = [c.strip() for c in df2.columns]
    if 'First Name' in df2.columns: df2.rename(columns={'First Name': 'First'}, inplace=True)
    if 'Last Name' in df2.columns: df2.rename(columns={'Last Name': 'Last'}, inplace=True)
except Exception as e:
    print(f"Error reading new CSV: {e}")
    sys.exit(1)

new_unique_authors = []
matched_log = []

for idx, row in df2.iterrows():
    l2 = str(row['Last']).strip()
    f2 = str(row['First']).strip()
    i2 = str(row.get('Institution', '')).strip()
    
    is_new = True
    for old in old_authors:
        if is_same_person(old['Last'], old['First'], old['Institution'], l2, f2, i2):
            is_new = False
            matched_log.append(f"MATCH: {l2}, {f2} ({i2})  ==  {old['Last']}, {old['First']} ({old['Institution']})")
            break
    
    if is_new:
        # Also check against already accepted unique new authors to avoid dupes in result
        is_dupe_in_new = False
        for new in new_unique_authors:
             if is_same_person(new['Lastname'], new['Firstname'], new['Institution'], l2, f2, i2):
                 is_dupe_in_new = True
                 break
        if not is_dupe_in_new:
            new_unique_authors.append({
                'Lastname': l2,
                'Firstname': f2, # Keep original formatting
                'Institution': normalize_inst(i2)
            })

print(f"\nAnalyzed {len(df2)} authors from CSV2 against {len(old_authors)} authors from CSV1.")
print(f"Identified {len(matched_log)} matches (excluded).")
print(f"Found {len(new_unique_authors)} truly new authors.")

print("\n--- Matches Log (Examples) ---")
for log in matched_log[:10]:
    print(log)

if new_unique_authors:
    output_df = pd.DataFrame(new_unique_authors)
    output_csv = "new_authors_in_2_not_1.csv"
    output_df.to_csv(output_csv, index=False)
    print(f"\nSuccessfully saved to '{output_csv}'")
    
    print("\n--- New Authors List ---")
    for idx, row in output_df.iterrows():
        print(f"{row['Lastname']}, {row['Firstname']}, {row['Institution']}")

# Clear exits
sys.exit(0)
