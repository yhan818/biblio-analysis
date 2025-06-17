import requests

# UArizona ID 
institution_id = "I138006243"

# Step 2: Get the number of documents indexed from the University of Arizona
works_url = f"https://api.openalex.org/works?filter=institutions.id:{institution_id}"
response = requests.get(works_url)

if response.status_code == 200:
    works_data = response.json()

    # Extract the count of documents
    document_count = works_data['meta']['count']
    print(f"Total number of documents from University of Arizona: {document_count}")
else:
    print(f"Failed to retrieve data: {response.status_code}")
    
    
    
search_url = "https://api.openalex.org/venues?search=University%20of%20Arizona%20repository"
response = requests.get(search_url)
venues_data = response.json()

# Print the found venues to help identify the correct one
for venue in venues_data['results']:
    print(f"Venue ID: {venue['id']}, Display Name: {venue['display_name']}")

