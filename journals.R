#### Journal analysis ##### 
# Author: Yan Han with assistance of ChatGPT 4o
# Updated: Oct 7, 2024

library(httr)
library(jsonlite)
library(openalexR)
library(dplyr)
library(tidyverse)
library(writexl)

options("max.print" = 100000)
options (openalexR.mailto="yhan@arizona.edu")
getwd()
setwd("/home/yhan/Documents/openalexR-test/")

### JPE https://openalex.org/S161564205
### JPE article example: https://openalex.org/W2772802389

# Define the journal ID
journal_id <- "S161564205"

# find the journal info
journal <- oa_fetch(
  entity = "source", 
  identifier = journal_id
)

view (journal)

test_article <- oa_fetch (
  entity = "works", 
  identifier = "https://openalex.org/W2772802389"
)

# JPE has 926 articles 
# Questions for openAlex: API shows 926 works (using best_oa_location.source.id), 38 duplicates. 
#           GUI says: 834 , Why? https://openalex.org/works?filter=primary_location.source.id%3As161564205
#

articles <- oa_fetch(
  entity = "works", 
  best_oa_location.source.id  = c(journal_id)
)


#### The following code:
### 1. To find any unique "id": openalex ID
### 2. To find any unique "so"
### 3. To find any duplicated using "title" 

# JPE: Find if "id" contain any duplicates: 923/926
articles_id_unique_values <- unique(tolower(articles$id))

# Find if "so" contains # Find the unique values in the $so column: 
articles_so_unique_values <- unique(tolower(articles$so))

# Find if multiple "issn_l" with the journal
articles_issn_unique_values <- unique(tolower(articles$issn_l))

##### JPE: 39 articles duplicated titles 
# Find if "title" contain any duplicates: 888 /926 
articles_title_unique_values <- unique(tolower(articles$title))

lower_titles <-tolower(articles$title)
all_duplicate_titles <- articles[lower_titles %in% lower_titles[duplicated(lower_titles)], ]
print(all_duplicate_titles)

#########################

# Sort the data frame by $cited_by_count in descending order
articles_cited_by_count <- articles[order(-articles$cited_by_count), ]

# View the sorted data
print(articles_cited_by_count)

write_xlsx(articles_cited_by_count, "citations/JPE_articles_cited_by_count.xlsx")


# Matching ISSN with publisher
# Define the ISSN you want to search for

data <- fromJSON(content(response, "text"))
publisher <- data$message$publisher

# Print the publisher
print(publisher)

# Define a list of ISSNs (replace with your actual list of 1000 ISSNs)
issn_list <- unique(publisher_NA$'issn_l')

# Initialize an empty data frame to store results
results <- data.frame(issn = character(), publisher = character(), stringsAsFactors = FALSE)

# Loop through each ISSN and make the API request
for (issn in issn_list[1:200]) {
  # Construct the CrossRef API URL for the ISSN
  url <- paste0("https://api.crossref.org/journals/", issn)
  response <- GET(url) # HTTP request
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Parse the response from JSON
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    if (!is.null(data$message$publisher)) {
      # Extract the publisher
      publisher <- data$message$publisher
    } else {
      # If no publisher found, set it to "Publisher not found"
      publisher <- "Publisher not found"
    }
  } else {
    # Handle failed request, store status code as publisher
    publisher <- paste("Failed - Status code:", status_code(response))
  }
  # Append the result to the data frame
  results <- rbind(results, data.frame(issn = issn, publisher = publisher, stringsAsFactors = FALSE))
}

print(results)
write.csv(results, "citations/issn_publishers.csv", row.names = FALSE)


