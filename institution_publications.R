############# Institution authors publication analysis and Collection Management ##########
######## Author: Yan Han with help of ChatGPT 4
######## Updated: Dec 14, 2024
######## Updated: Fixed NA issue with host_organization
##### Search an institution authors' publication using openAlex data ####
# OpenAlex R Documentation: https://github.com/ropensci/openalexR

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('data.table')
install.packages("openalexR")
install.packages("remotes")
install.packages("here")
# remotes::install_github("ropensci/openalexR", force=TRUE) 

library(openalexR)
packageVersion("openalexR")

library(here)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(data.table)
library(openxlsx)
library(readxl)
library(writexl)

source("my_functions.R")

# free unused obj to manage memory
rm(list=ls())
gc()

options("max.print" = 100000)

options (openalexR.mailto="yhan@arizona.edu")
getwd()
print(here())

##### General comments:
### OpenAlex data structure has been gone through sevearl changes from 2023- 2025. Therefore,
### if certain code not running/crash, need to check the error log carefully and verify using the retro datasets deposited in ReDATA

#######################################################################################
# SECTION 1: Works published
######################################################################################

##### 1. Getting data. (retrieved 2024-09-02)
# Retrieving all publications association with UArizona's ROR (Research Organization Registry) ID.
# UA works_published per year is ~9,000. For running 2 years data, need better computer or crashed R studio.
# Year 2023: 10,561 (2025-02) <<< 10,559 (2025-01) <<< 9,384 (2024-10)
# Year 2022: 8,825 (2025-02-24) <<< 8,833 (2024-10-18) <<< 8,674 (2024-09-09)
# Year 2021: 9,336 (7,048 type-journal articles and reviews)
# Year 2020: 
# Year 2019: 8,847 
# 2023-current: 14,660 works : 5 min to get UAworks with 3 GB mem, 264 mins to pull 372,000 reference's data with 8.6 GB  
# 2022-current: 23,360 works: 10 mins to get UAWorks with 6 GB RAM, 450 mins to pull 560,000 citedWorks's data with 12 GB. crashed R studio.
# 2020-current: 
# 2014-current: 86,000 works : 15 mins to run, and used 7GB RAM. 
# 2013-current: 50,000 records: 

### 1.1 Getting the count only. This is the quick way to find out the total number of works. 
### There are two types we are interested: article and non-article. see OpenAlex doc for more details
####### a) Any type (broader): journals, repositories (PubMed, arXiv etc). 
####### b) limited to  type = "article" to limit article only.
### 2025-06-16: web interface: copy/paste the following URL
# https://openalex.org/works?page=1&filter=authorships.institutions.lineage:i138006243,publication_year:2020,type:types/article&view=list,report,api
### 2020: same result; # articles : 7689


# OpenAlex internal id or ROR ID:
# Note: When we query OpenAlex, we use the ROR ID (not the institution ID) to retrieve an institution’s data. This is because the ROR ID is the stable, 
# universal standard for institutional identification, ensuring our findings are both reproducible and interoperable with the wider scholarly data ecosystem and stability in a long term


# Typically only some seconds
works_count <-oa_fetch(
  entity="works",
  # institutions.id = "i138006243", # University of Arizona openAlex institution id
  
  #institutions.ror=c("03efmqc40"), # ASU
  #institutions.ror=c("05hs6h993"), # MSU 
  institutions.ror=c("03m2x1q45"), # University of Arizona
  #institutions.ror=c("00cvxb145"), # University of Washington
  
  from_publication_date ="2024-01-01",
  to_publication_date = "2024-12-31",
  type = "article",  # comment out this line to include other types 
  count_only = TRUE
)

### 1.2 Getting all the works based on the institution ROR and publication date. It takes longer time. 

works_published_2021 <-oa_fetch(
  entity="works",
  
  # institutions.ror=c("03efmqc40"),  # ASU
   institutions.ror=c("03m2x1q45"), # UArizona
  
  #institutions.ror=c("00cvxb145"), # University of Washington
  type = "article", 
  from_publication_date ="2021-01-01",
  to_publication_date = "2021-12-31",
  
)

# SHALL get all works, then filter them if needed. 
# 2023: All works: 9,384 without type =journal (2024-09) 
# 2023: All works: 10,559 (2025-01) 
# 2023: journal only: 6,903 using primary_location.source.type = "journal" as a filter (not including type="repository")
# 
works_published_2023 <-oa_fetch(
  entity="works",
  
  # institutions.ror=c("03efmqc40"),  # ASU
  # institutions.ror=c("03m2x1q45"), # UArizona
  
  #institutions.ror=c("00cvxb145"), # University of Washington
  
  from_publication_date ="2023-01-01",
  to_publication_date = "2023-12-31",
)

works_published_2022 <-oa_fetch(
  entity="works",
  # institutions.ror=c("03efmqc40"),  # ASU
  institutions.ror=c("05hs6h993"), # MSU 
  
  # institutions.ror=c("03m2x1q45"), # U Arizona
  # institutions.ror=c("00cvxb145"), # University of Washington
  from_publication_date ="2022-01-01",
  to_publication_date = "2022-12-31",
)

# Save data
# saveRDS(works_published_2019, "../works_published_2019.rds")
# saveRDS(works_published_2020, "../works_published_2020.rds")
saveRDS(works_published_2021, "../works_published_2021.rds")
saveRDS(works_published_2022, "../msu_works_published_2022.rds")
saveRDS(works_published_2023, "../msu_works_published_2023.rds")
saveRDS(works_published_2024, "../msu_works_published_2024.rds")

# Load data 
works_published_2019 <- readRDS("../works_published_2019.rds")
works_published <- works_published_2019

works_published_2020 <- readRDS("../works_published_2020.rds")
works_published <- works_published_2020

works_published_2021 <- readRDS("../works_published_2021.rds")
#works_published_2021_journal <- readRDS("../works_published_journal_2021.rds")
works_published <- works_published_2021

works_published_2022 <- readRDS("../uw_works_published_2022.rds")
works_published <- works_published_2022


works_published_2023 <- readRDS("../uw_works_published_2023.rds")
# to filter "journal" works only. I feel it shall not be this restrict. (other works like grey literature are good too)
works_published <- works_published_2023

works_published_2024 <- readRDS("../uw_works_published_2024.rds")
works_published <- works_published_2024

####################################################
##### 2. Checking and verifying data
##### 2.1 Route 1: Getting citation data from $referenced_works
##### Route 2: Getting author's data? 
###### change this line only to update the right dataset.
works_published_ref <- works_published$referenced_works
#########################

# Find "NA" indexes: 18- 25% no references 
# Questions for openAlex: 
# 1. Is this normal? any plan to improve? 
# 2. I checked ~3500 records (1% ), Field "issn_l" has values, but "host_organization" field has no values. 
# 3. 
# "type" is "source.type" ??? 
# Year 2019: 1575 / 8848 referenced works value="NA", while $type is "article". 18%
# Year 2020: 1868 / 10161 referenced works value="NA", while $type is "article". 
# Year 2021: 1921 / 9336 referenced works value="NA", while $type is "article". 
# Year 2022: 1224 / 8674  referenced works value="NA", while $type is "article". 
# Year 2023: 1534 / 9384 referenced works value="NA", while $type is "article". 
# 2023: 1217 / 6889 published article, primary_location_type = journal, $type = article: 17%

# There are NA references. So we need to remove them. 
# No references: 20% (2022), 14% (2021)
# This na_indices include type: article, books, errata, letter, and other types
na_indices <- which(sapply(works_published_ref, function(x) is.logical(x) && is.na(x))) 
na_count <- sum(sapply(works_published_ref, function(x) is.logical(x) && is.na(x)))
na_percent <- na_count/length(works_published_ref) * 100

# Remove duplicate rows from the data frame
unique_works_published <- unique(works_published)
works_published_ref <- unique(works_published_ref) # this actually also remove NA lists.

# Filter the rows where $reference_works is NA and $type is "article"
works_na_referenced_works <- works_published %>%
  filter(is.na(referenced_works) & type == "article")

#write_xlsx(works_na_referenced_works, "citations/works_journal_2023_na_referenced_works.xlsx") # send this to OpenAlex

### 2.2 Combine all the references and do further data analysis
# Avg # of references per article: ~50
# Year 2023 total references: 364,304: total journal article: 308,359:  unique 281,470 / 351,479: more cited: ~77,000 
# Year 2022 total references: 356,718: 

# Year 2021 total references: 382,965: 
# Year 2020 total references: 392,992: article 
# Year 2019 total references: 352,509: articles 329,000  

# rm(works_published_ref_combined)
works_published_ref_combined <- unlist(works_published_ref, use.names = FALSE)
works_published_ref_combined <- works_published_ref_combined[!is.na(works_published_ref_combined)]  # Remove NA values

### 2.21 finding these duplicates, which mean the duplicates have been cited multiple times 
# (probably more important to have these journals subscribed!)
# cited more: ~20% - 25%  (2019, 2020, 2021, 2022, 2023 UArizona data)
works_published_ref_more_cited <- works_published_ref_combined[duplicated(works_published_ref_combined)]
works_published_ref_unique <- works_published_ref_combined[!duplicated(works_published_ref_combined)]

### Method 2: there are different
citation_counts <- table(works_published_ref_combined)
head(citation_counts)

############################################################
### 2.23 For Testing purpose: Trace back from the cited article -> $referenced_works -> original published article
# Find the index of multiple samples
head(works_published$referenced_works)
head(works_published_ref_unique)

# Use sapply to find matching elements in the works_published_ref for testing. 
matching_indices <- which(sapply(works_published_ref_combined, function(x) 
  any(x %in% c("https://openalex.org/W4210835162", "https://openalex.org/W2944198613")))) # https://openalex.org/W1624352668 were cited on 2021 and 2023 data
print(matching_indices)

# We can see the original works for samples
works_published[2, "id"]
works_published[174, "id"]

# Test to see how many times a work is cited. 
# 21 times (2020); 22 times(2021), 26 times(2022), 18 times(2023)
# https://openalex.org/W4247665917 were cited in 2019, 2021, 2022 and 2023 data
index <- which(works_published_ref_combined == "https://openalex.org/W4247665917")
print(index)

###########################################################

##### 3. From authors' DF. 
# Flattening authors fields from the DF (multiple authors per work). 
# 426,000 obs (multiple authors) from 50,400 obs (works)
works_published_since <- works_published

#### Year 2022: 
# -- works_published_authors: 75,222
# -- works_published_UAauthors: 16,432
# -- works_published_ua_authors_ref_combined: 656,712
# -- works_published_ua_authors_ref_cited: 249,629
works_published_authors<-works_published_since%>%
  mutate(author=lapply(author, function(x){
    names(x) <-paste0(names(x), "author")
    return(x)
  }))%>%
  unnest(author)

rm(list = c("works_published_authors", "na_percent" ))

UAauthors <-unique(works_published_authors)
#write_xlsx(UAauthors2, "UAauthors.xlsx")

# After flattening, authors' fields (e.g. au_idauthor, institution_rorauthor) are displayed
colnames(works_published)
colnames(works_published_authors)

#################### 3.3 TESTING!!!#################

# Then extract UArizona authors only
# 94,500 obs from 426,000 obs (UA authors only).  
## https://openalex.org/A5033317672 Saurav Mallik (is at two affiliations for https://api.openalex.org/works/W4389611927. Harvard and University of Arizona)
### https://openalex.org/W4401226694 author Renu Malhotra has two affiliations. 
oa_fetch_test1 <-oa_fetch( entity="works",  id="https://openalex.org/W4401226694")
oa_fetch_test1$author
view(oa_fetch_test1[[4]][[1]])

oa_fetch_test2 <-oa_fetch( entity="authors",  id="https://openalex.org/A5003933592")

#### This is not 100% accurate because UArizona has child organization whose ROR is associated with an article. By filtering institution_rorauthor
# to UArizona's ROR, certain articles are left out!!! 
# 2024-09: I am currently working with openAlexR developers to fix this. 
works_published_authors_ua <- works_published_authors%>%filter(institution_rorauthor== "https://ror.org/03m2x1q45")
works_published_authors_ua_unique <- unique (works_published_authors_ua)
duplicates <- works_published_authors_ua[duplicated(works_published_authors_ua), ]

# 3.32 
### Note: one article can be authored by multiple UA authors. However, the references cited are the same. 
### This data can study UA internal collaboration! 

### 3.33 Testing if a cited work is found. 
# Deep Learning, Nature, by Yann LeCun, Yoshua Bengio, Geoffrey Hinton. Cited by: 62,210
search_string <- "https://openalex.org/W2919115771"
result <- lapply(works_published_ref_combined, function(x) grep(search_string, x, value = TRUE))

matches <- result[sapply(result, length) > 0]
indices <- which(sapply(works_published_ref_combined, function(x) any(grepl(search_string, x))))
for (i in indices) {
  cat("Index:", i, "\n")
  cat("Element:\n", works_published_ref_combined[[i]], "\n\n")
}

#### Find it from works_published (UA author works_cited the work (search_string))
# Find it from the original article
search_string <- "https://openalex.org/W2594545996"  
# this article was cited 81 (2019, 130 (2020), 90 (2021), 52 (2022), 16 (2023)
indices_with_string <- which(sapply(works_published$referenced_works, function(x) search_string %in% x))
print(indices_with_string)
works_published[indices_with_string, ]$id

# test case 2: cited 6 from microbiology, multiple times for 2019, 2020, 2021, 2022
# both final published version and pre-print existing: https://openalex.org/works/W4379795917 and https://openalex.org/W4319339791 
search_string <- "https://openalex.org/W2153919737"
indices_with_string <- which(sapply(works_published$referenced_works, function(x) search_string %in% x))
print(indices_with_string)
works_published[indices_with_string, ]$id

# https://openalex.org/W4210835162


##### 3.34  Fetch time 
# the number of works to fetch at a time has little influence the time to run oa_fetch
# 2024-09: fetch_number = 1,000, reduced the total running time of 10% comparing to fetch_number 100
# 2024-09: fetching 241,000 works took 188 minutes
# optimize code: ... <to do> 

#Creating an empty dataframe to store the results of the for loop.
works_cited <-data.frame()

# Getting these works' metadata. This takes long time to run. 
# Warnings(). a work > 100 authors will be truncated 
# 2024: 
# 2023: 352,509 (checked) out of 364,304 : article  / 308,359
# 2022: 345,813 (checked) : article / 325,520 (type = journal)
# 2021: 384,886 (checked) out of 384,886
# 2019: 331,657 (checked).
########################################
### Testing optimization of rbind and oa_fetch
### 2024-09-21: 10,000 works in R old version (4.1.2): 270 seconds
### 2024-09-23: 10,000 works in R latest version (4.4.1): 112 seconds
install.packages("profvis")
library(profvis)

malaria_topic <- oa_fetch(entity = "topics", search = "malaria") %>% 
  filter(display_name == "Malaria") %>% 
  pull(id)
malaria_topic
#> [1] "https://openalex.org/T10091"
system.time({
  res <- oa_fetch(
    topics.id = malaria_topic,
    entity = "works",
    verbose = TRUE,
    options = list(sample = 10000, seed = 1),
    output = "list"
  )
})

rm(res)

fetch_number <- 100
num_of_works <- 10000
### The only difference from the above oa_fetch is the topics.id vs. id
## maybe it is my network?? 
# 2024-09-23: 10,000 works: 1.7GB data (my internet 90M/b about 155 second to download) : 766 seconds (real time): TBD in UA

res <-list()
#profvis({
system.time({
  batch_identifiers <-works_published_ref_unique[1:num_of_works]
  res <-oa_fetch(identifier=batch_identifiers, 
                 entity = "works",
                 options= list(sample=fetch_number, seed=1), 
                 output="list")
})

fetch_number <- 50
num_of_works <- length (works_published_ref_combined)

range_i <- seq(1, num_of_works, by=fetch_number)
works_cited_ls <- vector("list", length = length(range_i))

### Code has bugs??a lot of these have NA value???
time_taken <-system.time({
  for (idx in seq_along(range_i)) {
    i <- range_i[idx]
    batch_identifiers <-works_published_ref_combined[i:min(i+fetch_number-1, num_of_works)]
    batch_data <-oa_fetch(identifier=batch_identifiers, primary_location.source.type = "journal", )
                          #output="list", )
    works_cited_ls[[idx]] <- batch_data
  }
})
print(paste("fetch time: ", time_taken["elapsed"] / 60, "minutes"))

tail(works_cited_ls)

works_cited <- rbindlist(works_cited_ls, use.names=TRUE, fill=TRUE) 


#########################
# Ensure oa_fetch() is receiving the correct input and create a new dataframe for results.
works_cited <- data.frame()
works_cited2 <-data.frame()

fetch_number <- 50
num_of_works <- length (works_published_ref_combined)

# Loop to fetch data in batches
time_taken <- system.time({
  for(i in seq(1, num_of_works, by = fetch_number)) {
    batch_identifiers <- works_published_ref_combined[i:min(i + fetch_number - 1, num_of_works)]
    
    # Check if the batch_identifiers is a valid vector
    if (length(batch_identifiers) > 0 && !all(is.na(batch_identifiers))) {
      # Fetch data from OpenAlex using oa_fetch, ensure proper identifier input
      batch_data <- tryCatch({
        # Have to use "primary_location.source.type = journal" to filter out non-journal.
        # issn_l cannot be used alone (there are book chapters which have issn per OpenAlex)
        oa_fetch(identifier = batch_identifiers) 
                 #, primary_location.source.type = "journal")
      }, error = function(e) {
        message("Error fetching data: ", e)
        return(NULL)
      })
     # Only bind non-null data
      if (!is.null(batch_data) && nrow(batch_data) >0 ) {
        # Ensure consistent columns
        batch_data <- data.table::setDT(batch_data)[, setdiff(names(works_cited), names(batch_data)) := NA]
        works_cited <- rbindlist(list(works_cited, batch_data), use.names = TRUE, fill = TRUE)
      }
    }
  }
})
print(time_taken)

head(works_cited)
setdiff(works_cited, works_cited2)
dfdiff_2023<-setdiff(works_cited2, works_cited)
works_cited <- works_cited2

### For 2022 data pulled from 2025-01 and 2025-02, there is 18 / 3

######################################################
### There are two types of citations 
#
# 1. works_cited_2022.rds --> work cited by UA authors of any type publications (e.g. journal articles, journal reviews, reports, repository items)
############################################

# 2. works_cited_source_journal_2022.rds --> source.type = journal (work cited by UA authors of only journal type such as articles and reviews.  
# 

#### Step 1: Re-generate a new row if it matches (meaning; cited multiple times.)

## Save works_cited files
saveRDS(works_cited, "../works_cited_2020.rds")
saveRDS(works_cited, "../works_cited_2021.rds")




#######################################################################################
# SECTION 2: Works cited
######################################################################################

#  UA: 2022: 342,918 
# ASU: 2022: 303,563
# MSU: 2022: 356,486
#  UW: 2022: 678,317 
saveRDS(works_cited, "../msu_works_cited_2022.rds")

#  UA: 2023: 353,424
# ASU: 2023: 317,643
# MSU: 2023: 349,299
#  UW: 2023: 706,551
saveRDS(works_cited, "../msu_works_cited_2023.rds")

# 2024-04-04: 
#  UA: 2024: 305,670
# ASU: 2024: 271,694
# MSU: 2024: 307,672
#  UW: 2024: 616,427
saveRDS(works_cited, "../msu_works_cited_2024.rds")

rm(works_cited)

works_cited_2019 <- readRDS("../works_cited_2019.rds")
works_cited_2020 <- readRDS("../works_cited_2020.rds")
works_cited_2021 <- readRDS("../works_cited_2021.rds")

works_cited_2022 <- readRDS("../works_cited_2022.rds")

works_cited_2023 <- readRDS("../works_cited_2023.rds")

works_cited_2024 <- readRDS("../works_cited_2024.rds")

# One is primary.source.type = journal, the other (works_cited_2) contains everything
# For year 2022, 325,520 : 345,813. 

### If not filtering by "primary_location:source=journal", there are more.
# For example, https://api.openalex.org/works/W2984048300 (source = null)

difference_df1_df2 <- setdiff(works_cited$id, works_cited_2$id)
difference_df2_df1 <- setdiff(works_cited_2$id, works_cited$id)
head(difference_df2_df1)

############# Testing
difference_df1_df2 <- setdiff(works_cited$id, works_published_ref_combined)
difference_df2_df1 <- setdiff(works_published_ref_combined, works_cited$id)
head(difference_df2_df1)
head(works_cited$id)
head(matching_rows$id)
######################


#### need to recheck the numbers
# Step 2: Add these matching rows as new rows 
# matching_rows <- works_cited[works_cited$id %in% names(works_ref_more_cited_counts), ]
# Step 3: Repeat each row in the DataFrame based on the count in works_published_ref_more_cited
# matching_rows_expanded <- matching_rows[rep(1:nrow(matching_rows), times = works_ref_more_cited_counts[matching_rows$id]), ]

#matching_rows <- works_cited[works_cited$id %in% names(citation_counts), ]
# Step 3: Repeat each row in the DataFrame based on the count in works_published_ref_more_cited
#matching_rows_expanded <- matching_rows[rep(1:nrow(matching_rows), times = citation_counts[matching_rows$id]), ]

# Step4: We have the final works cited, including multiple occurances of a work
# works_cited <- rbind(works_cited, matching_rows_expanded)

### Questions: 
# 1. I fetched 354,355 unique works, returned 325,520 
# 2. 

# Count the occurrences of each unique element in the vector
#works_ref_more_cited_counts <- table(works_published_ref_more_cited)
# works_cited <- works_published_ref_combined

#### Naming Convention ###
### Within each of these categories, we further classify works based on two criteria: 
##### 1. Source Type: We distinguish between works originating from sources with an ISSN, and those from non-ISSN sources.
##### 2. Work Type: Within each source type, we differentiate between articles (traditional research papers) and other types of works (e.g., books, book chapters, preprints).

### This hierarchical classification system enables us to examine patterns in both the types of 
### publications produced by UA authors and the sources they cite, allowing for a deeper understanding of 
### research trends and influences within the UA. This system can easily expand if you need further categorization (e.g.,by discipline, college, department).

### We follow a hierarchical and descriptive approach with the following general naming structure:  
##### category_subcategory_sub-subcategory
##### Where: 
######### Category = works_published or works_cited (differentiates between UA's output and what they reference)
#########   Subcategory = source_issn or source_nonissn (indicates the type of source)
#########     Sub-subcategory: either type or publisher 
###########     type = defined above (e.g. articles or other)
###########     publisher = defined above.

###################### Citation Analysis ####################################
# 1. Analyse journal usage
# Date fetched: 2024-10 and 2024-12:

###  # of works_cited = # of works_cited_source_issn + # of works_cited_source_nonissn

### works_cited_source_issn_articles
##### # of works_cited_source_issn = # of works_cited_source_issn_articles + # of source_issn_non_articles_cited.
##### Example: 330,005 (works_cited_source_issn) = 287,142 (works_cited_source_issn_articles) + 42,863 (works_cited_source_issn_nonarticle)

# 2023 data: 353,424 (works_cited) = 330,005 (works_cited_source_issn) + 23,419 (works_cited_source_nonissn)
### 330,005 (works_cited_source_issn) = 287,142 (works_cited_source_issn_articles) + 42,863 (works_cited_source_issn_nonarticle)
### 23,419 (works_cited_source_noissn) = 9,335 (works_cited_source_nonissn_articles, e.g. arXiv/PubMed) + 14,084 (works_cited_nonissn_nonarticles, e.g. preprint, book, book-chapter)

# 2022: 342,900 (works_cited) =  320,227 (works_cited_source_issn) + 22,673 (works_cited_nonissn)
###### 320,227 = 276,684 + 43,543
######  22,673 =  8,700 + 13,973

# 2021: 374,067 (works_cited) = 341,738(works_cited_source_issn) + 32,329 (works_cited_source_nonissn)
######## 341,738 = 297,819 + 43,919
########  32,329 = 13,150 + 19,179

# 2020: 382,495 articles out of 421,866 works: 91%
# 2019: 291,705 articles out of 323,779 works: 90%

########################################################################################
#########################################################################################
### Step 2: Separate works_cited using criteria such as "type", "ISSN" or other criteria
# First getting all the works_cited by year data
works_cited <- works_cited_2021 %>%
  mutate(authored_year = 2021) %>%
  select(authored_year, everything())  # This moves UA_authored_year to first position

works_cited <- works_cited_2022 %>%
  mutate(authored_year = 2022) %>%
  select(authored_year, everything())  # This moves UA_authored_year to first position

works_cited <- works_cited_2023 %>%
  mutate(authored_year = 2023) %>%
  select(authored_year, everything())  # This moves UA_authored_year to first position

works_cited <- works_cited_2024 %>%
  mutate(authored_year = 2024) %>%
  select(authored_year, everything())  # This moves UA_authored_year to first position

#########################################################
# Step 2.1: One way is via type = article
# works_cited = works_cited_type_articles + works_cited_type_nonarticles
##########################################################

works_cited_type_articles    <- subset(works_cited, type == "article")
unique(works_cited_type_articles$type)
unique_issns <- unique(works_cited_type_articles$issn_l)
number_of_unique_issns <- length(unique_issns)

works_cited_type_nonarticles <- subset(works_cited, type != "article")
unique(works_cited_type_nonarticles$type)
unique_issns2 <- unique(works_cited_type_nonarticles$issn_l)
number_of_unique_issns2 <- length(unique_issns2)

####################################################################
#########################################################################
# Step 2.2: The other way is to filter rows where issn_l is neither NA nor an empty string
works_cited_source_issn_index <- !is.na(works_cited$issn_l) & works_cited$issn_l != ""
works_cited_source_issn <- works_cited[works_cited_source_issn_index, ]
works_cited_source_nonissn <- works_cited[!works_cited_source_issn_index, ]
#############################
# Filter records where type is "article" (excluding conference paper etc )
works_cited_source_issn_articles    <- works_cited_source_issn[works_cited_source_issn$type == "article", ]
works_cited_source_issn_nonarticles <- works_cited_source_issn[works_cited_source_issn$type != "article", ]
works_cited_source_nonissn_articles    <- works_cited_source_nonissn[works_cited_source_nonissn$type == "article", ]
works_cited_source_nonissn_nonarticles <- works_cited_source_nonissn[works_cited_source_nonissn$type != "article", ]

#######################################################################
### Step 3: Getting analysis for publisher

# publisher: host_organization
unique_publishers <- unique(works_cited_type_articles$host_organization)
num_unique_publishers <- length(unique_publishers)  # number of publishers: ~1,600
print(unique_publishers[1:50])

####################### Using ISSN 
# list NULL publishers ~ 1 %
# 2023: 2,227 (probably need ISSN matching) / 2,922 NA/
# 2022: 3,312 NA / 323,221
# 2021: 3,687 NA / 341,738 
# 2020: 4,039 NA / 382,495
num_na <- sum(is.na(works_cited_source_issn$host_organization))
# Replace NA values and empty strings with "NA"
works_cited_source_issn$host_organization[is.na(works_cited_source_issn$host_organization) | trimws(works_cited_source_issn$host_organization) == ""] <- "NA"

# Dealing with "NA" data in "host_organization" field.
# 1. First, showing all NA publisher: meaning publisher info is not available. 
publisher_NA <- works_cited_source_issn[works_cited_source_issn$host_organization == "NA", ]

publisher_NA_id <-unique(publisher_NA$id)
# Check if any row in the df 'publisher_NA' contains a non-missing value in the "issn_l" column
publisher_NA_with_issn <- publisher_NA[!is.na(publisher_NA$`issn_l`) & publisher_NA$`issn_l` != "", ]
print(publisher_NA_with_issn)

# Extract unique ISSNs from the 'issn_l' column: 1235 unique issns
# 2023: 1,236 / 3,489 NA
# 2022: 1,110 / 3,312 NA
# 2021: 1,204 / 3,687 NA
# 2020: 1,737 / 4,039 NA 
unique_issn <- unique(publisher_NA$`issn_l`)
print(unique_issn)

# Convert the 'author' dataframe to JSON for each row
publisher_NA <- publisher_NA %>%
  mutate(author = sapply(author, function(x) toJSON(x)))

# Truncate only strings that exceed Excel's 32,767 character limit
publisher_NA <- publisher_NA %>%
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))

### old code: 2024-12
publisher_microbiology <- works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
publisher_springer <- works_cited_source_issn[tolower(works_cited_source_issn$host_organization_name) == tolower("Springer Science+Business Media"), ]
publisher_plos <- works_cited_source_issn[grepl("Public Library of Science", works_cited_source_issn$host_organization_name, ignore.case = TRUE), ]
publisher_aaas <- works_cited_source_issn[grepl("American Association for the Advancement of Science", works_cited_source_issn$host_organization_name, ignore.case = TRUE), ]
publisher_ua  <- works_cited_source_issn[grepl("University of Arizona",       works_cited_source_issn$host_organization, ignore.case = TRUE), ]
publisher_uap <- works_cited_source_issn[grepl("University of Arizona Press", works_cited_source_issn$host_organization, ignore.case = TRUE), ]
works_cited_source_issn_cell <- works_cited_source_issn[grepl("Cell Press", works_cited_source_issn$host_organization, ignore.case = TRUE), ]

publisher_cell_press_unique <- unique(publisher_cell_press)
df <-publisher_cell_press

# IWA: cited (yyyy): 19 (2019), 34 (2020), 21 (2021), 19 (2022),   
works_cited_source_issn_iwa <- works_cited_source_issn[grepl("IWA Publishing", works_cited_source_issn$host_organization_name, ignore.case = TRUE), ]
truncate_and_write(works_cited_source_issn_iwa)

id_counts <-table(publisher_iwa$id)
duplicateds <- id_counts[id_counts >= 1]
print(id_counts)

# APS: 
# 2023: journal (article, review): 166; Non-journal (book-chapter): 0
# 2022: journal (article, review): 230; Non-journal (book-chapter): 2
# 2021: journal (article, review) : 170; Non-journal (book-chapter) : 2
works_cited_source_issn_aps  <- works_cited_source_issn[grepl("American Phytopathological Society", works_cited_source_issn$host_organization, ignore.case = TRUE), ]
works_cited_source_nonissn_aps <- works_cited_source_nonissn[grepl("American Phytopathological Society", works_cited_source_nonissn$host_organization, ignore.case = TRUE), ]

# Create a list to hold the data frames
cited_all_types <- list(
  APS_journal_type = publisher_aps, 
  APS_non_journal_type = publisher_aps2  
)
# Write the list to an Excel file with each data frame as a separate sheet
write_xlsx(cited_all_types, "citations/publisher_aps_cited_works_2022.xlsx")

# 2025-01: BMJ:
# 2023: journal (article, review): 1,694 ; Non-journal: 0
# 2022: journal (article, review): 1,914 ; Non-journal: 0
# 2021: journal (article, review): 1,815 ; Non-journal: 0
works_cited_source_issn_bmj  <- works_cited_source_issn[grepl("BMJ", works_cited_source_issn$host_organization, ignore.case = TRUE), ]
works_cited_source_nonissn_bmj <- works_cited_source_nonissn[grepl("BMJ", works_cited_source_nonissn$host_organization, ignore.case = TRUE), ]

truncate_and_write(works_cited_source_issn_bmj)

###############################################################################
#### Step 4: Analyzing publisher
###############################################################################
############################################################
##### 2025-02: Brill (https://openalex.org/publishers/p4310320561)
# 2024: 100, 
# 2023: 100 (article), 54 (nonarticle)
# 2022: 109 (article)

# Criteria: article and nonarticle.
publisher_str <- "Brill"
works_cited_type_articles_brill <- works_cited_type_articles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_nonarticles_brill <- works_cited_type_nonarticles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_published_brill <- works_published %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

### Test data
search_string <- "https://openalex.org/W2944198613"


works_cited_type_articles_brill_22 <- works_cited_type_articles_brill

works_cited_type_articles_brill_23 <- works_cited_type_articles_brill

works_cited_type_articles_brill_24 <- works_cited_type_articles_brill

works_cited_type_articles_brill_22_23_24 <- bind_rows(works_cited_type_articles_brill_22, 
                                                      works_cited_type_articles_brill_23, 
                                                      works_cited_type_articles_brill_24)

saveRDS(works_cited_type_articles_brill_22_23_24, "./citations/works_cited_type_articles_brill_22_23_24.rds")

works_cited_type_articles_brill_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_brill_22_23_24, 1)
write_df_to_excel(works_cited_type_articles_brill_yr22_23_24)

# Combine Excel Files
excel_files <- c("citations/works_cited_type_articles_brill_yr22_23_24.xlsx", "citations/brill_22_23_24_top_cited_journals.xlsx", "citations/README.xlsx")
tryCatch({
  wb <- createWorkbook()
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("citations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  saveWorkbook(wb, "citations/works_cited_type_articles_brill_22_23_24_v1.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})

###############################################
##### 2025-04: Emerald
# Emerald: type_articles: cited (yyyy): 237 (2024), 325(2023), 290 (2022),  
publisher_str <- "Emerald"
works_cited_type_articles_emerald <- works_cited_type_articles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

# type_nonarticles: 32 (2024), 43 (2023), 40 (2022)
works_cited_type_nonarticles_Emerald <- works_cited_type_nonarticles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

# published: 2 (2024), 8 (2023), 2 (2022)
works_published_Emerald <- works_published %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

### Test data for a work published by Emerald.
work_cited_str <- "https://openalex.org/W4211158612"
work_cited_str <- "https://openalex.org/W2010044735"

find_citing_works(work_cited_str, works_published_2023)


works_cited_type_articles_emerald_22 <- works_cited_type_articles_emerald

works_cited_type_articles_emerald_23 <- works_cited_type_articles_emerald

works_cited_type_articles_emerald_24 <- works_cited_type_articles_emerald

works_cited_type_articles_emerald_22_23_24 <- bind_rows(works_cited_type_articles_emerald_22, 
                                                      works_cited_type_articles_emerald_23, 
                                                      works_cited_type_articles_emerald_24)
# save or load  
saveRDS(works_cited_type_articles_emerald_22_23_24, "./citations/works_cited_type_articles_emerald_22_23_24.rds")

works_cited_type_articles_emerald_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_emerald_22_23_24, 1)
write_df_to_excel(works_cited_type_articles_emerald_yr22_23_24)


#### 2025-04: Taylor & Francis
# 2022: 7,134 
# 2023: 6,937
# 2024: 6,007

publisher_str <- "Taylor & Francis"
works_cited_type_articles_tf <- works_cited_type_articles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_nonarticles_tf <- works_cited_type_nonarticles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_published_tf <- works_published %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))


works_cited_type_articles_tf_22 <- works_cited_type_articles_tf

works_cited_type_articles_tf_23 <- works_cited_type_articles_tf

works_cited_type_articles_tf_24 <- works_cited_type_articles_tf

works_cited_type_articles_tf_22_23_24 <- bind_rows(works_cited_type_articles_tf_22, 
                                                      works_cited_type_articles_tf_23, 
                                                      works_cited_type_articles_tf_24)

saveRDS(works_cited_type_articles_tf_22_23_24, "./citations/works_cited_type_articles_tf_22_23_24.rds")
works_cited_type_articles_tf_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_tf_22_23_24, 1)
write_df_to_excel(works_cited_type_articles_tf_yr22_23_24)

# Combine Excel Files
excel_files <- c("citations/works_cited_type_articles_tf_yr22_23_24.xlsx", "citations/tf_22_23_24_top_cited_journals.xlsx", "citations/README.xlsx")
tryCatch({
  wb <- createWorkbook()
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("citations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  saveWorkbook(wb, "citations/works_cited_type_articles_tf_22_23_24_v1.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})

#### 2025-04: Springer Nature: there are two publishers "Springer Nature" and "Springer Nature (Netherland) :
# 2022: MSU: 3,648; UArizona: 2,686 ; U Washington: 6,950; 
# 2023: MSU: 3,694; UArizona: 3,118; U Washington: 8,189; 
# 2024: MSU: 2,792; UArizona: 2,550; U Washington: 6,787

publisher_str <- "Springer Nature"

# Since there are two publishers: use "grepl"
#works_cited_type_articles_sn <- works_cited_type_articles %>%  filter(tolower(host_organization) == tolower(publisher_str))
rm(works_cited_type_articles_sn)
works_cited_type_articles_sn <- works_cited_type_articles %>%
  filter(grepl(publisher_str, host_organization_name, ignore.case = TRUE))

works_cited_type_nonarticles_sn <- works_cited_type_nonarticles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_published_sn <- works_published %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))


works_cited_type_articles_sn_22 <- works_cited_type_articles_sn

works_cited_type_articles_sn_23 <- works_cited_type_articles_sn

works_cited_type_articles_sn_24 <- works_cited_type_articles_sn

works_cited_type_articles_sn_22_23_24 <- bind_rows(works_cited_type_articles_sn_22, 
                                                      works_cited_type_articles_sn_23, 
                                                      works_cited_type_articles_sn_24)

actual_df <- "works_cited_type_articles_sn_22_23_24"
if (exists(actual_df) && is.data.frame(get(actual_df))) {
  df <- get(actual_df)

  rds_file_name <- paste0("msu_", actual_df, ".rds")
  rds_file_path <- file.path("./citations", rds_file_name)
  
  saveRDS(df, rds_file_path)
  df_processed2 <- extract_topics_by_level(df, 1)
  
  works_cited_type_articles_sn_yr22_23_24 <- df_processed2 
  
  
} else {
  if (!exists(actual_df)) {
    print(paste("Error: Data frame '", actual_df, "' does not exist. Skipping operations.", sep=""))
  } else {
    print(paste("Error: Object '", actual_df, "' exists but is not a data frame. Skipping operations.", sep=""))
  }
}




#saveRDS(works_cited_type_articles_sn_22_23_24, "./citations/uw_works_cited_type_articles_sn_22_23_24.rds")
#source("my_functions.R")
#works_cited_type_articles_sn_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_sn_22_23_24, 1)


## 2025-04: Testing "Nature Portfolio"
### Since Nature journals may have the same publisher as "Springer Nature", so Using ISSNs to match is the best way to go
### 2024 data: Publisher is now "Nature Portfolio", which has 13,424 records (99.7% matched)
### 2024 data: Matching with ISSNs: 13,463 records.
### 2022 data: Matching "Nature Portofolio": 12,431; Matching ISSNs: 13,416
### 2023 data: Matching "Nature Portofolio":  ; Matching ISSNs: 


# 2022: MSU: 11,471; U Arizona: 12,431; U Washington: 32,079
# 2023: MSU: 12,104; U Arizona: 13,723; U Washington: 35,300
# 2024: MSU: 11,663; U Arizona: 12,564; U Washington: 33,026

publisher_str <- "Nature Portfolio" 

works_cited_type_articles_nature <- works_cited_type_articles %>%
  filter(tolower(host_organization_name) == tolower(publisher_str))


works_cited_type_articles_nature_22 <- works_cited_type_articles_nature

works_cited_type_articles_nature_23 <- works_cited_type_articles_nature

works_cited_type_articles_nature_24 <- works_cited_type_articles_nature

works_cited_type_articles_nature_22_23_24 <- bind_rows(works_cited_type_articles_nature_22, 
  works_cited_type_articles_nature_23, 
  works_cited_type_articles_nature_24)

#saveRDS(works_cited_type_articles_nature_22_23_24, "./citations/uw_works_cited_type_articles_nature_22_23_24.rds")
#works_cited_type_articles_nature_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_nature_22_23_24, 1)
#write_df_to_excel(works_cited_type_articles_nature_yr22_23_24)

actual_df <- "works_cited_type_articles_nature_22_23_24" 

if (exists(actual_df) && is.data.frame(get(actual_df))) {
  original_data <- get(actual_df)

    rds_file_name <- paste0("uw_", actual_df, ".rds")
  rds_file_path <- file.path("./citations", rds_file_name)
  saveRDS(original_data, rds_file_path)
  
  df_processed <- extract_topics_by_level(original_data, 1)
  
  # To make write_df_to_excel(VAR) generate a filename like "actual_df_value.xlsx",
  # we assign the processed data to a variable whose name is the string held in actual_df.
  # Note: This will replace/overwrite the object named by 'actual_df' in your current environment
  # with the content of 'df_processed'.
  assign(actual_df, df_processed)
  
  # Now, construct and evaluate the call to write_df_to_excel.
  # This dynamically builds and runs a command like:
  # write_df_to_excel(works_cited_type_articles_sn_22_23_24)
  # where 'works_cited_type_articles_sn_22_23_24' now holds the processed data.
  # Your write_df_to_excel function would then internally derive the filename
  # (e.g., "works_cited_type_articles_sn_22_23_24.xlsx") from this variable name.
  eval(parse(text = paste0("write_df_to_excel(", actual_df, ")")))
  
} else {
  if (!exists(actual_df)) {
    print(paste("Error: Data frame '", actual_df, "' does not exist. Skipping operations.", sep=""))
  } else {
    print(paste("Error: Object '", actual_df, "' exists but is not a data frame. Skipping operations.", sep=""))
  }
}




############################################
############################################
# We can use ISSNs to do exact match. 
xlsx_file_path <- "2025-nature-journals-issns.xlsx"
df <- works_cited_type_articles
df_issn_col_name <- "issn_l"

issn_p_col_name <- "ISSN print"
issn_e_col_name <- "ISSN electronic"

if (!file.exists(xlsx_file_path)) {
  stop(paste("Error: File not found at path:", xlsx_file_path))
}
tryCatch({
  nature_issns_df <- read_excel(xlsx_file_path)
}, error = function(e) {
  stop(paste("Error reading Excel file:", e$message))
})

if (!issn_p_col_name %in% names(nature_issns_df)) {
  stop(paste("Error: Column '", issn_p_col_name, "' not found in the Excel file."))
}
if (!issn_e_col_name %in% names(nature_issns_df)) {
  stop(paste("Error: Column '", issn_e_col_name, "' not found in the Excel file."))
}

# Extract ISSNs from both columns, combine, remove NAs, and get unique values
nature_issns_list <- unique(c(
  na.omit(nature_issns_df[[issn_p_col_name]]),
  na.omit(nature_issns_df[[issn_e_col_name]])
))

# Optional: You might need to clean the ISSNs if the format differs
# (e.g., remove hyphens) between the files.
# Example:
# nature_issns_list <- gsub("-", "", nature_issns_list)
# df[[df_issn_col_name]] <- gsub("-", "", df[[df_issn_col_name]])

cat(sprintf("Extracted %d unique ISSNs from the Nature journals file.\n", length(nature_issns_list)))

if (!exists("df")) {
  stop("Error: DataFrame 'df' not found. Please load or define it before this step.")
}
if (!df_issn_col_name %in% names(df)) {
  stop(paste("Error: Column '", df_issn_col_name, "' not found in DataFrame 'df'."))
}

df_filtered <- df %>%
  filter(!is.na(.data[[df_issn_col_name]])) %>%
  filter(.data[[df_issn_col_name]] %in% nature_issns_list)

### For OpenAlex: 2025-04-27
### Certain % data 0.3%-1% Nature journals articles are classified as "Springer Nature", while 99.7%+ are published by "Nature Portfolio"
#--- Configuration ---
dataframe1 <- works_cited_type_articles_sn
dataframe2 <- works_cited_type_articles_sn2 #df_filtered

id_column_name <- "id"
if (!id_column_name %in% names(dataframe1)) {
  stop(paste("Error: Column '", id_column_name, "' not found in the first dataframe."))
}
if (!id_column_name %in% names(dataframe2)) {
  stop(paste("Error: Column '", id_column_name, "' not found in the second dataframe."))
}

# Extract the ID column from each dataframe
ids_df1 <- dataframe1[[id_column_name]]
ids_df2 <- dataframe2[[id_column_name]]

# Find the unique IDs that are present in BOTH vectors
common_ids <- intersect(ids_df1, ids_df2)
count_of_common_ids <- length(common_ids)
cat(sprintf("Number of unique IDs present in both dataframes: %d\n", count_of_common_ids))

# --- Find IDs in df1 but NOT in df2 ---
ids_only_in_df1 <- setdiff(ids_df1, ids_df2)
count_only_in_df1 <- length(ids_only_in_df1)
dataframe1_only <- dataframe1[dataframe1[[id_column_name]] %in% ids_only_in_df1, ]

### There are two journals (Horticulture Research with Weily) no longer with Nature. 1 journal "Nature New Biology" is no longer in press
ids_only_in_df2 <- setdiff(ids_df2, ids_df1)
dataframe2_only <- dataframe2[dataframe2[[id_column_name]] %in% ids_only_in_df2, ]

# --- Step : Assign the filtered data to the new DataFrame ---
works_cited_type_articles_nature <- df_filtered
cat(sprintf("\nCreated 'works_cited_type_articles_nature' DataFrame with %d rows.\n", nrow(works_cited_type_articles_nature)))

#######################################################

### Binding Springer Nature and Nature together
works_cited_type_articles_nature_yr22_23_24 <-df_processed

works_cited_type_articles_nature_sn_yr22_23_24 <-bind_rows(works_cited_type_articles_nature_yr22_23_24, works_cited_type_articles_sn_yr22_23_24)
write_df_to_excel(works_cited_type_articles_nature_sn_yr22_23_24)

# 2025-05: old openAlex data structure using different name "so", "host_organization"
# top_cited_journals <- rank_top_cited_journals(works_cited_type_articles_nature_sn_yr22_23_24, "so", "issn_l", "host_organization", 3000)

top_cited_journals <- rank_top_cited_journals(works_cited_type_articles_nature_sn_yr22_23_24, "source_display_name", "issn_l", "host_organization_name", 3000)

ua_df <- readxl::read_excel("citations/nature_sn_yr22_23_24_top_cited_j.xlsx")

# ou_df <-readxl::read_excel("citations/asu_nature_sn_yr22_23_24_top_cited_j.xlsx")
ou_df <-readxl::read_excel("citations/msu_nature_sn_yr22_23_24_top_cited_j.xlsx")
# ou_df <-readxl::read_excel("citations/uw_nature_sn_yr22_23_24_top_cited_j.xlsx")


# Compare top 10
comparison <- compare_top_journals(ua_df, ou_df, 10)
print("--- Top 10 Journal Comparison ---")
print(paste("Common Journals:", paste(comparison$common, collapse = ", ")))
print(paste("UA Unique Journals:", paste(comparison$ua_unique, collapse = ", ")))
print(paste("U Unique Journals:", paste(comparison$ou_unique, collapse = ", ")))
print(paste("Number of Common Journals:", comparison$count_common))
print(paste("Number of UA Unique Journals:", comparison$count_ua_unique))
print(paste("Number of U Unique Journals:", comparison$count_ou_unique))

# Compare top 100
comparison <- compare_top_journals(ua_df, ou_df, 100)
print("--- Top 100 Journal Comparison ---")
print(paste("Common Journals:", paste(comparison$common, collapse = ", ")))
print(paste("UA Unique Journals:", paste(comparison$ua_unique, collapse = ", ")))
print(paste("U Unique Journals:", paste(comparison$ou_unique, collapse = ", ")))
print(paste("Number of Common Journals:", comparison$count_common))
print(paste("Number of UA Unique Journals:", comparison$count_ua_unique))
print(paste("Number of U Unique Journals:", comparison$count_ou_unique))

# Combine Excel Files
excel_files <- c("citations/msu_works_cited_type_articles_nature_sn_yr22_23_24.xlsx", "citations/msu_nature_sn_yr22_23_24_top_cited_j.xlsx", "citations/README.xlsx")
tryCatch({
  wb <- createWorkbook()
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("citations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  saveWorkbook(wb, "citations/msu_works_cited_type_articles_nature_sn_yr22_23_24_v2.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})




#### 2025-04: Elsevier


publisher_str <- "Elsevier"
works_cited_type_articles_elsevier <- works_cited_type_articles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_nonarticles_elsevier <- works_cited_type_nonarticles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_published_elsevier <- works_published %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_articles_elsevier_22 <- works_cited_type_articles_elsevier

works_cited_type_articles_elsevier_23 <- works_cited_type_articles_elsevier

works_cited_type_articles_elsevier_24 <- works_cited_type_articles_elsevier

works_cited_type_articles_elsevier_22_23_24 <- bind_rows(works_cited_type_articles_elsevier_22, 
                                                      works_cited_type_articles_elsevier_23, 
                                                      works_cited_type_articles_elsevier_24)

saveRDS(works_cited_type_articles_elsevier_22_23_24, "./citations/works_cited_type_articles_elsevier_22_23_24.rds")

works_cited_type_articles_elsevier_22_23_24 <- readRDS("../../works_cited_type_articles_elsevier_22_23_24.rds")

works_cited_type_articles_elsevier_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_elsevier_22_23_24, 1)
write_df_to_excel(works_cited_type_articles_elsevier_yr22_23_24)

# Combine Excel Files
excel_files <- c("citations/works_cited_type_articles_elsevier_yr22_23_24.xlsx", "citations/elsevier_22_23_24_top_cited_journals.xlsx", "citations/README.xlsx")
tryCatch({
  wb <- createWorkbook()
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("citations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  saveWorkbook(wb, "citations/works_cited_type_articles_elsevier_22_23_24_v1.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})

#### 2025-04: Wiley
publisher_str <- "Wiley"
works_cited_type_articles_wiley <- works_cited_type_articles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_nonarticles_wiley <- works_cited_type_nonarticles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_published_wiley <- works_published %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_articles_wiley_22 <- works_cited_type_articles_wiley

works_cited_type_articles_wiley_23 <- works_cited_type_articles_wiley

works_cited_type_articles_wiley_24 <- works_cited_type_articles_wiley

works_cited_type_articles_wiley_22_23_24 <- bind_rows(works_cited_type_articles_wiley_22, 
                                                      works_cited_type_articles_wiley_23, 
                                                      works_cited_type_articles_wiley_24)

saveRDS(works_cited_type_articles_wiley_22_23_24, "./citations/works_cited_type_articles_wiley_22_23_24.rds")

works_cited_type_articles_wiley_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_wiley_22_23_24, 1)
write_df_to_excel(works_cited_type_articles_wiley_yr22_23_24)

# Combine Excel Files
excel_files <- c("citations/works_cited_type_articles_wiley_yr22_23_24.xlsx", "citations/wiley_22_23_24_top_cited_journals.xlsx", "citations/README.xlsx")
tryCatch({
  wb <- createWorkbook()
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("citations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  saveWorkbook(wb, "citations/works_cited_type_articles_wiley_22_23_24_v1.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})

#### 2025-04: Sage
publisher_str <- "Sage"
works_cited_type_articles_sage <- works_cited_type_articles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_nonarticles_sage <- works_cited_type_nonarticles %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_published_sage <- works_published %>%
  filter(grepl(publisher_str, host_organization, ignore.case = TRUE))

works_cited_type_articles_sage_22 <- works_cited_type_articles_sage

works_cited_type_articles_sage_23 <- works_cited_type_articles_sage

works_cited_type_articles_sage_24 <- works_cited_type_articles_sage

works_cited_type_articles_sage_22_23_24 <- bind_rows(works_cited_type_articles_sage_22, 
                                                      works_cited_type_articles_sage_23, 
                                                      works_cited_type_articles_sage_24)

saveRDS(works_cited_type_articles_sage_22_23_24, "./citations/works_cited_type_articles_sage_22_23_24.rds")

works_cited_type_articles_sage_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_sage_22_23_24, 1)
write_df_to_excel(works_cited_type_articles_sage_yr22_23_24)

# Combine Excel Files
excel_files <- c("citations/works_cited_type_articles_sage_yr22_23_24.xlsx", "citations/sage_22_23_24_top_cited_journals.xlsx", "citations/README.xlsx")
tryCatch({
  wb <- createWorkbook()
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("citations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  saveWorkbook(wb, "citations/works_cited_type_articles_sage_22_23_24_v1.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})

### Test data for Brill: 2025-02
## 2022: search journals articles do UA authors cited.
search_string <- "https://openalex.org/W2176010001"
search_references(search_string, works_cited_type_articles_brill_2022_2023)


# 2022
search_string <- "https://openalex.org/W2465933872" # 3 times
search_string <- "https://openalex.org/"  #2 times


########################################################################
###################### End of Testing ##################################
########################################################################

#### Find duplicates and frequencies #####
# change DF here
df <-works_cited_source_issn
# Find the rows that are duplicated
duplicate_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
# Create a table to count the frequency of duplicated rows
#duplicate_frequency <- table(apply(duplicate_rows, 1, paste, collapse = "-"))
duplicate_frequency <- table(duplicate_rows$id)
# show more than 10 times cited. change "10" to any number
duplicate_ids <- names(duplicate_frequency[duplicate_frequency > 10])

duplicate_multi_cited_rows <- df[df$id %in% duplicate_ids, ]

duplicate_multi_cited_rows <- duplicate_multi_cited_rows %>%   
  mutate(across(where(is.character), ~ ifelse(nchar(.) > 32767, substr(., 1, 32767), .)))

# Remove duplicate rows from duplicate_multi_cited_rows
duplicate_multi_cited_rows_unique <- duplicate_multi_cited_rows[!duplicated(duplicate_multi_cited_rows), ]

# write_xlsx(duplicate_multi_cited_rows, "citations/duplicate_multi_cited_2023.xlsx")
# write_xlsx(duplicate_multi_cited_rows_unique, "citations/duplicate_multi_cited_unique_2023.xlsx")

######################################
######################################
### Function: To count issns occurrences for a given publisher (note: issns count is more accurate)
# @param: dataframe issns_articles_cited
#          publisher_name
# return: issns and counts cited and sorted


count_issns_by_publisher <- function(works_cited_source_issn, publisher_name) {
  # Filter rows where host_organization matches the specified publisher
  publisher1 <- works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
  
  # Count the occurrences of each ISSN under the specified publisher
  issns_counts <- table(publisher1$so)
  issns_counts_df <- as.data.frame(issns_counts)
  
  colnames(issns_counts_df) <- c("Journal Title", "Count")
  # Sort the data frame by Count in descending order
  issns_counts_df <- issns_counts_df[order(issns_counts_df$Count, decreasing = TRUE), ]
  
  return(issns_counts_df)
}


library(dplyr)
# Use dplyr for the function
count_issns_by_publisher <- function(works_cited_source_issn, publisher_name) {
  works_cited_source_issn %>%
    filter(grepl(publisher_name, host_organization, ignore.case = TRUE)) %>%
    group_by(so) %>%
    summarize(Count = n()) %>%
    rename(`Journal Title` = so) %>%
    arrange(desc(Count))
}

publisher_name <- "Microbiology society"
publisher1 <-  works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_source_issn, publisher_name)
print(journal_counts_df)
# Note: Errors
# https://openalex.org/W2165027548 (1994 v44n3, Journal name changes and ISSN changed)


publisher_name <- "Optica Publishing Group"
publisher1 <-  works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_source_issn, publisher_name)
print(journal_counts_df)

publisher_name <- "Canadian Science Publishing"
publisher1 <-  works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_source_issn, publisher_name)
print(journal_counts_df)

publisher_name <- "IWA publishing"
publisher1 <-  works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_source_issn, publisher_name)
print(journal_counts_df)

publisher_name <- "Emerald Publishing"
publisher1 <-  works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
journal_counts_df <- count_issns_by_publisher(works_cited_source_issn, publisher_name)
print(journal_counts_df)
write_xlsx(journal_counts_df, "citations/publisher_emerald_2023_counts.xlsx")

publisher_name <- "American Phytopathological Society"
publisher1 <-  works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
issns_counts_df <- count_issns_by_publisher(works_cited_source_issn, publisher_name)
print(issns_counts_df)


publisher_name <- "BMJ"
publisher1 <-  works_cited_source_issn[grepl(publisher_name, works_cited_source_issn$host_organization, ignore.case = TRUE), ]
issns_counts_df <- count_issns_by_publisher(works_cited_source_issn, publisher_name)
print(issns_counts_df, n= Inf)
view(issns_counts_df)

unique_issns <- unique(publisher1$`issn_l`)
num_unique_issn<- length(unique_issns)
print(unique_issns)

unique_journals <- unique(publisher1$`so`)
num_unique_issn<- length(unique_journals)
print(unique_journals)

search_string <- "https://openalex.org/W2070851128"
search_references(search_string, works_published)

# Group by 'host_organization' and count the number of articles for each publisher
publisher_ranking <- works_cited_source_issn %>%
  group_by(host_organization) %>%
  summarise(article_count = n()) %>%
  arrange(desc(article_count))

# Calculate the total number of articles across all publishers
total_article_count <- sum(publisher_ranking$article_count)

# Calculate the percentage for each publisher relative to the total article count
publisher_ranking <- publisher_ranking %>%
  mutate(percentage = (article_count / total_article_count) * 100)

library(ggplot2)
top_20_publishers <- publisher_ranking %>% slice(1:20)
top_20_publishers$percentage <- (top_20_publishers$article_count / total_article_count) * 100
top_20_publishers$host_organization <- substr(top_20_publishers$host_organization, 1, 10)

# top 50
top_50_publishers <- publisher_ranking %>% slice(1:50)
top_50_publishers$percentage <- (top_50_publishers$article_count / total_article_count) * 100
top_50_publishers$host_organization <- substr(top_50_publishers$host_organization, 1, 10)

# top 100
top_100_publishers <- publisher_ranking %>% slice(1:100)
top_100_publishers$percentage <- (top_100_publishers$article_count / total_article_count) * 100
top_100_publishers$host_organization <- substr(top_100_publishers$host_organization, 1, 10)


# Bar plot for top 20 publishers
ggplot(top_20_publishers, aes(x = reorder(host_organization, -article_count), y = article_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  # Real number (article count) inside the bar
  geom_text(aes(label = article_count), vjust = 0.5, hjust = 1.2, size = 2.5, color = "white") +  
  # Adjust hjust and color for positioning inside
  # Percentage outside the bar
  geom_text(aes(label = sprintf("(%.1f%%)", percentage)), vjust = 0.5, hjust = -0.2, size = 3) +  
  # Adjust hjust for positioning outside
  coord_flip() +  # Flip the axis for better readability
  labs(x = "Publisher", y = "Number of Articles", title = "2022 UA Top 20 Publishers (Number of Articles Cited)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))  # Reduce font size of publisher names

# Calculate the percentage of the top 20, top 50, and top 100 publishers over the total
total_article_count <- sum(publisher_ranking$article_count) # Total articles in all publishers
top_20_total_count <- sum(top_20_publishers$article_count)  
top_50_total_count <- sum(top_50_publishers$article_count)  
top_100_total_count <- sum(top_100_publishers$article_count)  

# Calculate the percentage for year 2019, 2020, 2021, 2022, 2023
# Top  20: ~74-76%
# Top  50: ~90%
# Top 100: ~95%
top_20_percentage_of_total <- (top_20_total_count / total_article_count) * 100
top_50_percentage_of_total <- (top_50_total_count / total_article_count) * 100
top_100_percentage_of_total <-(top_100_total_count/ total_article_count) * 100

print(paste("Top 20 publishers represent",  round(top_20_percentage_of_total, 0), "% of the total articles."))
print(paste("Top 50 publishers represent",  round(top_50_percentage_of_total, 0), "% of the total articles."))
print(paste("Top 100 publishers represent", round(top_100_percentage_of_total, 0), "% of the total articles."))

view(publisher_ranking)
# View the top 50 publishers.  
# Top 10: Elsevier (20%), Wiley (9%), Oxford University Press (7%), ICP (5%), Springer(5%), Nature,
# IOP Publishing, Lippincott Williams & Wilkins, Taylor & Francis, SAGE Publishing (2%)


### Step 5: Final output to Excel

df <- works_cited_type_articles_nature_sn_yr22_23_24
required_columns <- c("source_display_name", "issn_l", "host_organization_name")
columns_exist <- required_columns %in% colnames(df)

if (all(columns_exist)) {  print("All required columns exist in the data frame.")
} else {  cat("MISSING columns.", "\n") }


# 1. top cited journals

rank_top_cited_journals(works_cited_type_articles_brill_22_23_24, "so", 2000)


rank_top_cited_journals(works_cited_type_articles_elsevier_22_23_24, "so", 8000)
rank_top_cited_journals(works_cited_type_articles_wiley_22_23_24, "so", 5000)

rank_top_cited_journals(works_cited_type_articles_sage_22_23_24, "so", 2000)

rank_top_cited_journals(works_cited_type_articles_tf_22_23_24, "so", 2000)

#top_cited_journals <- rank_top_cited_journals(works_cited_type_articles_nature_sn_yr22_23_24, "so", 3000)

#### Binding multiple years data
#works_cited_type_articles_brill_2022_2023 <- bind_rows(works_cited_type_articles_brill_2023, works_cited_type_articles_brill_2022)
# Extract primary topic and add topic-subfield-field-domain cols to the DF
#works_cited_type_articles_brill_combined_2022_2023 <- extract_topics_by_level(works_cited_type_articles_brill_2022_2023, 1)
#works_cited_type_articles_brill_yr22_23_24 <- extract_topics_by_level(works_cited_type_articles_brill_22_23_24, 1)

#write_df_to_excel(works_cited_type_nonarticles_brill)
#write_df_to_excel(works_published_brill)

# 2. Combine Excel Files
excel_files <- c("citations/works_cited_type_articles_brill_yr22_23_24.xlsx", "citations/brill_22_23_24_top_cited_journals.xlsx", "citations/README.xlsx")

tryCatch({
  wb <- createWorkbook()
  
  for (i in seq_along(excel_files)) {
    df <- read.xlsx(excel_files[i])
    sheet_name <- gsub("citations/(.*)\\.xlsx", "\\1", excel_files[i]) # Extract sheet name from file name
    sheet_name <-substr(sheet_name, 1, 31)  # Truncate to 31 chars for worksheet
    addWorksheet(wb, sheetName = sheet_name)
    writeData(wb, sheet = sheet_name, x = df)
  }
  
  saveWorkbook(wb, "citations/works_cited_type_articles_brill_22_23__24_v1.xlsx", overwrite = TRUE)
  message("!!! Combination successful!")
  
}, error = function(e) {
  message("Combination failed: ", e)
  print(e)
})


