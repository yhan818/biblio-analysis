# https://github.com/massimoaria/bibliometrix 

install.packages("bibliometrix")
library('bibliometrix')
#> To cite bibliometrix in publications, please use:
#> 
#> Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, 
#>                                  Journal of Informetrics, 11(4), pp 959-975, Elsevier.
#> 
#> https://www.bibliometrix.org
#> 
#>                         
#> For information and bug reports:
#>                         - Send an email to info@bibliometrix.org   
#>                         - Write a post on https://github.com/massimoaria/bibliometrix/issues
#>                         
#> Help us to keep Bibliometrix free to download and use by contributing with a small donation to support our research team (https://bibliometrix.org/donate.html)
#> 
#>                         
#> To start with the shiny web-interface, please digit:
#> biblioshiny()
#> 
#> ## An example from bibliometrix vignettes

file <- c("https://www.bibliometrix.org/datasets/management1.txt","https://www.bibliometrix.org/datasets/management2.txt")

getwd() 

# Data frame columns are named using the standard Clarivate Analytics WoS Field Tag codify (https://www.bibliometrix.org/documents/Field_Tags_bibliometrix.pdf).

file <- c("~/documnents/ua-dataset /management1.txt",".txt")
M <- convert2df(file = file, dbsource = "scopus", format = "")

M <- convert2df(file = file, dbsource = "wos", format = "plaintext")

# missingData returns a list containing two data frame. The first one, allTags includes the results for all metadata in M. The latter, mandatoryTags, reports the results only for the metadata needed to perform analyses with bibliometrix or biblioshiny.
# The column status classifies the percentage of missing value in 5 categories: “Excellent” (0%), “Good” (0.01% to 10.00%), “Acceptable” (from 10.01% to 20.00%), “Poor” (from 20.01% to 50.00%), “Critical” (from 50.01% to 99.99%), “Completely missing” (100%).
com <- missingData(M)

# biblioAnalysis returns an object of class "bibliometrix"
results <- biblioAnalysis(M, sep = ";")

options(width=100)
# function "summary" returns results of the biblio analysis
S <- summary(object = results, k = 10, pause = FALSE)

# basic plots 
plot(x = results, k = 10, pause = FALSE)

# mostly cited articles
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

# most frequent local cited authors
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]

CR$Papers[1:10,]

DF <- dominance(results, k = 10)
DF

# H index
indices <- Hindex(M, field = "author", elements="BORNMANN L", sep = ";", years = 10)

# Bornmann's impact indices:
indices$H

# Bornmann's citations
indices$CitationList

authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)

indices$H


topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

L <- lotka(results)

# Author Productivity. Empirical Distribution
L$AuthorProd

#> 
#> Converting your wos collection into a bibliographic dataframe
#> 
#> Done!
#> 
#> 
#> Generating affiliation field tag AU_UN from C1:  Done!
#> 
#> 

### biblioNetwork function
# The function biblioNetwork calculates, starting from a bibliographic data frame, the most frequently used networks: Coupling, Co-citation, Co-occurrences, and Collaboration.
# biblioNetwork uses two arguments to define the network to compute:
  # analysis argument can be “co-citation”, “coupling”, “collaboration”, or “co-occurrences”.
  # network argument can be “authors”, “references”, “sources”, “countries”, “universities”, “keywords”, “author_keywords”, “titles” and “abstracts”.

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

### Visualizating biblioNetwork using networkPlot function

# 1. Create a country collaboration network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.8)

# 2. Create a co-citation network
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", n=30, sep = ";")
# Plot the network
net=networkPlot(NetMatrix, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5) 

# 3. Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


# 4. Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M,field="ID", method="MCA", minDegree=10, clust=5, stemming=FALSE, labelsize=15, documents=20, graph=FALSE)
plot(CS$graph_terms)

plot(CS$graph_dendogram)


# 5. Create a historical citation network
histResults <- histNetwork(M, sep = ";")
#> 
#> WOS DB:
#> Searching local citations (LCS) by reference items (SR) and DOIs...
#> 
#> Analyzing 62646 reference items...
#> 
#> Found 422 documents with no empty Local Citations (LCS)

# Plot a historical co-citation network
net <- histPlot(histResults, n=20, size = FALSE,label="short")
