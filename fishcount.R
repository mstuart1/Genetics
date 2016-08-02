# This script attaches sample ID's to a gene pop and then counts the number of fish per year within that genepop


# 0) Set working directory ------------------------------------------------

setwd("~/Documents/Philippines/Genetics/")
source("code/old/Genetics/src/readGenepop_space.R")
source("code/old/Genetics/src/writeGenepop.R")

# 1) Read in genepop ------------------------------------------------------

genfile <- "2016-07-20_noregeno.genepop"
genedf <- readGenepop(genfile)



# 2) Figure out what kind of sample ID system you’re working with ---------

genedf$names


# 3) Connect to google data -----------------------------------------------

# open the laboratory database to retrieve sample info
library(googlesheets)
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1Rf_dFJ5WK-vTTsIT_kHHOcFrKzQtMFtKiuXiFw1lh9Y' # for Sample_Data file
lab <-gs_key(mykey)
# If this doesn't output Auto-refreshing stale OAuth token.
# Sheet successfully identified: "Sample_Data"
# Then use the #gs_auth above
lig <-gs_read(lab, ws='Ligations')
dig <- gs_read(lab, ws='Digests')
extr <- gs_read(lab, ws="Extractions")

# merge the two dataframes so that lig IDs match up
largedf <- merge(genedf, lig[,c('Ligation_ID', 'Digest_ID')], by.x = "names", by.y = "Ligation_ID", all.x = T)

# TEST - check the last 2 column names and that the number of rows hasn't changed
p <- ncol(largedf)
names(largedf[,(p-1):p]) # "dDocent_Contig_256998_105" "Digest_ID"
nrow(genedf) == nrow(largedf) # should be TRUE

# add extraction IDs
largedf <- merge(largedf, dig[,c("Digest", "Extraction_ID")], by.x = "Digest_ID", by.y = "Digest", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
p <- ncol(largedf)
names(largedf[,(p-1):p]) # "dDocent_Contig_256998_105" "Extraction_ID"
nrow(genedf) == nrow(largedf) # should be TRUE

# add sample ID's
largedf <- merge(largedf, extr[,c("Extract", "Sample_ID")], by.x = "Extraction_ID", by.y = "Extract", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
p <- ncol(largedf)
names(largedf[,(p-1):p]) # " dDocent_Contig_256998_105" "Sample_ID" 
nrow(genedf) == nrow(largedf) # should be TRUE
# look for missing names
setdiff(genedf$names, largedf$names) # should be character(0)


# 4) Count the fish per year ----------------------------------------------

# how many fish from each year are in genepop
length(which(substr(largedf$Sample_ID,5,6) == 12))
length(which(substr(largedf$Sample_ID,5,6) == 13))
length(which(substr(largedf$Sample_ID,5,6) == 14))
length(which(substr(largedf$Sample_ID,5,6) == 15))
length(which(substr(largedf$Sample_ID,5,6) == 16))

