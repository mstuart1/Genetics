# Malin wants a table of all of the samples that went into the Cervus identity analysis with their metadata


# 1) retreive samples that went into analysis -----------------------------

# Lightning
setwd("/Users/macair/Documents/Philippines/Genetics/")
source("code/old/Genetics/src/readGenepop_space.R")
source("code/old/Genetics/src/writeGenepop.R")

# 1) Strip down to Ligation ID --------------------------------------------

# locate the genepop file and read as data frame
genfile <- "2016-07-20_noregeno.genepop"
genedf <- readGenepop(genfile)

### WAIT ###

genedf[,1] <- NULL # remove the pop column from the data file
# TEST - make sure the first 2 columns are names and a contig and get number of rows
names(genedf[,1:2]) # [1] "names" "dDocent_Contig_107_30"
nrow(genedf) # 1651


# 2) Retrieve metadata from google ----------------------------------------

library(googlesheets)
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1Rf_dFJ5WK-vTTsIT_kHHOcFrKzQtMFtKiuXiFw1lh9Y' # for Sample_Data file
lab <-gs_key(mykey)

### WAIT ###

# If this doesn't output Auto-refreshing stale OAuth token.
# Sheet successfully identified: "Sample_Data"
# Then use the #gs_auth above
lig <-gs_read(lab, ws='Ligations')
dig <- gs_read(lab, ws='Digests')
extr <- gs_read(lab, ws="Extractions")
sample <- gs_read(lab, ws="Samples")

# merge the two dataframes so that lig IDs match up
# TODO - get the names to come over on it's own, currently requires bringing over a column of loci because bringing just one column turns it into a vector or factor (as.data.frame didn't work)
largedf <- merge(genedf[,c("names", "dDocent_Contig_107_30")], lig[,c('Ligation_ID', 'Digest_ID')], by.x = "names", by.y = "Ligation_ID", all.x = T)
largedf$dDocent_Contig_107_30 <- NULL

# TEST - check the last 2 column names and that the number of rows hasn't changed
names(largedf) # "dDocent_Contig_256998_105" "Digest_ID"
nrow(genedf) == nrow(largedf) # should be TRUE
largedf$names[1:5]

# add extraction IDs
largedf <- merge(largedf, dig[,c("Digest", "Extraction_ID")], by.x = "Digest_ID", by.y = "Digest", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
names(largedf) # "dDocent_Contig_256998_105" "Extraction_ID"
nrow(genedf) == nrow(largedf) # should be TRUE

# add sample ID's
largedf <- merge(largedf, extr[,c("Extract", "Sample_ID")], by.x = "Extraction_ID", by.y = "Extract", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
p <- ncol(largedf)
names(largedf[,(p-1):p]) # " dDocent_Contig_256998_105" "Sample_ID" 
nrow(genedf) == nrow(largedf) # should be TRUE

# add field data 
largedf <- merge(largedf, sample[,c("Sample_ID", "Lat", "Lon", "Date", "Size")], by.x = "Sample_ID", by.y = "Sample_ID", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
p <- ncol(largedf)
names(largedf[,(p-1):p]) # " dDocent_Contig_256998_105" "Sample_ID" 
nrow(genedf) == nrow(largedf) # should be TRUE


# look for missing names
setdiff(genedf$names, largedf$names) # should be character(0)


# Import Site Data --------------------------------------------------------

sites14 <- read.csv("~/Documents/Philippines/Surveys_2014/output/Collections2014_2016-07-20.csv", header = T)

# Change ID to 3 digits (leading zeros)
sites14$ID <- formatC(sites14$ID, width = 3, format = "d", flag = "0")

# Generate the Pinsky format sample ID
sites14$Sample_ID <- paste(sites14$Spp, "14_", sites14$ID, sep = "")
sites14$Sample_ID[1:5]
# Merge into the sample data
metasite <- merge(largedf, sites14[,c("Name", "Sample_ID")], by = "Sample_ID", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
nrow(genedf) == nrow(metasite) # should be TRUE

metasite$Site <- NA

# move the data from Name to Site
for (i in 1:nrow(metasite)){
  if(!is.na(metasite$Name[i])){
    metasite$Site[i] <- metasite$Name[i] 
  }
}
p <- length(which(!is.na(metasite$Site)))
p
metasite$Name <- NULL
metasite$Site

sites15 <- read.csv("~/Documents/Philippines/Surveys_2015_05/output/Collections2015_05_2016-06-11.csv", header = T)

# Change ID to 3 digits (leading zeros)
sites15$ID <- formatC(sites15$ID, width = 3, format = "d", flag = "0")

# Generate the Pinsky format sample ID
sites15$Sample_ID <- paste(sites15$Spp, "15_", sites15$ID, sep = "")
sites15$Sample_ID[1:5]
# Merge into the sample data
metasite <- merge(metasite, sites15[,c("Name", "Sample_ID")], by.x = "Sample_ID", by.y = "Sample_ID", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
nrow(genedf) == nrow(metasite) # should be TRUE

# move the data from Name to Site
for (i in 1:nrow(metasite)){
  if(!is.na(metasite$Name[i])){
    metasite$Site[i] <- metasite$Name[i] 
  }
}
q <- length(which(!is.na(metasite$Site)))
q
q > p
metasite$Name <- NULL

sites13 <- read.csv("~/Documents/Philippines/Surveys_2013/output/Collections2013_2016-07-20.csv", header = T)

# Change ID to 3 digits (leading zeros)
sites13$ID <- formatC(sites13$ID, width = 3, format = "d", flag = "0")

# Generate the Pinsky format sample ID
sites13$Sample_ID <- paste(sites13$Spp, "13_", sites13$ID, sep = "")
sites13$Sample_ID[1:5]
# Merge into the sample data
metasite <- merge(metasite, sites13[,c("Name", "Sample_ID")], by.x = "Sample_ID", by.y = "Sample_ID", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
nrow(genedf) == nrow(metasite) # should be TRUE

# move the data from Name to Site
for (i in 1:nrow(metasite)){
  if(!is.na(metasite$Name[i])){
    metasite$Site[i] <- metasite$Name[i] 
  }
}
r <- length(which(!is.na(metasite$Site)))
r
r > q
metasite$Name <- NULL 

sites12 <- read.csv("~/Documents/Philippines/Surveys_2012/output/Collections2012_2016-07-20.csv", header = T)

# Change ID to 3 digits (leading zeros)
sites12$ID <- formatC(sites12$ID, width = 3, format = "d", flag = "0")

# Generate the Pinsky format sample ID
sites12$Sample_ID <- paste(sites12$Spp, "12_", sites12$ID, sep = "")
sites12$Sample_ID[1:5]
# Merge into the sample data
metasite <- merge(metasite, sites12[,c("Name", "Sample_ID")], by.x = "Sample_ID", by.y = "Sample_ID", all.x = T)
# TEST - check the last 2 column names and that the number of rows hasn't changed
nrow(genedf) == nrow(metasite) # should be TRUE

# move the data from Name to Site
for (i in 1:nrow(metasite)){
  if(!is.na(metasite$Name[i])){
    metasite$Site[i] <- metasite$Name[i] 
  }
}
s <- length(which(!is.na(metasite$Site)))
s
s > r
metasite$Name <- NULL 



write.csv(metasite, file= paste(Sys.Date(), '_AllInputMeta.csv', sep=''))
