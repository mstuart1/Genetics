# This script evaluates the output of a cervus identity analysis and flags "true matches" versus "false positives"


# Set up workspace --------------------------------------------------------

# Lightning
setwd('/Users/macair/Documents/Philippines/Genetics/identity')
source("../readGenepop_space.R")

# Import cervus identity results ------------------------------------------

idcsv <- read.csv("allsamples_ID.csv", stringsAsFactors = F)


# Add metadata ------------------------------------------------------------

# googlesheets::gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1Rf_dFJ5WK-vTTsIT_kHHOcFrKzQtMFtKiuXiFw1lh9Y' # for Sample_Data file
lab <- googlesheets::gs_key(mykey)

### WAIT ###

lig <- googlesheets::gs_read(lab, ws="Ligations")
dig <- googlesheets::gs_read(lab, ws='Digests')
extr <- googlesheets::gs_read(lab, ws="Extractions")
sample <- googlesheets::gs_read(lab, ws="Samples")

# create a table for the First half of the match
lig1 <- lig
names(lig1) <- paste('First.', names(lig1), sep='')

# merge the two dataframes so that lig IDs match up
match <- merge(idcsv, lig1[,c('First.Ligation_ID', 'First.Digest_ID')], by.x = "First.ID", by.y = "First.Ligation_ID", all.x = T)

# create a table for the Second half of the match
lig2 <- lig
names(lig2) <- paste('Second.', names(lig2), sep='')

# merge the two dataframes so that lig IDs match up
match <- merge(match, lig2[,c('Second.Ligation_ID', 'Second.Digest_ID')], by.x = "Second.ID", by.y = "Second.Ligation_ID", all.x = T)

# clean up
rm(lig, lig1, lig2)

# create a table for the First half of the match
dig1 <- dig
names(dig1) <- paste('First.', names(dig1), sep='')

# merge the two dataframes so that dig IDs match up
match <- merge(match, dig1[,c('First.Digest', 'First.Extraction_ID')], by.x = "First.Digest_ID", by.y = "First.Digest", all.x = T)

# create a table for the Second half of the match
dig2 <- dig
names(dig2) <- paste('Second.', names(dig2), sep='')

# merge the two dataframes so that dig IDs match up
match <- merge(match, dig2[,c('Second.Digest', 'Second.Extraction_ID')], by.x = "Second.Digest_ID", by.y = "Second.Digest", all.x = T)

# clean up
rm(dig, dig1, dig2)

# create a table for the First half of the match
extr1 <- extr
names(extr1) <- paste('First.', names(extr1), sep='')

# merge the two dataframes so that dig IDs match up
match <- merge(match, extr1[,c('First.Extract', 'First.Sample_ID')], by.x = "First.Extraction_ID", by.y = "First.Extract", all.x = T)

# create a table for the Second half of the match
extr2 <- extr
names(extr2) <- paste('Second.', names(extr2), sep='')

# merge the two dataframes so that dig IDs match up
match <- merge(match, extr2[,c('Second.Extract', 'Second.Sample_ID')], by.x = "Second.Extraction_ID", by.y = "Second.Extract", all.x = T)

# clean up
rm(extr, extr1, extr2)

# add field data
sample1 <- sample
names(sample1) <- paste('First.', names(sample1), sep='')

match <- merge(match, sample1[,c("First.Sample_ID", "First.Lat", "First.Lon", "First.Date", "First.Size")], by.x = "First.Sample_ID", by.y = "First.Sample_ID", all.x = T)

# add field data for second half of match
sample2 <- sample
names(sample2) <- paste('Second.', names(sample2), sep='')

match <- merge(match, sample2[,c("Second.Sample_ID", "Second.Lat", "Second.Lon", "Second.Date", "Second.Size")], by.x = "Second.Sample_ID", by.y = "Second.Sample_ID", all.x = T)

# clean up 
rm (sample2, sample1, sample)


# Flag matches with same date of capture ----------------------------------
match$First.Date <- as.Date(match$First.Date, "%m/%d/%Y")
match$Second.Date <- as.Date(match$Second.Date, "%m/%d/%Y")


match$date_eval <- NA
for(i in 1:nrow(match)){
  a <- match$First.Date[i]
  b <- match$Second.Date[i]
  if (a == b & !is.na(a) & !is.na(b)){
    match$date_eval[i] <- "FAIL"
  }
}

### WAIT ### - if you have to wait here, double check the number of obs, there may be a problem with the attachment of metadata



# Flag matches that were caught more than 250m apart ----------------------

library(fields)
# source('greatcircle_funcs.R') # alternative, probably faster
alldists <- rdist.earth(as.matrix(match[,c('First.Lon', 'First.Lat')]), as.matrix(match[,c('Second.Lon', 'Second.Lat')]), miles=FALSE, R=6371) # see http://www.r-bloggers.com/great-circle-distance-calculations-in-r/ # slow because it does ALL pairwise distances, instead of just in order
match$distkm <- diag(alldists)

match$disteval <- NA # placeholder
for(i in 1:nrow(match)){
  if(!is.na(match$distkm[i]) & 0.250 <= match$distkm[i]){
    match$disteval[i] <- "FAIL"
  }
}


# Flag matches where size decreases by more than 1.5cm --------------------

match$size_eval <- NA
for (i in 1:nrow(match)){
  if(!is.na(match$First.Date[i]) & !is.na(match$Second.Date[i])  & match$First.Date[i] < match$Second.Date[i]) {
    if(!is.na(match$First.Size[i]) & !is.na(match$Second.Size[i]) & (match$First.Size[i] - 1.5) > match$Second.Size[i]){
      match$size_eval[i] <- "FAIL"
    }
  }
}
  
for (i in 1:nrow(match)){
  if(!is.na(match$First.Date[i]) & !is.na(match$Second.Date[i])  & match$First.Date[i] > match$Second.Date[i]) {
    if(!is.na(match$First.Size[i]) & (match$First.Size[i] + 1.5) < match$Second.Size[i]){
      match$size_eval[i] <- "FAIL"
    }
  }
}

# TODO - make a list of good matches and a list of bad matches and see where they intersect, then drop bad part of the bad match sample but not the good part.

# Adjust for known exceptions ---------------------------------------------
# These samples will go back onto the match list so the one with the most loci will be kept and the other will be removed

# L0444 or L1626 should've been removed in the regeno phase and wasn't
p <- which(match$First.ID == "L0444")
match$date_eval[p] <- NA

# APCL13_131 and APCL13_120 are the same fish caught twice on the same day (L1092, L1084)
p <- which(match$First.Sample_ID == "APCL13_120")
match$date_eval[p] <- NA

# L0979 and L0980 both regenotype to match APCL13_651L1725 - L0979 should've been removed at regenotype stage but was mislabeled at the time, ligation sheet has now been corrected
p <- which(match$First.ID == "L0979")
match$date_eval[p] <- NA

# L1728 is a regenotype of L0806 - mistake wasn't noted in the spreadsheet and this regenotype was missed earlier - digest sheet (where error occured) has been updated
p <- which(match$First.ID == "L0806")
match$date_eval[p] <- NA


# Write output ------------------------------------------------------------

write.csv(match, file = paste(Sys.Date(), "_idanalyis.csv", sep = ""), row.names = F)


# Open genepop ------------------------------------------------------------

genfile <- ""
genedf <- readGenepop(genfile)

### WAIT ###

genedf[,1] <- NULL # remove the pop column from the data file
# TEST - make sure the first 2 columns are names and a contig and get number of rows
names(genedf[,1:2]) # [1] "names" "dDocent_Contig_107_30"
nrow(genedf) # 1651


# Calculate the number of loci for analysis -------------------------------

# convert 0000 to NA in the genepop data
genedf[genedf == "0000"] = NA
# TEST - make sure there are no "0000" left
which(genedf == "0000") # should return integer(0)

# count the number of loci per individual
for(h in 1:nrow(genedf)){
  genedf$numloci[h] <- sum(!is.na(genedf[h,]))
}
# TEST - make sure all of the numloci were populated
which(is.na(genedf$numloci)) # should return integer(0)


genedf$drop <- NA

# Remove problem samples --------------------------------------------------

for (i in 1:nrow(match)){
  if(!is.na(match$size_eval[i]) & match$size_eval[i] == "FAIL" | !is.na(match$disteval[i]) & match$disteval[i] == "FAIL" | match$date_eval[i] == "FAIL" & !is.na(match$date_eval[i])){
    a <- which(genedf$names == match$First.ID[i])
    b <- which(genedf$names == match$Second.ID[i])
    genedf$drop[a] <- "DROP"
    genedf$drop[b] <- "DROP"
  }
}

# remove the special cases above

# # TEST - compare the ligation IDs that were marked to be dropped from the genepop to the IDs in the match 
# genos <- genedf$names[which(genedf$drop == "DROP")]
# firsts <- match$First.ID[which(match$disteval == "FAIL")]
# secs <- match$Second.ID[which(match$disteval == "FAIL")]
# firdif <- setdiff(genos,firsts)
# secdif <- setdiff(firdif, secs)

# Run through id analysis and compare to determine which to remove --------
for(i in 1:nrow(match)){
  # a & b are  the line numbers from genepop file that matches an the first and second ID in the match table
  a <- which(genedf$names == match$First.ID[i])
  b <- which(genedf$names == match$Second.ID[i])
if (genedf$numloci[a] > genedf$numloci[b]){
  genedf$drop[b] <- "DROP"
} else{
  genedf$drop[a] <- "DROP"
}
}

# Make a dataframe of the samples that will be dropped
drops <- genedf[!is.na(genedf$drop),]

# Make a dataframe of the samples to keep for parentage analysis
keep <- genedf[is.na(genedf$drop),]

keep$numloci <- NULL
keep$drop <- NULL

# TODO -  Look for regenotypes again:

# convert all the NA genotypes to 0000
keep[is.na(keep)] = "0000"
# TEST - make sure there are no NA's left
which(keep == NA) # should return integer(0)

# Write out genepop  ------------------------------------------------------

# Build the genepop components
msg <- c("This genepop file was generated using a script called identity_analysis.R written by Michelle Stuart with help from Malin Pinsky and Ryan Batt")

loci <- paste(names(keep[,2:ncol(keep)]), collapse =",")

gene <- vector()
sample <- vector()
for (i in 1:nrow(keep)){
  gene[i] <- paste(keep[i,2:ncol(keep)], collapse = " ")
  sample[i] <- paste(keep[i,1], gene[i], sep = ", ")
}

  ### WAIT ###

out <- c(msg, loci, 'pop', sample)

write.table(out, file = paste(Sys.Date(), 'norecap.genepop', sep = '_'), row.names=FALSE, quote=FALSE, col.names=FALSE)


