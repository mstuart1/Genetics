# This script is written to take the filtered genepop file from dDocent and 1) strip any named samples down to pure ligation number, 2) identify and remove re-genotyped samples based on number of loci (SNPs), 3) generate a new genepop file to be fed to cervus for identification of recaptures.

# Set up working directory ---------------------------------------------
source("code/readGenepop_space.R")
source("code/sampforlig.R")
source("~/Documents/Philippines/Phil_code/conleyte.R")
library(tidyverse)




# 1) Read the genepop  - double check genepop to make sur --------

# locate the genepop file and read as data frame
genfile <- "data/seq0217.gen"
genedf <- readGenepop(genfile)

### WAIT ###

# remove the pop column from the data file
genedf$pop <- NULL

# # TEST - make sure the first 2 columns are names and a contig and get number of rows
# names(genedf[,1:2]) # [1] "names" "dDocent_Contig_107_30"
# nrow(genedf) #2111

# # Strip out the ligation ID
# # first have to get all of the ligation ids to be the same length
  for (i in 1:nrow(genedf)){
    if(nchar(genedf$names[i]) == 15){
    genedf$names[i] <- paste("APCL_", substr(genedf$names[i], 11, 15), sep = "")
  }
    if(nchar(genedf$names[i]) == 10){
    genedf$names[i] <- substr(genedf$names[i], 6, 10)
    }
    if(nchar(genedf$names[i]) == 9){
      genedf$names[i] <- substr(genedf$names[i], 5, 9)
    }
    if(nchar(genedf$names[i]) == 8){
      genedf$names[i] <- substr(genedf$names[i], 4, 8)
    }
  }



# TEST are any names still longer than 5 characters?
# which(nchar(genedf$names) > 5) # should be integer(0)

# Add sample IDs ----------------------------------------------------------
c5 <- sampforlig(genedf$names)

# Merge the two dataframes so that lig IDs match up -----------------------

largedf <- left_join(genedf, c5, by = c("names" = "ligation_id"), copy = T)
rm(c5)

# # TEST - check the last 2 column names and that the number of rows hasn't changed
# p <- ncol(largedf)
# names(largedf[,(p-1):p]) # " dDocent_Contig_256998_105" "sample_id"    
# nrow(genedf) == nrow(largedf) # should be TRUE
# # look for missing names
# setdiff(genedf$names, largedf$names) # should be character(0)




# 1431 1432 1433 1434 1435 1436 1437 1438 1439 1440 1441 1442 1443 1444 1445 1972



# Remove samples with known issues ----------------------------------------

# to remove samples with known issues, pull the data from the known issues database


# open the laboratory database to retrieve sample info
# suppressMessages(library(dplyr))
leyte <- conleyte()

iss <- leyte %>% tbl("known_issues") %>% collect()
rm(leyte)

# remove issues from largedf
largedf <- largedf %>%
  filter(!names %in% iss$Ligation_ID)
rm(iss, genedf)

# make sure all of the Ligation ids have sample ids
which(is.na(largedf$sample_id)) # 1972- L3118 has no sample id, is a mixture of samples

largedf <- largedf %>% filter(!is.na(largedf$sample_id))

# # TEST - make sure no more match the list
# j <- largedf %>%
#   filter(names %in% iss$Ligation_ID)
# nrow(j) # should return 0
  

# Remove regenotyped samples ----------------------------------------------

# convert 0000 to NA in the genepop data
largedf[largedf == "0000"] = NA

# # TEST - make sure there are no "0000" left
# which(largedf == "0000") # should return integer(0)

# count the number of loci per individual (have to use for loop)
for(h in 1:nrow(largedf)){
	largedf$numloci[h] <- sum(!is.na(largedf[h,]))
}

### WAIT ###

# # TEST - make sure all of the numloci were populated ----------------------
# which(is.na(largedf$numloci)) # should return integer(0)

# make a list of all of the sample ID's that have duplicates (some on this list occur more than once because there are 3 regenos)
# this line of code keeps any sample_id that comes up as TRUE for being duplicated
regenod <- largedf %>%
  filter(duplicated(largedf$sample_id)) %>%
  select(sample_id)

# # TEST - make sure a list was generated
k <- nrow(regenod)
k # 189


largedf$drop <- NA # place holder
#run through all of the SampleIDs that are found more than once and keep the one with the most loci
# for testing b <- 1
for(b in 1:k){
  # regeno_drop is the line number from largedf that matches an ID in the regeno_match list
  regeno_drop <- which(largedf$sample_id == regenod[b,]) 
  # df is the data frame that holds all of the regenotyped versions of the sample, pulled from largedf
  df <- largedf[regeno_drop, ]  
	# convert the drop column of large df to not na
	largedf$drop[regeno_drop[which.max(df$numloci)]] <- "KEEP"
	
	# find the row numbers of largedf that need to be dropped
	# test e <- 2
	for(e in 1:nrow(df)){
	if(is.na(df$drop[e])){
		f <-regeno_drop[e]
		largedf$drop[f] <- "DROP"
	}
	}
}

# TEST - make sure all of the regenos were dropped ----------------------------
a <- length(which(largedf$drop == "KEEP")) # num keeps
b <- length(which(duplicated(regenod) == TRUE)) # num multiple regenos
a + b == nrow(regenod) # should return TRUE
length(which(largedf$drop == "DROP")) == nrow(regenod) # should be TRUE


# convert all of the KEEPs to NAs 
for(g in 1:nrow(largedf)){
	if(!is.na(largedf$drop[g]) && largedf$drop[g]=="KEEP"){
		largedf$drop[g] <- NA
	}
}

# create a new data frame with none of the "DROP" rows
noregeno <- largedf[is.na(largedf$drop),]
# TEST - make sure no drop rows made it
which(noregeno$drop == "DROP") # should return integer(0)
# TEST - check to see if there are any regenos that were missed
noregeno_match <- noregeno$sample_id[duplicated(noregeno$sample_id)]
noregeno_match # should return character(0)  
# If it doesn't, look deeper: noregeno[which(noregeno$SampleID == "APCL15_403"),], largedf[which(largedf$sample_ID == "APCL15_403"),]

# remove the extra columns from noregeno
noregeno [,c("extraction_ID")] <- NULL
noregeno [,c("digest_ID")] <- NULL
noregeno [,c("numloci")] <- NULL
noregeno [,c("drop")] <- NULL

# convert all the NA genotypes to 0000
noregeno[is.na(noregeno)] = "0000"
# TEST - make sure there are no NA's left
which(is.na(noregeno)) # should return integer(0)

# TEST - compare the length of noregeno to the length of largedf
nrow(noregeno) == nrow(largedf) - k # 1569/1531 - should return TRUE


# remove known recaptures - only do this if you are ablsolutely sure you do not want to find new recapture events with this data
leyte <- conleyte()
recap <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid)) %>% select(sample_id, capid) %>% collect()


########################################################################
# # TEST - make sure a list was generated
k <- nrow(recap)
k # 277


noregeno$drop <- NA # place holder
#run through all of the SampleIDs that are found more than once and keep the one with the most loci
# for testing b <- 1
for(i in 1:max(recap$capid)){
  # recap_drop is the line number from noregeno that matches an ID in the regeno_match list
  X <- recap$sample_id[recap$capid == recap$capid[i]]
  recap_drop <- which(noregeno$sample_id %in% X)
  # df is the data frame that holds all of the regenotyped versions of the sample, pulled from noregeno
  df <- noregeno[recap_drop, ]  
  # the row number of df with the largest number of loci (p-1 indicates the column)
  keep <- which.max(df$numloci) 
  # convert the df number to the row number of large df
  c <- recap_drop[keep]
  # convert the drop column of the row to keep to not na
  df$drop[keep] <- "KEEP"
  # convert the drop column of large df to not na
  noregeno$drop[c] <- "KEEP"
  
  # find the row numbers of noregeno that need to be dropped
  # test e <- 2
  for(e in 1:nrow(df)){
    if(is.na(df$drop[e])){
      f <-recap_drop[e]
      noregeno$drop[f] <- "DROP"
    }
  }
}

# convert all of the KEEPs to NAs 
for(g in 1:nrow(noregeno)){
  if(!is.na(noregeno$drop[g]) && noregeno$drop[g]=="KEEP"){
    noregeno$drop[g] <- NA
  }
}

# create a new data frame with none of the "DROP" rows
noregeno <- noregeno[is.na(noregeno$drop),]
# TEST - make sure no drop rows made it
which(noregeno$drop == "DROP") # should return integer(0)
# TEST - check to see if there are any regenos that were missed
noregeno_match <- noregeno$sample_id[duplicated(noregeno$sample_id)]
noregeno_match # should return character(0)  
# If it doesn't, look deeper: noregeno[which(noregeno$SampleID == "APCL15_403"),], largedf[which(largedf$sample_ID == "APCL15_403"),]

# remove the extra columns from noregeno
noregeno [,c("extraction_ID")] <- NULL
noregeno [,c("digest_ID")] <- NULL
noregeno [,c("numloci")] <- NULL
noregeno [,c("drop")] <- NULL

# convert all the NA genotypes to 0000
noregeno[is.na(noregeno)] = "0000"
# TEST - make sure there are no NA's left
which(is.na(noregeno)) # should return integer(0)

# TEST - compare the length of noregeno to the length of largedf
nrow(noregeno) == nrow(largedf) - k # 1569/1531 - should return TRUE



# 4) Output genepop file --------------------------------------------------

# Build the genepop components
msg <- c("This genepop file was generated using a script called process_genepop.R written by Michelle Stuart with help from Malin Pinsky")

loci <- paste(names(noregeno[,2:ncol(noregeno)]), collapse =",")

gene <- vector()
sample <- vector()
for (i in 1:nrow(noregeno)){
		gene[i] <- paste(noregeno[i,2:ncol(noregeno)], collapse = " ")
	sample[i] <- paste(noregeno[i,1], gene[i], sep = ", ")
}

out <- c(msg, loci, 'pop', sample)

write.table(out, file = paste("data/",Sys.Date(), '_noregeno.gen', sep = ""), row.names=FALSE, quote=FALSE, col.names=FALSE) # won't let me use header=FALSE - use col.names instead of header


