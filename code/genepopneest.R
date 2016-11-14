# This script is written to take a genepop and reduce it to samples contained in only one year for the purpose of examining year by year differences in effective population size with the program Ne Estimator.  

# Going to use a genepop that contains recaptures but does not contain regenotypes

# Prepare the work space
source("code/readGenepop_space.R")
suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# 1) Strip down to Ligation ID  - double check genepop to make sure it isn't already in Lig id format --------

# locate the genepop file and read as data frame
genfile <- "data/2016-09-08_noregeno.genepop"
genedf <- readGenepop(genfile)

### WAIT ###

# remove the pop column from the data file
genedf$pop <- NULL

# # TEST - make sure the first 2 columns are names and a contig and get number of rows
# names(genedf[,1:2]) # [1] "names" "dDocent_Contig_107_30"
# nrow(genedf) # 1651

# Strip out the ligation ID
genedf$lig <- substr(genedf$names,11,15)

# TEST - make sure samples were renamed properly
genedf$lig[1:5] # "L1733" "L2552" "L2553" "L2344" "L2463"

# Connect to database Labor  ----------------------------------------------
# open the laboratory database to retrieve sample info
suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

# Add sample IDs ----------------------------------------------------------
suppressWarnings(c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id))
suppressWarnings(c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id))
c3 <- left_join(c2, c1, by = "extraction_id")
suppressWarnings(c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id))
c5 <- left_join(c4, c3, by = "digest_id") %>% collect()
c5 <- subset(c5, !is.na("sample_id"), select = c(ligation_id, sample_id)) 

# cleanup
rm(c1, c2, c3, c4)

# Merge the two dataframes so that lig IDs match up -----------------------

largedf <- left_join(genedf, c5, by = c("lig" = "ligation_id"), copy = T)

# # TEST - check the last 2 column names and that the number of rows hasn't changed
# p <- ncol(largedf)
# names(largedf[,(p-1):p]) # "lig" "sample_ID"
# nrow(genedf) == nrow(largedf) # should be TRUE
# # look for missing names
# setdiff(genedf$names, largedf$names) # should be character(0)

# Remove samples with known issues ----------------------------------------

# to remove samples with known issues, pull the data from the known issues database

# Connect to database Leyte -----------------------------------------------------
# open the laboratory database to retrieve sample info
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

# Read known issues table into R ------------------------------------------
iss <- leyte %>% tbl("known_issues") %>% collect()

# Remove ligation IDs with issues -----------------------------------------

# create a new table with only the issue samples, keep only lig and issue
iss_lig <- largedf %>%
  filter(lig %in% iss$Ligation_ID) %>%
  select(lig) %>%
  mutate(issue = 1)

m <- nrow(iss_lig) # for testing later

# merge the issue column into the original table
largedf <- left_join(largedf, iss_lig, by = "lig")

# remove all samples with issue = 1
largedf <- largedf[is.na(largedf$issue), ]
largedf$issue <- NULL

# # TEST - make sure no more match the list
# j <- largedf %>%
#   filter(lig %in% iss$Ligation_ID)
# nrow(j) # should return 0
  

# Remove regenotyped samples ----------------------------------------------

# Calculate the number of genotyped loci for each sample ----------------

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
k # 82


largedf$drop <- NA # place holder
#run through all of the SampleIDs that are found more than once and keep the one with the most loci
# for testing b <- 1
for(b in 1:k){
  # regeno_drop is the line number from largedf that matches an ID in the regeno_match list
  regeno_drop <- which(largedf$sample_id == regenod[b,]) 
  # df is the data frame that holds all of the regenotyped versions of the sample, pulled from largedf
  df <- largedf[regeno_drop, ]  
	# the row number of df with the largest number of loci (p-1 indicates the column)
	keep <- which.max(df$numloci) 
	# convert the df number to the row number of large df
	c <- regeno_drop[keep]
	# convert the drop column of the row to keep to not na
	df$drop[keep] <- "KEEP"
	# convert the drop column of large df to not na
	largedf$drop[c] <- "KEEP"
	
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
noregeno [,c("names")] <- NULL
noregeno [,c("sample_ID")] <- NULL
noregeno [,c("numloci")] <- NULL
noregeno [,c("drop")] <- NULL

# convert all the NA genotypes to 0000
noregeno[is.na(noregeno)] = "0000"
# TEST - make sure there are no NA's left
which(noregeno == NA) # should return integer(0)

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

write.table(out, file = paste("data/",Sys.Date(), '_noregeno.genepop', sep = ""), row.names=FALSE, quote=FALSE, col.names=FALSE) # won't let me use header=FALSE - use col.names instead of header


