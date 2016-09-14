# This script is written to take the filtered genepop file from dDocent and 1) strip any named samples down to pure ligation number, 2) identify and remove re-genotyped samples based on number of loci (SNPs), 3) generate a new genepop file to be fed to cervus for identification of recaptures.
# TODO: change from google docs to database

# Set up working directory ---------------------------------------------

# # Mr. Whitmore
# setwd('/Users/michelle/Google Drive/Pinsky Lab/Cervus/Michelle\'s R codes/Genetics/rosetta stone genepop')
# source('/Users/michelle/Google Drive/Pinsky Lab/Cervus/Michelle\'s R codes/Genetics/code/readGenepop_space.R')

# Lightning
# setwd("~/Documents/Philippines/Genetics/")
# source("code/readGenepop_space.R")
source("data/readGenepop_space.R")


# 1) Strip down to Ligation ID  - double check genepop to make sur --------

# locate the genepop file and read as data frame
genfile <- "data/seq03_16_DP10g95maf40.genepop"
genedf <- readGenepop(genfile)

### WAIT ###

# remove the pop column from the data file
genedf[,1] <- NULL

# TEST - make sure the first 2 columns are names and a contig and get number of rows
names(genedf[,1:2]) # [1] "names" "dDocent_Contig_107_30"
nrow(genedf) # 1651

# Strip out the ligation ID
genedf$lig <- substr(genedf$names,11,15)

# TEST - make sure samples were renamed properly
genedf$lig[1:5] # "L1733" "L2552" "L2553" "L2344" "L2463"

# open the laboratory database to retrieve sample info
suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

# add lab IDs
c1 <- labor %>% tbl("extraction") %>% select(extraction_ID, sample_ID)
c2 <- labor %>% tbl("digest") %>% select(digest_ID, extraction_ID)
c3 <- left_join(c2, c1, by = "extraction_ID")
c4 <- labor %>% tbl("ligation") %>% select(ligation_ID, digest_ID)
c5 <- data.frame(left_join(c4, c3, by = "digest_ID"))

# merge the two dataframes so that lig IDs match up
largedf <- merge(genedf, c5, by.x = "lig", by.y = "ligation_ID", all.x = T)

# TEST - check the last 2 column names and that the number of rows hasn't changed
p <- ncol(largedf)
names(largedf[,(p-1):p]) # "extraction_ID" "sample_ID"
nrow(genedf) == nrow(largedf) # should be TRUE
# look for missing names
setdiff(genedf$names, largedf$names) # should be character(0)

# Remove samples with known issues ----------------------------------------

# to remove samples with known issues, pull the data from the known issues database

# Connect to the database
library(RMySQL)
leyte <- DBI::dbConnect(MySQL(), host="amphiprion.deenr.rutgers.edu", user="michelles", password="larvae168", dbname="Leyte", port=3306)

# See a list of tables in the database
# dbListTables(leyte)

# Read a table into R
iss <- dbSendQuery(leyte, "select * from known_issues")

# iss is now sitting on the mysql server waiting to be retrieved.The n in the function specifies the number of records to retrieve, using n=-1 retrieves all pending records.
iss <- fetch(iss, n=-1) 


# change the lig_ID for known issue samples to NA for easy removal
for (i in 1:nrow(iss)){
  j <- which(iss$Ligation_ID[i] == largedf$lig)
  largedf$lig[j] <- NA
}
# the number of IDs that matched (not necessarily the length of the iss table)
m <- length(which(is.na(largedf$lig))) 

# remove the samples with issues from the noregeno table
inds <- !is.na(largedf$lig)
largedf <- largedf[inds,]

# TEST - make sure no more match the list
for (i in 1:nrow(iss)){
  j <- which(iss$Ligation_ID[i] == largedf$lig)}
j # should return integer(0)
  
# Remove regenotyped samples ----------------------------------------------

# make a list of all of the sample ID's that have duplicates (some on this list occur more than once because there are 3 regenos)
regeno_match <- largedf$sample_ID[duplicated(largedf$sample_ID)]
# TEST - make sure a list was generated
k <- length(regeno_match) # 82
k
##### calculate the number of genotyped loci for each sample #####

# convert 0000 to NA in the genepop data
largedf[largedf == "0000"] = NA
# TEST - make sure there are no "0000" left
which(largedf == "0000") # should return integer(0)

# count the number of loci per individual (have to use for loop)
for(h in 1:nrow(largedf)){
	largedf$numloci[h] <- sum(!is.na(largedf[h,]))
}

### WAIT ###

# TEST - make sure all of the numloci were populated ----------------------
which(is.na(largedf$numloci)) # should return integer(0)

largedf$drop <- NA # place holder
#run through all of the SampleIDs that are found more than once and keep the one with the most loci
for(b in 1:k){
  # regeno_drop is the line number from largedf that matches an ID in the regeno_match list
  regeno_drop <- which(largedf$sample_ID == regeno_match[b]) 
	df <- largedf[c(regeno_drop[1],regeno_drop[2],regeno_drop[3],regeno_drop[4]),]  # df is the data frame that holds all of the regenotyped versions of the sample, pulled from largedf
	p <- ncol(df)
	keep <- which.max(df[,p-1]) # the row number of df with the largest number of loci
	c <- regeno_drop[keep]
	df$drop[keep] <- "KEEP"
	largedf$drop[c] <- "KEEP"
	for(e in 1:nrow(df)){
	if(is.na(df$drop[e])){
		f <-regeno_drop[e]
		largedf$drop[f] <- "DROP"
	}
	}
}


# TEST - make sure all of the regenos were dropped ----------------------------
a <- length(which(largedf$drop == "KEEP")) # num keeps
b <- length(which(duplicated(regeno_match) == TRUE)) # num multiple regenos
a + b == length(regeno_match) # should return TRUE
length(which(largedf$drop == "DROP")) == length(regeno_match) # should be TRUE


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
noregeno_match <- noregeno$sample_ID[duplicated(noregeno$sample_ID)]
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


