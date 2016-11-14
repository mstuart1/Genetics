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
# nrow(genedf) # 1482


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

largedf <- left_join(genedf, c5, by = c("names" = "ligation_id"), copy = T)

# # TEST - check the last 2 column names and that the number of rows hasn't changed
# p <- ncol(largedf)
# names(largedf[,(p-1):p]) # "lig" "sample_ID"
# nrow(genedf) == nrow(largedf) # should be TRUE
# # look for missing names
# setdiff(genedf$names, largedf$names) # should be character(0)


# Assign years to each sample
largedf$year <- substr(largedf$sample_id, 5, 6)

# make a genepop for the desired year

geno <- subset(largedf, year == 13)

# remove extra columns from data table
geno$sample_id <- NULL
geno$year <- NULL



# 4) Output genepop file --------------------------------------------------

# Build the genepop components
msg <- c("This genepop file was generated using a script called genepopneest.R written by Michelle Stuart")

loci <- paste(names(geno[,2:ncol(geno)]), collapse =",")

gene <- vector()
sample <- vector()
for (i in 1:nrow(geno)){
		gene[i] <- paste(geno[i,2:ncol(geno)], collapse = " ")
	sample[i] <- paste(geno[i,1], gene[i], sep = ", ")
}

out <- c(msg, loci, 'pop', sample)

write.table(out, file = paste("data/",Sys.Date(), '_13_geno.genepop', sep = ""), row.names=FALSE, quote=FALSE, col.names=FALSE) # won't let me use header=FALSE - use col.names instead of header


