# # analyze low performers, 
# see if they have been genotyped in the past or 
# if they need to be regenotyped, 
# update database

data <- read.csv("data/low_performers.csv")

# find sample IDs
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
 

# Merge the two dataframes so that lig IDs match up -----------------------

data <- left_join(data, c5, by ="ligation_id")

# for each sample ID, what are the digest IDs and possible ligation ids
# for testing i <- 1
# create empty dataframe
regenos <- data.frame()

for (i in 1:nrow(data)){
  a <- c5[which(data$sample_id[i] == c5$sample_id), ]
  regenos <- rbind(regenos, a)
}


# which sample IDs are only on the list once?
# test i <- 1
occ <- NA
samples <- unique(regenos$sample_id)
count <- data.frame(samples, occ)
for (i in 1:length(samples)){
  count$occ[i] <- length(grep(samples[i], regenos$sample_id))
}

# for all of the samples that were regenotyped more than once, did one pass dDocent?

source("code/readGenepop_space.R")
genfile <- "data/seq03_16_DP10g95maf40.genepop"
genedf <- readGenepop(genfile)
genedf$lig <- substr(genedf$names,11,15)

# test i <- 2
regenos$test <- NA
for (i in 1:nrow(genedf)){
  regenos$test[which(regenos$ligation_id == genedf$lig[i])] <- "PASS"
}


# test i <- 1
count$ok <- NA
for (i in 1:nrow(count)){
  if (count$occ[i] > 1){
    a <- regenos %>%
      filter(sample_id == count$samples[i] & test == "PASS") %>%
      select(test)
    count$ok[i] <- "PASS" %in% a$test
  }   
  
}


