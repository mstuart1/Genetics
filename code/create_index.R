# The purpose of this code is to create an index file for use in sequence analysis by matching the Pool number to the Illumina index code and the string of nucleotides used to make that index

seq <- "SEQ17"  #manually fill in seq number


# Query the database to find out which pools(PCRs) went into that seq ------
# connect to the database
suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

# get the pools contained in this seq
pools <- labor %>% tbl("sequencing") %>% filter(SEQ_ID == "SEQ17") %>% select(contents) %>% collect()

# split pools into a list of 4 values
pools <- strsplit(as.character(pools), ", ")

# find the index number for each pool
pcr <- labor %>% tbl("pcr")  %>% filter(pcr_id %in% pools)
