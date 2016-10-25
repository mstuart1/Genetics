# The purpose of this code is to create an index file for use in sequence analysis by matching the Pool number to the Illumina index code and the string of nucleotides used to make that index

seq <- c("SEQ17")  #manually fill in seq number

# Query the database to find out which pools(PCRs) went into that seq ------
# connect to the database
suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

suppressWarnings(index <- labor %>% tbl("pcr") %>% select(index, pool_id) %>% filter(SEQ == seq))

illumina <- labor %>% tbl("illumina")

# match the index number to the nucleotide code
index <- left_join(index, illumina, by = c("index" = "index_num")) %>% collect()

index <- index[ , c(2,4)]

# write a tsv of the result - this should be uploaded to amphiprion
write.table(index, file = "index-seq17.tsv", sep = "\t", row.names = F, col.names = F, quote = F)
