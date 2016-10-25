# The purpose of this code is to create a names file for use in sequence analysis by matching the Pool number to the barcode number and the string of nucleotides used to make that barcode

# Query the database to find out which pools(PCRs) went into that seq ------
# connect to the database
suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

suppressWarnings(ligs <- labor %>% tbl("ligation") %>% select(ligation_id, barcode_num, pool) %>% filter(pool == "P072")) # manuall fill in pool number

barcode <- labor %>% tbl("barcodes")

# match the index number to the nucleotide code
names <- left_join(ligs, barcode, by = "barcode_num") %>% collect()

filename <- as.character(names[1, 3])
names <- names[ , c(2,4)]

# add population to names for dDocent
names$ligation_id <- paste("APCL_", names$ligation_id, sep = "")

# write a tsv of the result - this should be uploaded to amphiprion
write.table(names, file = paste("data/names-", filename, ".tsv", sep = ""), sep = "\t", row.names = F, col.names = F, quote = F)

# cleanup before running next pool
rm(names, filename, ligs)

