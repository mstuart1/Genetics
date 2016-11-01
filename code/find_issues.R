# this script is written to find the known issues ligations and match up the new ligation numbers

# connect to databases
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

# pull in known issues table
iss <- leyte %>% tbl("known_issues") %>% collect()

# attach sample_ids to known issues
# Add sample IDs ----------------------------------------------------------
suppressWarnings(c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id))
suppressWarnings(c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id))
c3 <- left_join(c2, c1, by = "extraction_id")
suppressWarnings(c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id))
c5 <- left_join(c4, c3, by = "digest_id") %>% collect()
c5 <- subset(c5, !is.na("sample_id"), select = c(ligation_id, sample_id)) 

# cleanup
rm(c1, c2, c3, c4)

iss <- left_join(iss, c5, by = c("Ligation_ID" = "ligation_id"))

# add recently ligated samples
recent <- labor %>% tbl("ligation") %>% select(ligation_id, date) %>% filter(date == "2016-09-21") %>% collect()

# add sample ids to recent ligations
suppressWarnings(c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id))
suppressWarnings(c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id))
c3 <- left_join(c2, c1, by = "extraction_id")
suppressWarnings(c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id))
c5 <- left_join(c4, c3, by = "digest_id") %>% collect()
c5 <- subset(c5, !is.na("sample_id"), select = c(ligation_id, sample_id)) 

# cleanup
rm(c1, c2, c3, c4)

recent <- left_join(recent, c5, by = "ligation_id")

# rename so that ligation ids aren't overwritten
colnames(recent) <- c("lig", "date", "sample_id")

# attach recent ids to issue table
iss <- left_join(iss, recent, by = "sample_id")

write.csv(iss, file = "data/issues_regeno.csv", row.names = F)
# iss <- read.csv("data/issues_regeno.csv", stringsAsFactors = F)

# find ligs that were regenotyped
ligs <- iss[!is.na(iss$lig), ]

# find read counts for those ligs
reads <- dplyr::left_join(ligs, old, by = c("lig" = "ligation_id"))

# only one ligation didn't have enough reads - L3008
reads <- reads[which(reads$lig != "L3008"), ]

write.csv(reads, file = "data/regenotyped_in_SEQ17.csv", row.names = F)

# remove duplicate rows
reads[38, ] <- NA



# this block doesn't work
# # pull the list of samples with regeno marked as Y from the database
# need_regeno <- sql("select * from ligation where regeno like 'Y';") 
# need_regeno <- collect(need_regeno)


