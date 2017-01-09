# what fish were caught on the same dive as a given fish?
source("../code/conleyte.R")
source("../../myRcode/Laboratory/R/conlabor.R")

leyte <- conleyte()

X <- 1630 # anem_table_id

# find the dive table id for the dive in question
c1 <- leyte %>% tbl("anemones") %>% filter(anem_table_id == X) %>% select(dive_table_id) %>% collect()

# find all of the anem_table_ids for that dive
c2 <- leyte %>% tbl("anemones") %>% filter(dive_table_id == c1$dive_table_id[1]) %>% collect()
l <- list(c2$anem_table_id)
Y <- min(c2$anem_table_id)
Z <- max(c2$anem_table_id)

# find all of the fish with those anem_table_ids
c3 <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% Y:Z) %>% collect()

fish <- c3[!is.na(c3$sample_id), ]

# were any of those fish disgested on the date in question? 2016-04-25
# or ligated on 2016.04.28
labor <- conlabor()
# test <- labor %>% tbl("digest") %>% filter(date == "2016-04-25") %>% collect()
test <- labor %>% tbl("ligation") %>% filter(date == "2016-04-28") %>% collect()

# add Sample IDs
c6 <- labor %>% tbl("digest") %>% select(extraction_id, digest_id)
c7 <- left_join(test, c6, by = "digest_id", copy = T)
c4 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
# c5 <- left_join(test, c4, by = "extraction_id", copy = T) %>% collect()
c5 <- left_join(c7, c4, by = "extraction_id", copy = T) %>% collect()

match <- data.frame()
for (i in 1:nrow(fish)){
  match <- rbind(match, c5[which(c5$sample_id == fish$sample_id[i]), ])
  
}

  