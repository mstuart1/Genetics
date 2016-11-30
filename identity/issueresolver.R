source("../../myRcode/Laboratory/R/findsampleid.R")
source("../../myRcode/Laboratory/R/findlabwork.R")
# if not starting immediately from identity_analysis.R then load the data
idcsv <- read.csv("data/2016-11-28_idanalyis.csv", stringsAsFactors = F)

reads <- read.csv("data/APCL_read_data.csv", stringsAsFactors = F)

# Load the issues list
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# 93 known issues
iss <- leyte %>% tbl("known_issues") %>% collect()

# issue under examination
lig <- "L0737"

sample <- findsample(lig)
lab <- findlab(sample$sample_id)

print(iss$Issue[which(iss$Ligation_ID == lig)])

# does the regenotype match the original
X <- lab$ligation_id[1]
print(paste(lab$ligation_id[1], "matches with", idcsv$Second.ID[which(idcsv$First.ID == X)], idcsv$First.ID[which(idcsv$Second.ID == X)]))

Y <- lab$ligation_id[2]
print(paste(lab$ligation_id[2], "matches with", idcsv$Second.ID[which(idcsv$First.ID == Y)], idcsv$First.ID[which(idcsv$Second.ID == Y)]))

reads$retained[which(reads$ligation_id == X)]
reads$retained[which(reads$ligation_id == Y)]

# Where was the fish caught?
site1 <- which(idcsv$Second.ID == lig)
print(paste(sample$sample_id, "was captured at", idcsv$Second.name[site1[1]]))
site2 <- which(idcsv$First.ID == lig)
print(paste(sample$sample_id, "was captured at", idcsv$First.name[site2[1]]))



# which anemone
paste("anem_table_id =",idcsv$First.anem_table_id[which(idcsv$First.ID == lig)], sep = " ")
atable <- idcsv$First.anem_table_id[unique(which(idcsv$First.ID == lig))]
paste("anem_table_id =",idcsv$Second.anem_table_id[which(idcsv$Second.ID == lig)], sep = " ")
atable <- idcsv$Second.anem_table_id[unique(which(idcsv$Second.ID == lig))]

anem <- leyte %>% tbl("anemones") %>% filter(anem_table_id == atable[1]) %>% select(anem_id) %>% collect() 
paste("anem_id =", anem, sep = " ")

# which fish did we see
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id == atable[1]) %>% select(sample_id, size, col) %>% collect() 
print(fish)

# have we been to this anem at other times?
anem <- anem$anem_id
visits <- leyte %>% tbl("anemones") %>% filter(anem_id == anem) %>% select(anem_table_id) %>% collect() 
if (nrow(visits) > 1){
  paste("Also visited", visits, sep = " ")
}else{print("This was the only visit made to this anemone.")}

# other anemones in the area have what fish?
anem2 <- 133
oanem <- leyte %>% tbl("anemones") %>% filter(anem_id == anem2) %>% select(anem_table_id) %>% as.list(collect())
print(oanem)
ati <- 6470
atable2 <- leyte %>% tbl("clownfish") %>% filter(anem_table_id == ati) %>% select(sample_id, size, col) %>% collect()
print(atable2)







# Before 11-28-2016
# ##########################################################################################
# # Open the csv from identity analysis and look for resolutions to issues
# 
# filename <- "data/2016-11-28_idanalyis.csv"
# 
# dat <- read.csv(filename, stringsAsFactors = F)
# 
# # z <- nrow(dat) #366
# 
# 
# # # find regenotyped samples
# # regeno <- dat[dat$First.sample_id == dat$Second.sample_id, ]
# # y <- nrow(regeno) #48
# 
# # # remove regenotyped samples from dat
# # dat <- dat[dat$First.sample_id != dat$Second.sample_id, ]
# # nrow(dat) == z-y
# 
# # find the samples that are on the known issues list
# suppressMessages(library(dplyr))
# leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
# 
# # 93 known issues
# iss <- leyte %>% tbl("known_issues") %>% collect()
# 
# # which samples are in both the dat and iss?
# issue1 <- data.frame()
# issue2 <- data.frame()
# for (i in 1:nrow(iss)){
#   issue1 <- rbind(issue1, dat[dat$First.ID == iss$Ligation_ID[i],])
# }
# iss <- anti_join(iss, issue1, by = c("Ligation_ID" = "First.ID"))
# 
# for (i in 1:nrow(iss)){
#   issue2 <- rbind(issue2, dat[dat$Second.ID == iss$Ligation_ID[i],])
# }
# iss <- anti_join(iss, issue2, by = c("Ligation_ID" = "Second.ID"))
# 
# 
# issue <- rbind(issue1, issue2)
# rm(issue1, issue2)
# 
# # cut down to just essential columns
# issue <- issue[ , c("First.sample_id", "Second.sample_id", "First.ID", "Second.ID")]
# 
# # restore iss to full info
# iss <- leyte %>% tbl("known_issues") %>% collect()
# 
# # attach notes to iss
# iss$notes <- NA
# 
# iss$notes[iss$Ligation_ID == "L0370"] <- "APCL13_351 still matched to APCL14_445 when 445 was regenotyped from a new digest, APCL13_351 redigest was unsuccessful, not sequenced"
# iss$notes[iss$Ligation_ID == "L0377"] <- "APCL13_362 still matched to APCL14_555 when 555 was regenotyped from a new digest, APCL13_351 redigest was unsuccessful, not sequenced"
# iss$notes[iss$Ligation_ID == "L0288"] <- "APCL13_255 redigest was unsuccessful, not sequenced"
# iss$notes[iss$Ligation_ID == "L0413"] <- "APCL13_040 redigest was unsuccessful, not sequenced"
# iss$notes[iss$Ligation_ID == "L0415"] <- "L0415 matched to L2935 in the July 18, 2016 identity analysis, but did not match to it in the 11-18-2016 analysis because the matching loci threshold went up"
# 
