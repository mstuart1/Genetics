source("../../myRcode/Laboratory/R/findsampleid.R")
source("../../myRcode/Laboratory/R/findlabwork.R")
# if not starting immediately from identity_analysis.R then load the data
idcsv <- read.csv("data/2016-11-28_idanalyis.csv", stringsAsFactors = F)

reads <- read.csv("data/APCL_read_data.csv", stringsAsFactors = F)

# Load the issues list
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# 93 known issues
iss <- leyte %>% tbl("known_issues") %>% collect()

# issue under examination
lig <- "L0936"

sample <- findsample(lig)
lab <- findlab(sample$sample_id)
# lab <- findlab("APCL14_556")

print(iss$Issue[which(iss$Ligation_ID == lig)])

# does the regenotype match the original
X <- lab$ligation_id[1]
print(paste(lab$ligation_id[1], "matches with", idcsv$Second.ID[which(idcsv$First.ID == X)], idcsv$First.ID[which(idcsv$Second.ID == X)]))

Y <- lab$ligation_id[2]
print(paste(lab$ligation_id[2], "matches with", idcsv$Second.ID[which(idcsv$First.ID == Y)], idcsv$First.ID[which(idcsv$Second.ID == Y)]))

Z <- lab$ligation_id[3]
print(paste(lab$ligation_id[3], "matches with", idcsv$Second.ID[which(idcsv$First.ID == Z)], idcsv$First.ID[which(idcsv$Second.ID == Z)]))

A <- lab$ligation_id[4]
print(paste(lab$ligation_id[4], "matches with", idcsv$Second.ID[which(idcsv$First.ID == A)], idcsv$First.ID[which(idcsv$Second.ID == A)]))

B <- lab$ligation_id[5]
print(paste(lab$ligation_id[5], "matches with", idcsv$Second.ID[which(idcsv$First.ID == B)], idcsv$First.ID[which(idcsv$Second.ID == B)]))

reads$retained[which(reads$ligation_id == X)]
reads$retained[which(reads$ligation_id == Y)]
reads$retained[which(reads$ligation_id == Z)]
reads$retained[which(reads$ligation_id == A)]
reads$retained[which(reads$ligation_id == B)]

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
anem2 <- 226
oanem <- leyte %>% tbl("anemones") %>% filter(anem_id == anem2) %>% select(anem_table_id) %>% as.list(collect())
print(oanem)
ati <- 2363
atable2 <- leyte %>% tbl("clownfish") %>% filter(anem_table_id == ati) %>% select(sample_id, size, col) %>% collect()
print(atable2)

# pull labwork plate locations
extract <- lab$extraction_id
digest <- lab$digest_id
ligation <- lab$ligation_id

for (i in length(extract)){
  print(labor %>% tbl("extraction") %>% filter(extraction_id %in% extract) %>% select(extraction_id, date, well, plate))
}
for (i in length(digest)){
  print(labor %>% tbl("digest") %>% filter(digest_id %in% digest) %>% select(digest_id, date, well, plate))
}
for (i in length(ligation)){
  print(labor %>% tbl("ligation") %>% filter(ligation_id %in% ligation) %>% select(ligation_id, date))
}

### Examine extract plates for evidence of mislabeling ###

# find the list of fish caught at a given site in a given year
dives <- leyte %>% tbl("diveinfo") %>% filter(date %like% "2014%") %>% filter(name %like% "Palanas") %>% collect()
anems <- leyte %>% tbl("anemones") %>% filter(dive_table_id %in% dives$id) %>% collect()
fish <- leyte %>% tbl("clownfish") %>% filter(anem_table_id %in% anems$anem_table_id) %>% collect()
fish <- fish[!is.na(fish$sample_id),]

# What date was our sample in question extracted on?
date <- labor %>% tbl("extraction") %>% filter(sample_id == "APCL14_384") %>% select(date, plate) %>% collect()

# were any samples from the site above extracted on that date?
match <- labor %>% tbl("extraction") %>% filter(sample_id %in% fish$sample_id) %>% filter(date == date$date) %>% filter(plate == date$plate) %>% collect()


# plot a histogram of distances
dat <- read.csv("data/2016-11-28_idanalyis.csv", as.is = T)
hist(dat$distkm, breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.5, 2, 2.5, 25, 30))
