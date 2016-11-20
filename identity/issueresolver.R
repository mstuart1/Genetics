# Open the csv from identity analysis and look for resolutions to issues

dat <- read.csv("data/2016-11-18_idanalyis.csv", stringsAsFactors = F)
dat647 <- read.csv("data/2016-11-20_idanalyis.csv", stringsAsFactors = F)
dat2 <- read.csv("data/seq17_03_ID.csv")
dat2_647 <- read.csv("data/seq17_03_ID_647.csv")
# z <- nrow(dat) #166

# there are fewer matches in the July set with fewer loci than in the November set.

# compare which loci are present in the 2 data sets
dat1 <- read.table("data/seq17_03_ID_647.txt", skip = 27, fill = T)
dat2 <- read.table("data/seq03-16_20160718_ID.txt", skip = 28, fill = T)

dat1 <- dat1[2:1039, 1:2]
dat2 <- dat2[1:809, ]

dat2[,2] <- factor(dat2[,2], levels=levels(dat1[,2]))

same <- list()
# setdiff(dat1$V2, dat2$V2)
for (i in 1:nrow(dat2)){
  same <- c(same, dat1$V2[which(dat1$V2 != dat2$V2)])
}

# # find regenotyped samples
# regeno <- dat[dat$First.sample_id == dat$Second.sample_id, ]
# y <- nrow(regeno) #48

# # remove regenotyped samples from dat
# dat <- dat[dat$First.sample_id != dat$Second.sample_id, ]
# nrow(dat) == z-y

# find the samples that are on the known issues list
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

iss <- leyte %>% tbl("known_issues") %>% collect()

issue1 <- data.frame()
issue2 <- data.frame()
for (i in 1:nrow(iss)){
  issue1 <- rbind(issue1, dat[dat$First.ID == iss$Ligation_ID[i],])
}
iss <- anti_join(iss, issue1, by = c("Ligation_ID" = "First.ID"))

for (i in 1:nrow(iss)){
  issue2 <- rbind(issue2, dat[dat$Second.ID == iss$Ligation_ID[i],])
}
iss <- anti_join(iss, issue2, by = c("Ligation_ID" = "Second.ID"))


issue <- rbind(issue1, issue2)
rm(issue1, issue2)

# cut down to just essential columns
issue <- issue[ , c("First.sample_id", "Second.sample_id", "First.ID", "Second.ID")]

# restore iss to full info
iss <- leyte %>% tbl("known_issues") %>% collect()

# attach notes to iss
iss$notes <- NA

iss$notes[iss$Ligation_ID == "L0370"] <- "APCL13_351 still matched to APCL14_445 when 445 was regenotyped from a new digest, APCL13_351 redigest was unsuccessful, not sequenced"
iss$notes[iss$Ligation_ID == "L0377"] <- "APCL13_362 still matched to APCL14_555 when 555 was regenotyped from a new digest, APCL13_351 redigest was unsuccessful, not sequenced"
iss$notes[iss$Ligation_ID == "L0288"] <- "APCL13_255 redigest was unsuccessful, not sequenced"
iss$notes[iss$Ligation_ID == "L0413"] <- "APCL13_040 redigest was unsuccessful, not sequenced"
iss$notes[iss$Ligation_ID == "L0415"] <- "L0415 matched to L2935 in the July 18, 2016 identity analysis, but did not match to it in the 11-18-2016 analysis because the matching loci threshold went up"

