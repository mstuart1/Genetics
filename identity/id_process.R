# this script takes the output csv of identity analysis and creates a table to track fish over time.
source("../code/conleyte.R")

# Connect to database
leyte <- conleyte()

# Initial Setup -----------------------------------------------------------


# import the results of the identity_analysis script used to remove recaptured fish from parentage
filename <- "data/2016-12-20_idanalyis.csv"

idcsv <- read.csv(filename, stringsAsFactors = F)

# strip down to sample_id and field data
idsimp <- idcsv[ , c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.date", "First.name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.date", "Second.name",  "Second.lat", "Second.lon", "First.ID", "Second.ID")]

# pull anem_id from database
suppressWarnings(c1 <- leyte %>% tbl("anemones") %>% select(anem_table_id, anem_id, old_anem_id) %>% collect()) 

# keep only anem ids that match to our id analysis
idsimp <- left_join(idsimp, c1, by = c("First.anem_table_id" = "anem_table_id"))
colnames(idsimp) <- c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon", "First.ID", "Second.ID", "first_anem_id", "first_old_anem_id")

idsimp <- left_join(idsimp, c1, by = c("Second.anem_table_id" = "anem_table_id"))
colnames(idsimp) <- c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon", "First.ID", "Second.ID", "first_anem_id", "first_old_anem_id", "second_anem_id", "second_old_anem_id")

rm(c1)

# add row numbers 
idsimp$fish <- 1:nrow(idsimp)

# re-order columns
idsimp <- idsimp[ , c("fish", "First.sample_id", "Second.sample_id","First.Size", "Second.Size","First.Date","Second.Date","first_anem_id","second_anem_id","first_old_anem_id",  "second_old_anem_id",  "First.Name",   "Second.Name", "First.anem_table_id", "First.fish_table_id",  "First.dive_table_id", "First.ObsTime",  "First.id", "First.lat",  "First.lon", "Second.anem_table_id", "Second.fish_table_id",   "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.lat", "Second.lon")]  

# widen the table to find fish that were caught in more than 2 years

idsimp$sample12 <- NA
 idsimp$sample13 <- NA
 idsimp$sample14 <- NA
 idsimp$sample15 <- NA
 idsimp$sample16 <- NA
 idsimp$size12 <- NA
 idsimp$anem12 <- NA
 idsimp$site12 <- NA
 idsimp$date12 <- NA


for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "12" & is.na(idsimp$sample12[i])){
    idsimp$sample12[i] <- idsimp$First.sample_id[i]
    idsimp$size12[i] <- idsimp$First.Size[i]
    idsimp$anem12[i] <- idsimp$first_anem_id[i]
    idsimp$site12[i] <- idsimp$First.Name[i]
    idsimp$date12[i] <- idsimp$First.Date[i]
  }
  if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "12" & is.na(idsimp$sample12[i])){
    idsimp$sample12[i] <- idsimp$Second.sample_id[i]
    idsimp$size12[i] <- idsimp$Second.Size[i]
    idsimp$anem12[i] <- idsimp$second_anem_id[i]
    idsimp$site12[i] <- idsimp$Second.Name[i]
    idsimp$date12[i] <- idsimp$Second.Date[i]
  }
}

 idsimp$size13 <- NA
 idsimp$anem13 <- NA
 idsimp$site13 <- NA
 idsimp$date13 <- NA
 for (i in 1:nrow(idsimp)){
    if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "13" & is.na(idsimp$sample13[i])){
      idsimp$sample13[i] <- idsimp$First.sample_id[i]
      idsimp$size13[i] <- idsimp$First.Size[i]
      idsimp$anem13[i] <- idsimp$first_anem_id[i]
      idsimp$site13[i] <- idsimp$First.Name[i]
      idsimp$date13[i] <- idsimp$First.Date[i]
    }
    if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "13" & is.na(idsimp$sample13[i])){
      idsimp$sample13[i] <- idsimp$Second.sample_id[i]
      idsimp$size13[i] <- idsimp$Second.Size[i]
      idsimp$anem13[i] <- idsimp$second_anem_id[i]
      idsimp$site13[i] <- idsimp$Second.Name[i]
      idsimp$date13[i] <- idsimp$Second.Date[i]
    }
}

 idsimp$site14 <- NA
 idsimp$date14 <- NA
 idsimp$anem14 <- NA
 idsimp$size14 <- NA
 
 for (i in 1:nrow(idsimp)){
   if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "14" & is.na(idsimp$sample14[i])){
     idsimp$sample14[i] <- idsimp$First.sample_id[i]
     idsimp$size14[i] <- idsimp$First.Size[i]
     idsimp$anem14[i] <- idsimp$first_anem_id[i]
     idsimp$site14[i] <- idsimp$First.Name[i]
     idsimp$date14[i] <- idsimp$First.Date[i]
   }
   if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "14" & is.na(idsimp$sample14[i])){
     idsimp$sample14[i] <- idsimp$Second.sample_id[i]
     idsimp$size14[i] <- idsimp$Second.Size[i]
     idsimp$anem14[i] <- idsimp$second_anem_id[i]
     idsimp$site14[i] <- idsimp$Second.Name[i]
     idsimp$date14[i] <- idsimp$Second.Date[i]
   }
 }
 
 idsimp$site15 <- NA
 idsimp$date15 <- NA
 idsimp$anem15 <- NA
 idsimp$size15 <- NA
 
 for (i in 1:nrow(idsimp)){
   if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "15" & is.na(idsimp$sample15[i])){
     idsimp$sample15[i] <- idsimp$First.sample_id[i]
     idsimp$size15[i] <- idsimp$First.Size[i]
     idsimp$anem15[i] <- idsimp$first_anem_id[i]
     idsimp$site15[i] <- idsimp$First.Name[i]
     idsimp$date15[i] <- idsimp$First.Date[i]
   }
   if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "15" & is.na(idsimp$sample15[i])){
     idsimp$sample15[i] <- idsimp$Second.sample_id[i]
     idsimp$size15[i] <- idsimp$Second.Size[i]
     idsimp$anem15[i] <- idsimp$second_anem_id[i]
     idsimp$site15[i] <- idsimp$Second.Name[i]
     idsimp$date15[i] <- idsimp$Second.Date[i]
   }
 }
 
 idsimp$site16 <- NA
 idsimp$date16 <- NA
 idsimp$anem16 <- NA
 idsimp$size16 <- NA
 
 for (i in 1:nrow(idsimp)){
   if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "16" & is.na(idsimp$sample16[i])){
     idsimp$sample16[i] <- idsimp$First.sample_id[i]
     idsimp$size16[i] <- idsimp$First.Size[i]
     idsimp$anem16[i] <- idsimp$first_anem_id[i]
     idsimp$site16[i] <- idsimp$First.Name[i]
     idsimp$date16[i] <- idsimp$First.Date[i]
   }
   if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "16" & is.na(idsimp$sample16[i])){
     idsimp$sample16[i] <- idsimp$Second.sample_id[i]
     idsimp$size16[i] <- idsimp$Second.Size[i]
     idsimp$anem16[i] <- idsimp$second_anem_id[i]
     idsimp$site16[i] <- idsimp$Second.Name[i]
     idsimp$date16[i] <- idsimp$Second.Date[i]
   }
 }
 

# group columns by year
 twel <- grep("12", colnames(idsimp))
 thirt <- grep("13", colnames(idsimp)) 
 four <- grep("14", colnames(idsimp))
fift <-  grep("15", colnames(idsimp))
sixt <-  grep("16", colnames(idsimp))
 
wide <- idsimp[ , c(twel, thirt, four, fift, sixt)]
wide$fish <- idsimp$fish
 
twel <- grep("12", colnames(wide))
thirt <- grep("13", colnames(wide)) 
four <- grep("14", colnames(wide))
fift <-  grep("15", colnames(wide))
sixt <-  grep("16", colnames(wide))

names(wide)
wide <- wide[,c("sample12", "sample13", "sample14", "sample15", "sample16")]

# flatten multiples (using na.rm keeps the data and omits NAs)
twelve <- aggregate(x=wide, by=list(name=wide$sample12), min, na.rm = T)
thirteen <- aggregate(x=wide, by=list(name=wide$sample13), min, na.rm = T)
fourteen <- aggregate(x=wide, by=list(name=wide$sample14), min, na.rm = T)
fifteen <- aggregate(x=wide, by=list(name=wide$sample15), min, na.rm = T)
sixteen <- aggregate(x=wide, by=list(name=wide$sample16), min, na.rm = T)


whole <- rbind(twelve, thirteen, fourteen, fifteen, sixteen)
whole$name <- NULL

suppressMessages(library(dplyr))
whole <- distinct(whole)

# add an id# to each row
whole$fish_num <- 1:nrow(whole)

# # save wide and idsimp for later
# write.csv(wide, file = paste("data/", Sys.Date(), "wide", sep = ""), row.names = F)
# write.csv(idsimp, file = paste("data/", Sys.Date(),"idsimp.csv", sep = ""), row.names = F)
# idsimp <- read.csv("data/2016-11-03idsimp.csv", stringsAsFactors = F)




# make long again

# pull out each set of data by year
twelve <- whole[!is.na(whole$sample12), ]
twelve <- twelve[, c("sample12", "fish_num")]
colnames(twelve) <- c("sample_id", "capid")

thirteen <- whole[!is.na(whole$sample13), ]
thirteen <- thirteen[, c("sample13", "fish_num")]
colnames(thirteen) <- c("sample_id", "capid")

fourteen <- whole[!is.na(whole$sample14), ]
fourteen <- fourteen[, c("sample14", "fish_num")]
colnames(fourteen) <- c("sample_id", "capid")

fifteen <- whole[!is.na(whole$sample15), ]
fifteen <- fifteen[, c("sample15", "fish_num")]
colnames(fifteen) <- c("sample_id", "capid")

sixteen <- whole[!is.na(whole$sample16), ]
sixteen <- sixteen[, c("sample16", "fish_num")]
colnames(sixteen) <- c("sample_id", "capid")

long <- rbind(twelve, thirteen, fourteen, fifteen, sixteen)


# long is a table with a fish id number for each fish, which is repeated every time that fish was caught

# save for later
# write.csv(long, file = paste("data/", Sys.Date(), "long.csv", sep = ""), row.names = F)
# long <- read.csv("data/2016-11-03long.csv", stringsAsFactors = F)

#### add recapture ID to the fish database ####

# connect to database
leyte <- conleyte()

# pull in all existing fish data
fish <- leyte %>% tbl("clownfish") %>% collect()

# make a backup in case something goes wrong
write.csv(fish, file = paste("data/", Sys.time(), "_fishbackup.csv", sep = ""))

# match wells from plates above to wells in database
fish$capid <- ifelse(is.na(fish$capid), long$capid[match(fish$sample_id, long$sample_id)], fish$capid)
  