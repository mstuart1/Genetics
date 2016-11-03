# this script takes the output csv of identity analysis and creates a table to track fish over time.

# import the results of the identity_analysis script
filename <- "identity/2016-11-02_idanalyis.csv"
idcsv <- read.csv(filename, stringsAsFactors = F)

# strip down to sample_id and field data
idsimp <- idcsv[ , c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon")]

# pull anem_id from database
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

suppressWarnings(c1 <- leyte %>% tbl("anemones") %>% select(anem_table_id, anem_id, old_anem_id) %>% collect()) 

idsimp <- left_join(idsimp, c1, by = c("First.anem_table_id" = "anem_table_id"))
colnames(idsimp) <- c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon", "first_anem_id", "first_old_anem_id")

idsimp <- left_join(idsimp, c1, by = c("Second.anem_table_id" = "anem_table_id"))
colnames(idsimp) <- c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon", "first_anem_id", "first_old_anem_id", "second_anem_id", "second_old_anem_id")

rm(c1)

# convert all anemone numbers to same digit format
idsimp$first_anem_id <- as.numeric(idsimp$first_anem_id)
idsimp$first_old_anem_id <- as.numeric(idsimp$first_old_anem_id)
idsimp$second_anem_id <- as.numeric(idsimp$second_anem_id)
idsimp$second_old_anem_id <- as.numeric(idsimp$second_old_anem_id)

# add row numbers 
idsimp$fish <- 1:nrow(idsimp)

# re-order columns
idsimp <- idsimp[ , c("fish", "First.sample_id", "Second.sample_id","First.Size", "Second.Size","First.Date","Second.Date","first_anem_id","second_anem_id","first_old_anem_id",  "second_old_anem_id",  "First.Name",   "Second.Name", "First.anem_table_id", "First.fish_table_id",  "First.dive_table_id", "First.ObsTime",  "First.id", "First.lat",  "First.lon", "Second.anem_table_id", "Second.fish_table_id",   "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.lat", "Second.lon")]  

# convert site names to same convention
idsimp$First.Name[grep("Wangag", idsimp$First.Name)] <- "Wangag"
idsimp$First.Name[grep("Magbangon", idsimp$First.Name)] <- "Magbangon"
idsimp$First.Name[grep("Visca", idsimp$First.Name)] <- "Visca"
idsimp$First.Name[grep("Sitio Baybayon", idsimp$First.Name)] <- "Sitio Baybayon"
idsimp$First.Name[grep("Poroc San Flower", idsimp$First.Name)] <- "Poroc San Flower"
idsimp$First.Name[grep("Palanas", idsimp$First.Name)] <- "Palanas"
idsimp$First.Name[grep("Elementary School", idsimp$First.Name)] <- "Elementary School"
idsimp$First.Name[grep("Tamakin Dacot", idsimp$First.Name)] <- "Tamakin Dacot"
idsimp$First.Name[grep("Poroc Rose", idsimp$First.Name)] <- "Poroc Rose"
idsimp$First.Name[grep("Cabatoan", idsimp$First.Name)] <- "Cabatoan"

idsimp$Second.Name[grep("Cabu", idsimp$Second.Name)] <- "Cabatoan"
idsimp$Second.Name[grep("Haina", idsimp$Second.Name)] <- "Haina"
idsimp$Second.Name[grep("Poroc Rose", idsimp$Second.Name)] <- "Poroc Rose"
idsimp$Second.Name[grep("Tamakin Dacot", idsimp$Second.Name)] <- "Tamakin Dacot"
idsimp$Second.Name[grep("Elementary School", idsimp$Second.Name)] <- "Elementary School"
idsimp$Second.Name[grep("Palanas", idsimp$Second.Name)] <- "Palanas"
idsimp$Second.Name[grep("Poroc San Flower", idsimp$Second.Name)] <- "Poroc San Flower"
idsimp$Second.Name[grep("Sitio Baybayon", idsimp$Second.Name)] <- "Sitio Baybayon"
idsimp$Second.Name[grep("Visca", idsimp$Second.Name)] <- "Visca"
idsimp$Second.Name[grep("Wangag", idsimp$Second.Name)] <- "Wangag"
idsimp$Second.Name[grep("Magbangon", idsimp$Second.Name)] <- "Magbangon"

# find sites that don't match
site_mismatch <- idsimp[idsimp$First.Name != idsimp$Second.Name, ]

# because there are 7 fish with sites that don't match, pull in distkm data
idsimp$distkm <- idcsv$distkm
site_mismatch <- idsimp[idsimp$First.Name != idsimp$Second.Name, ]

# all except the APCL14_437/APCL15_399786 look like they are very close together, double check site info for where the fish were caught - all are on the Magbangon/Cabatoan border, these are the same fish.
lat_lon_table <- data.frame(site_mismatch$First.sample_id,site_mismatch$First.lat, site_mismatch$First.lon)
temp <- data.frame(site_mismatch$Second.sample_id,site_mismatch$Second.lat, site_mismatch$Second.lon)
colnames(temp) <- colnames(lat_lon_table)
lat_lon_table <- rbind(lat_lon_table, temp)
# write to csv and import into QGIS
write.csv(lat_lon_table, file = "data/site_investigation.csv", row.names = F)

# calculate how much a fish grew over time
idsimp$growth <- idsimp$Second.Size - idsimp$First.Size

# test i <- 11
for (i in 1:nrow(idsimp)){
  if(idsimp$Second.Date[i] < idsimp$First.Date[i]){
    idsimp$growth[i] <- abs(idsimp$growth[i])
  }
}

hmmm <- idsimp[idsimp$growth < 0, ]

multiples1 <- idsimp[1, ]
multiples1[1, ] <- NA

# find multiple instances of the same fish and assign to the same row
# test i <- 70
for (i in 1:nrow(idsimp)){
  n <- grep(idsimp$First.sample_id[i], idsimp)
  if(length(n) > 1){
    multiples1 <- rbind(multiples1, idsimp[i, ])
  }
}
multiples1 <- multiples1[!is.na(multiples1$fish), ]

multiples2 <- idsimp[1, ]
multiples2[1, ] <- NA

# find multiple instances of the same fish and assign to the same row
# test i <- 70
for (i in 1:nrow(idsimp)){
  n <- grep(idsimp$Second.sample_id[i], idsimp)
  if(length(n) > 1){
    multiples2 <- rbind(multiples2, idsimp[i, ])
  }
}
multiples2 <- multiples2[!is.na(multiples2$fish), ]

multiples1 <- rbind(multiples1, multiples2)
rm(multiples2)

# make a dataframe that works by year
idsimp <- multiples1

# test i <- 1

idsimp$sample_id_12 <- NA
idsimp$size_12 <- NA
idsimp$anem_id_12 <- NA
idsimp$site_12 <- NA

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "12"){
    idsimp$sample_id_12[i] <- idsimp$First.sample_id[i]
    idsimp$size_12[i] <- idsimp$First.Size[i]
    idsimp$anem_id_12[i] <- idsimp$first_anem_id[i]
    idsimp$site_12[i] <- idsimp$First.Name[i]
  }
}

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "12"){
    idsimp$sample_id_12[i] <- idsimp$Second.sample_id[i]
    idsimp$size_12[i] <- idsimp$Second.Size[i]
    idsimp$anem_id_12[i] <- idsimp$second_anem_id[i]
    idsimp$site_12[i] <- idsimp$Second.Name[i]
  }
}

idsimp$sample_id_13 <- NA
idsimp$size_13 <- NA
idsimp$anem_id_13 <- NA
idsimp$site_13 <- NA

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "13"){
    idsimp$sample_id_13[i] <- idsimp$First.sample_id[i]
    idsimp$size_13[i] <- idsimp$First.Size[i]
    idsimp$anem_id_13[i] <- idsimp$first_anem_id[i]
    idsimp$site_13[i] <- idsimp$First.Name[i]
  }
}

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "13"){
    idsimp$sample_id_13[i] <- idsimp$Second.sample_id[i]
    idsimp$size_13[i] <- idsimp$Second.Size[i]
    idsimp$anem_id_13[i] <- idsimp$second_anem_id[i]
    idsimp$site_13[i] <- idsimp$Second.Name[i]
  }
}

idsimp$sample_id_14 <- NA
idsimp$size_14 <- NA
idsimp$anem_id_14 <- NA
idsimp$site_14 <- NA

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "14"){
    idsimp$sample_id_14[i] <- idsimp$First.sample_id[i]
    idsimp$size_14[i] <- idsimp$First.Size[i]
    idsimp$anem_id_14[i] <- idsimp$first_anem_id[i]
    idsimp$site_14[i] <- idsimp$First.Name[i]
  }
}

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "14"){
    idsimp$sample_id_14[i] <- idsimp$Second.sample_id[i]
    idsimp$size_14[i] <- idsimp$Second.Size[i]
    idsimp$anem_id_14[i] <- idsimp$second_anem_id[i]
    idsimp$site_14[i] <- idsimp$Second.Name[i]
  }
}


idsimp$sample_id_15 <- NA
idsimp$size_15 <- NA
idsimp$anem_id_15 <- NA
idsimp$site_15 <- NA

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "15"){
    idsimp$sample_id_15[i] <- idsimp$First.sample_id[i]
    idsimp$size_15[i] <- idsimp$First.Size[i]
    idsimp$anem_id_15[i] <- idsimp$first_anem_id[i]
    idsimp$site_15[i] <- idsimp$First.Name[i]
  }
}

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "15"){
    idsimp$sample_id_15[i] <- idsimp$Second.sample_id[i]
    idsimp$size_15[i] <- idsimp$Second.Size[i]
    idsimp$anem_id_15[i] <- idsimp$second_anem_id[i]
    idsimp$site_15[i] <- idsimp$Second.Name[i]
  }
}

wide <- idsimp[ , c("sample_id_12", "size_12", "anem_id_12", "site_12", "sample_id_13", "size_13", "anem_id_13", "site_13", "sample_id_14", "size_14", "anem_id_14", "site_14", "sample_id_15", "size_15", "anem_id_15", "site_15")]

# flatten multiples
wide[44, 5:8] <- wide[45, 5:8]
wide[45, ] <- NA

wide[16, 13:16] <- wide[69, 13:16]
wide[69, ] <- NA

wide[21, ] <- NA

wide[17, 13:16] <- wide[58, 13:16]
wide[58, ] <- NA

wide[13, 13:16] <- wide[78, 13:16]
wide[78, ] <- NA

wide[70, ] <- NA
wide[57, ] <- NA
wide[79, ] <- NA

figure <- wide[ , c("size_12", "size_13", "size_14", "size_15")]

# save idsimp and wide for later
write.csv(idsimp, file = paste("data/", Sys.Date(), "idsimp.csv", sep = ""), row.names = F)
write.csv(wide, file = paste("data/", Sys.Date(),"wide.csv", sep = ""), row.names = F)

# next step is to look at the data from Chris's class and see how to plot growth