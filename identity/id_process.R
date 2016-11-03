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

# calculate how much a fish grew over time
idsimp$growth <- idsimp$Second.Size - idsimp$First.Size

# test i <- 11
for (i in 1:nrow(idsimp)){
  if(idsimp$Second.Date[i] < idsimp$First.Date[i]){
    idsimp$growth[i] <- abs(idsimp$growth[i])
  }
}

# test i <- 1

fields <- c("sample", "size", "anem", "site", "date")
fields1 <- NA
for (i in 1:length(fields)){
  n <- paste(fields[i], 12:15, sep = "")
  fields1 <- c(fields1, n)
}
fields1 <- fields1[2:21]

wide <- data.frame(fields1)
wide$fields2 <- NA
wide <- reshape(wide, direction = "wide", idvar = "fields1")

for (i in 1:length(fields1)){
  wide$fields1[i] <- "a"
}

idsimp$sample_id_12 <- NA
idsimp$size_12 <- NA
idsimp$anem_id_12 <- NA
idsimp$site_12 <- NA
idsimp$date_12 <- NA

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$First.sample_id[i]) & substr(idsimp$First.sample_id[i], 5,6) == "12"){
    idsimp$sample_id_12[i] <- idsimp$First.sample_id[i]
    idsimp$size_12[i] <- idsimp$First.Size[i]
    idsimp$anem_id_12[i] <- idsimp$first_anem_id[i]
    idsimp$site_12[i] <- idsimp$First.Name[i]
    idsimp$date_12[i] <- idsimp$First.Date[i]
  }
}

for (i in 1:nrow(idsimp)){
  if(!is.na(idsimp$Second.sample_id[i]) & substr(idsimp$Second.sample_id[i], 5,6) == "12"){
    idsimp$sample_id_12[i] <- idsimp$Second.sample_id[i]
    idsimp$size_12[i] <- idsimp$Second.Size[i]
    idsimp$anem_id_12[i] <- idsimp$second_anem_id[i]
    idsimp$site_12[i] <- idsimp$Second.Name[i]
    idsimp$date_12[i] <- idsimp$Second.Date[i]
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

wide <- idsimp[ , c("sample_id_12", "size_12", "anem_id_12", "site_12", "date_12", "sample_id_13", "size_13", "anem_id_13", "site_13", "sample_id_14", "size_14", "anem_id_14", "site_14", "sample_id_15", "size_15", "anem_id_15", "site_15")]
twel <- grep("12", colnames(wide))
thirt <- grep("13", colnames(wide))
fourt <- grep("14", colnames(wide))
fift <- grep("15", colnames(wide))

# flatten multiples
wide[44, thirt] <- wide[45, thirt]
wide[45, ] <- NA

wide[16, fift] <- wide[69, fift]
wide[69, ] <- NA

wide[21, ] <- NA

wide[17, fift] <- wide[58, fift]
wide[58, ] <- NA

wide[13, fift] <- wide[78, fift]
wide[78, ] <- NA

wide[70, ] <- NA
wide[57, ] <- NA
wide[79, ] <- NA


# # save idsimp and wide for later
# write.csv(idsimp, file = paste("data/", Sys.Date(), "idsimp.csv", sep = ""), row.names = F)
# write.csv(wide, file = paste("data/", Sys.Date(),"wide.csv", sep = ""), row.names = F)
# wide <- read.csv("data/2016-11-03wide.csv", stringsAsFactors = F)

wide$date_12 <- 2012
wide$date_13 <- 2013
wide$date_14 <- 2014
wide$date_15 <- 2015


# next step is to look at the data from Chris's class and see how to plot growth
plot(x = wide$date_12, y = wide$size_12, type = "p", xlim = c(2010, 2020))
plot(x = wide$date_14, y = wide$size_14, type = "p")
plot(x = wide$)
