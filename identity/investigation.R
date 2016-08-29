# 7/27/2016 (MRS) - A script to examine all samples (no regenos or recaps have been removed from the genepop) to check for lab errors.  Uses the match table generated in identity_analysis.R

# Lightning
# setwd("/Users/macair/Documents/Philippines/Genetics/")
# source("readGenepop_space.R")

# Import cervus identity results ------------------------------------------

# 8/29/2016 - use fixed lat long code, double check all samples
idcsv <- read.csv("identity/allsamples_ID.csv", stringsAsFactors = F)
# idcsv <- read.csv("identity/2016-08-18_ID.csv", stringsAsFactors = F)

### WAIT ###

# # strip down to just ligation numbers for the all samples file -------------------------------------
# 
# idcsv$First.ID <- substr(idcsv$First.ID,11,15)
# idcsv$Second.ID <- substr(idcsv$Second.ID,11,15)
# 
# # remove samples that are known for the all samples file
# idcsv <- idcsv[idcsv$First.ID != "L2364", ] # no field data for this fish
# idcsv <- idcsv[idcsv$Second.ID != "L0465", ] # no field data for this fish

# for the 8/18/2016 file, remove the non-matches
idcsv <- idcsv[idcsv$Status != "Excluded", ]
idcsv <- idcsv[idcsv$Status != "Not enough loci", ]


# Connect to database and add lab data ------------------------------------

# Connect to database using dplyr
suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

c1 <- labor %>% tbl("extraction") %>% select(extraction_ID, sample_ID)
c2 <- labor %>% tbl("digest") %>% select(digest_ID, extraction_ID)
c3 <- left_join(c2, c1, by = "extraction_ID")
c4 <- labor %>% tbl("ligation") %>% select(ligation_ID, digest_ID)
c5 <- data.frame(left_join(c4, c3, by = "digest_ID"))

# for First.IDs 
lab1 <- c5
names(lab1) <- paste("First.", names(lab1), sep = "")

idcsv <- merge(idcsv, lab1, by.x = "First.ID", by.y = "First.ligation_ID", all.x = T)

# For Second.IDs
lab2 <- c5
names(lab2) <- paste("Second.", names(lab2), sep = "")

idcsv <- merge(idcsv, lab2, by.x = "Second.ID", by.y = "Second.ligation_ID", all.x = T)

# Add field data ----------------------------------------------------------
leyte <- src_mysql(dbname = "Leyte", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)

c1 <- leyte %>% tbl("diveinfo") %>% select(id, Date, Name)
c2 <- leyte %>% tbl("anemones") %>% select(dive_table_id, anem_table_id, ObsTime)
c3 <- left_join(c2, c1, by = c("dive_table_id" = "id"))
# c4 <- leyte %>% tbl("clownfish") %>% select(fish_table_id, anem_table_id, Sample_ID, Size) 
c4 <- tbl(leyte, sql("SELECT fish_table_id, anem_table_id, Sample_ID, Size FROM clownfish where Sample_ID is not NULL"))
first <- data.frame(left_join(c4, c3, by = "anem_table_id"))

### WAIT ###

second <- data.frame(left_join(c4, c3, by = "anem_table_id"))

### WAIT ###

names(first) <- paste("First.", names(first), sep = "")
names(second) <- paste("Second.", names(second), sep = "")
idcsv <- left_join(idcsv, first, by = c("First.sample_ID" = "First.Sample_ID"))
idcsv <- left_join(idcsv, second, by = c("Second.sample_ID" = "Second.Sample_ID"))

latlong <- data.frame(leyte %>% tbl("GPX") %>% collect())
# latlong <- leyte %>% tbl("GPX")

### WAIT ###

idcsv$First.lat <- NA
idcsv$First.lon <- NA
idcsv$Second.lat <- NA
idcsv$Second.lon <- NA

# Add lat long for first.id -----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$First.Date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$First.ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour <0){
    day <- day-1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))

# Calculate the lat/long for this time
if(length(i2)>0){
  idcsv$First.lat[i] = latlong$lat[latlongindex][i2]
  idcsv$First.lon[i] = latlong$long[latlongindex][i2]
}
}

### WAIT ###

# Add lat long for second.id ----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$Second.Date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$Second.ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour <0){
    day <- day-1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    idcsv$Second.lat[i] = latlong$lat[latlongindex][i2]
    idcsv$Second.lon[i] = latlong$long[latlongindex][i2]
  }
}

### WAIT ###

# Check for fish that were caught on the same day -------------------------

idcsv$date_eval <- NA
for(i in 1:nrow(idcsv)){
  a <- idcsv$First.Date[i]
  b <- idcsv$Second.Date[i]
  if (a == b & !is.na(a) & !is.na(b)){
    idcsv$date_eval[i] <- "FAIL"
  }
}

# Flag matches that were caught more than 250m apart ----------------------

library(fields)
alldists <- rdist.earth(as.matrix(idcsv[,c('First.lon', 'First.lat')]), as.matrix(idcsv[,c('Second.lon', 'Second.lat')]), miles=FALSE, R=6371) # see http://www.r-bloggers.com/great-circle-distance-calculations-in-r/ # slow because it does ALL pairwise distances, instead of just in order
idcsv$distkm <- diag(alldists)

idcsv$disteval <- NA # placeholder
for(i in 1:nrow(idcsv)){
  if(!is.na(idcsv$distkm[i]) & 0.250 <= idcsv$distkm[i]){
    idcsv$disteval[i] <- "FAIL"
  }
}

# Flag matches where size decreases by more than 1.5cm --------------------

idcsv$size_eval <- NA
for (i in 1:nrow(idcsv)){
  if(!is.na(idcsv$First.Date[i]) & !is.na(idcsv$Second.Date[i])  & idcsv$First.Date[i] < idcsv$Second.Date[i]) {
    if(!is.na(idcsv$First.Size[i]) & !is.na(idcsv$Second.Size[i]) & (idcsv$First.Size[i] - 1.5) > idcsv$Second.Size[i]){
      idcsv$size_eval[i] <- "FAIL"
    }
  }
}


# Create a list of regenotyped samples -----------------------------------
regeno <- idcsv[which(idcsv$Second.sample_ID == idcsv$First.sample_ID), ]

# create a file of regenotyped samples
write.csv(regeno, file = paste(Sys.Date(),"regenotyped.csv", sep = ""))

# Remove the regenotypes from the analysis
idcsv <- idcsv[which(idcsv$Second.sample_ID != idcsv$First.sample_ID), ]


# Make a list of all of the samples that fail for some reason ------------
issues <- idcsv[which(!is.na(idcsv$disteval) | !is.na(idcsv$date_eval) | !is.na(idcsv$size_eval)), ]


# Make a list of all the samples with no issues --------------------------
matches <- idcsv[which(is.na(idcsv$disteval) & is.na(idcsv$date_eval) & is.na(idcsv$size_eval)), ]

# write out the matches file
write.csv(matches, file = paste(Sys.Date(),"matches.csv", sep = ""))

# remove samples that did not have GPS data
issues <- issues[issues$First.ID != "L2283", ] # no gps data for this fish (2012 fish)
issues <- issues[issues$First.ID != "L2431", ] # no gps data for this fish (2012 fish)
issues <- issues[issues$First.ID != "L2292", ] # no gps data for this fish (2012 fish)
issues <- issues[issues$First.ID != "L1101", ] # no gps data for this fish (2014 fish)
issues <- issues[issues$First.ID != "L1102", ] # no gps data for this fish (2014 fish)

# remove the fish that was caught twice in the same field season
issues <- issues[issues$First.ID != "L1084", ] # regenotype






###########################################################################

# Write out a list of known issues ligations IDs to add to the known issues list

out <- subset(issues, select = c(First.ID, First.extraction_ID))
names(out) <- c("Ligation_ID", "Extraction_ID")

x <- subset(issues, select = c(Second.ID, Second.extraction_ID))
names(x) <- names(out)

out <- rbind(out, x)
# out$Issue <- "date, distance, or size issue found during id analysis"

dbWriteTable(leyte, "known_issues", out, row.names = F, append = T)

dbSendQuery(leyte, "CREATE TABLE temp (
  Ligation_ID VARCHAR(5) NOT NULL,
  Issue text(140)
);")

dbSendQuery(leyte, "insert into temp select distinct Ligation_ID, Issue from known_issues;")

dbSendQuery(leyte, "DROP TABLE known_issues;")
dbSendQuery(leyte, "RENAME TABLE temp TO known_issues;")

dbDisconnect(leyte)
dbDisconnect(labor)

rm(leyte, labor)

# # create a table that can be imported to update notes about samples that are not recaptures
# 
# examine <- match[which(match$date_eval == "FAIL"), ]
# 
# 
# # Take a look at the remaining samples in the examine table and write notes in evernote about why they are there - remember so far these are only samples that raise a red flag because of the date of capture
# 
# x <- examine[,c("First.Sample_ID", "First.ID")]
# x <- examine[,c("Second.Sample_ID", "Second.ID")]
# 
# # make a list that can be imported into a known issues table
# 
# # split the data
# 
# out <- subset(examine, select = c(First.ID))
# names(out) <- c("Ligation_ID")
# 
# x <- subset(examine, select = c(Second.ID))
# names(x) <- names(out)
# 
# out <- rbind(out, x)
# 
# out <- unique(out$Ligation_ID)
# 
# # write a list of known issue ligations
# write.table(out, file = "knownIssueLigations.txt", row.names = F, col.names = F)
# 
# # Now take a look at failed distance evals
# examine <- rbind(examine, match[which(match$disteval == "FAIL"), ])
# 
# # create a table of samples that are too far apart
# distance <- examine[which(examine$disteval == "FAIL"), ]
# 
# # reorder the columns - move columns
# dist2 <- distance[ , c(2,8,1,7,6,5,4,3)]
