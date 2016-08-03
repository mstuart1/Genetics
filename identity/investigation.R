# 7/27/2016 (MRS) - A script to examine all samples (no regenos or recaps have been removed from the genepop) to check for lab errors.  Uses the match table generated in identity_analysis.R

# Lightning
# setwd('/Users/macair/Documents/Philippines/Genetics/identity')
source("../readGenepop_space.R")

# Import cervus identity results ------------------------------------------

idcsv <- read.csv("allsamples_ID.csv", stringsAsFactors = F)


# strip down to just ligation numbers -------------------------------------

idcsv$First.ID <- substr(idcsv$First.ID,11,15)
idcsv$Second.ID <- substr(idcsv$Second.ID,11,15)

# Connect to database and add lab data -----------------------------------------------------

library(RMySQL)
labor <- dbConnect(MySQL(), host="amphiprion.deenr.rutgers.edu", user="michelles", password="larvae168", dbname="Laboratory", port=3306)


# add lab data
lab <- dbSendQuery(labor, "select ligations.`ligation_ID`, digests.digest_ID, extractions.extraction_ID, extractions.Sample_ID from extractions 
join digests on extractions.extraction_ID = digests.extraction_ID
  join ligations on digests.digest_ID = ligations.digest_ID;")
lab <- fetch(lab, n=-1)

# For First.IDs
lab1 <- lab
names(lab1) <- paste("First.", names(lab1), sep = "")

idcsv <- merge(idcsv, lab1, by.x = "First.ID", by.y = "First.ligation_ID", all.x = T)

# For Second.IDs
lab2 <- lab
names(lab2) <- paste("Second.", names(lab2), sep = "")

idcsv <- merge(idcsv, lab2, by.x = "Second.ID", by.y = "Second.ligation_ID", all.x = T)


# Add field data ----------------------------------------------------------
leyte <- dbConnect(MySQL(), host="amphiprion.deenr.rutgers.edu", user="michelles", password="larvae168", dbname="Leyte", port=3306)

# add date of capture
field <- dbSendQuery(leyte, "select clownfish.Sample_ID as Sample_ID, 
diveinfo.Date as Date, anemones.ObsTime as time, clownfish.Size as size
  from clownfish 
  join anemones on clownfish.anem_table_id = anemones.anem_table_id
  join diveinfo on anemones.dive_table_id = diveinfo.id;;")
field <- fetch(field, n=-1)

# For First.IDs
field1 <- field
names(field1) <- paste("First.", names(field1), sep = "")

idcsv <- merge(idcsv, field1, by.x = "First.Sample_ID", by.y = "First.Sample_ID", all.x = T)

# For Second.IDs
field2 <- field
names(field2) <- paste("Second.", names(field2), sep = "")

idcsv <- merge(idcsv, field2, by.x = "Second.Sample_ID", by.y = "Second.Sample_ID", all.x = T)


latlong <- dbReadTable(leyte, "GPX")


# Add lat long for first.id -----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$First.Date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$First.time[i])
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
latlongindex <- which(latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
i2 <- which.min(abs(latlong$sec[latlongindex] - sec))

# Calculate the lat/long for this time
if(length(i2)>0){
  idcsv$First.lat[i] = latlong$lat[latlongindex][i2]
  idcsv$First.lon[i] = latlong$long[latlongindex][i2]
}
}


# Add lat long for second.id ----------------------------------------------
for(i in 1:nrow(idcsv)){
  #Get date and time information for the anemone
  date <- as.character(idcsv$Second.Date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(idcsv$Second.time[i])
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
  latlongindex <- which(latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    idcsv$Second.lat[i] = latlong$lat[latlongindex][i2]
    idcsv$Second.lon[i] = latlong$long[latlongindex][i2]
  }
}


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
    if(!is.na(idcsv$First.size[i]) & !is.na(idcsv$Second.size[i]) & (idcsv$First.size[i] - 1.5) > idcsv$Second.size[i]){
      idcsv$size_eval[i] <- "FAIL"
    }
  }
}


# Create a list of regenotyped samples -----------------------------------
regeno <- idcsv[which(idcsv$Second.Sample_ID == idcsv$First.Sample_ID), ]

# Remove the regenotypes from the analysis
idcsv <- idcsv[which(idcsv$Second.Sample_ID != idcsv$First.Sample_ID), ]


# Make a list of all of the samples that fail for some reason ------------
issues <- idcsv[which(!is.na(idcsv$disteval) | !is.na(idcsv$date_eval) | !is.na(idcsv$size_eval)), ]


# Make a list of all the samples with no issues --------------------------
matches <- idcsv[which(is.na(idcsv$disteval) & is.na(idcsv$date_eval) & is.na(idcsv$size_eval)), ]



# create a table that can be imported to update notes about samples that are not recaptures

examine <- match[which(match$date_eval == "FAIL"), ]


# Take a look at the remaining samples in the examine table and write notes in evernote about why they are there - remember so far these are only samples that raise a red flag because of the date of capture

x <- examine[,c("First.Sample_ID", "First.ID")]
x <- examine[,c("Second.Sample_ID", "Second.ID")]

# make a list that can be imported into a known issues table

# split the data

out <- subset(examine, select = c(First.ID))
names(out) <- c("Ligation_ID")

x <- subset(examine, select = c(Second.ID))
names(x) <- names(out)

out <- rbind(out, x)

out <- unique(out$Ligation_ID)

# write a list of known issue ligations
write.table(out, file = "knownIssueLigations.txt", row.names = F, col.names = F)

# Now take a look at failed distance evals
examine <- rbind(examine, match[which(match$disteval == "FAIL"), ])

# create a table of samples that are too far apart
distance <- examine[which(examine$disteval == "FAIL"), ]

# reorder the columns - move columns
dist2 <- distance[ , c(2,8,1,7,6,5,4,3)]
