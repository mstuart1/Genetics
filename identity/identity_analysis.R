# This script evaluates the output of a cervus identity analysis and flags "true matches" versus "false positives"
# TODO - make a list of good matches and a list of bad matches and see where they intersect, then drop bad part of the bad match sample but not the good part.


# Set up workspace --------------------------------------------------------

<<<<<<< HEAD
# Lightning
# setwd('/Users/macair/Documents/Philippines/Genetics/identity')
source("code/readGenepop_space.R")
||||||| merged common ancestors
# Lightning
setwd('/Users/macair/Documents/Philippines/Genetics/identity')
source("../readGenepop_space.R")
=======
# # Lightning
# setwd('/Users/macair/Documents/Philippines/Genetics/identity')

source("readGenepop_space.R")
>>>>>>> f1e79422a31d500e4761ffefeeb12d342066f1c9

# Import cervus identity results ------------------------------------------

<<<<<<< HEAD
idcsv <- read.csv("identity/2016-08-18_ID.csv", stringsAsFactors = F)

# for the 8/18/2016 file, remove the non-matches
idcsv <- idcsv[idcsv$Status != "Excluded", ]
idcsv <- idcsv[idcsv$Status != "Not enough loci", ]
||||||| merged common ancestors
idcsv <- read.csv("2016-08-18_ID.csv", stringsAsFactors = F)
=======
idcsv <- read.csv("identity/2016-08-18_ID.csv", stringsAsFactors = F)
# # for the 8/18/2016 file, remove the non-matches
idcsv <- idcsv[idcsv$Status != "Excluded", ]
idcsv <- idcsv[idcsv$Status != "Not enough loci", ]

>>>>>>> f1e79422a31d500e4761ffefeeb12d342066f1c9

### WAIT ###

# Add metadata ------------------------------------------------------------

# Connect to database -----------------------------------------------------

suppressMessages(library(dplyr))
labor <- src_mysql(dbname = "Laboratory", host = "amphiprion.deenr.rutgers.edu", user = "michelles", password = "larvae168", port = 3306, create = F)


# add lab IDs

c1 <- labor %>% tbl("extraction") %>% select(extraction_ID, sample_ID)
c2 <- labor %>% tbl("digest") %>% select(digest_ID, extraction_ID)
c3 <- left_join(c2, c1, by = "extraction_ID")
c4 <- labor %>% tbl("ligation") %>% select(ligation_ID, digest_ID)
c5 <- data.frame(left_join(c4, c3, by = "digest_ID"))

# for First.IDs 
lab1 <- c5
names(lab1) <- paste("First.", names(lab1), sep = "")


idcsv <- merge(idcsv, lab1, by.x = "First.ID", by.y = "First.ligation_ID", all.x = T)

### WAIT ###

# For Second.IDs
lab2 <- c5
names(lab2) <- paste("Second.", names(lab2), sep = "")

idcsv <- merge(idcsv, lab2, by.x = "Second.ID", by.y = "Second.ligation_ID", all.x = T)

### WAIT ###

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

<<<<<<< HEAD
idcsv$First.lat <- NA
idcsv$First.lon <- NA
idcsv$Second.lat <- NA
idcsv$Second.lon <- NA

latlong <- data.frame(leyte %>% tbl("GPX") %>% collect())
||||||| merged common ancestors
latlong <- data.frame(leyte %>% tbl("GPX"), n = -1)
=======
latlong <- data.frame(leyte %>% tbl("GPX") %>% collect())
>>>>>>> f1e79422a31d500e4761ffefeeb12d342066f1c9
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

# Flag matches with same date of capture ----------------------------------
<<<<<<< HEAD
idcsv$First.Date <- as.Date(idcsv$First.Date, "%m/%d/%Y")
idcsv$Second.Date <- as.Date(idcsv$Second.Date, "%m/%d/%Y")


idcsv$date_eval <- NA
for(i in 1:nrow(idcsv)){
  a <- idcsv$First.Date[i]
  b <- idcsv$Second.Date[i]
||||||| merged common ancestors
match$First.Date <- as.Date(match$First.Date, "%m/%d/%Y")
match$Second.Date <- as.Date(match$Second.Date, "%m/%d/%Y")


match$date_eval <- NA
for(i in 1:nrow(match)){
  a <- match$First.Date[i]
  b <- match$Second.Date[i]
=======
idcsv$date_eval <- NA
for(i in 1:nrow(idcsv)){
  a <- idcsv$First.Date[i]
  b <- idcsv$Second.Date[i]
>>>>>>> f1e79422a31d500e4761ffefeeb12d342066f1c9
  if (a == b & !is.na(a) & !is.na(b)){
    idcsv$date_eval[i] <- "FAIL"
  }
}

### WAIT ### - if you have to wait here, double check the number of obs, there may be a problem with the attachment of metadata



# Flag matches that were caught more than 250m apart ----------------------

<<<<<<< HEAD
# library(fields)
# source('greatcircle_funcs.R') # alternative, probably faster
alldists <- fields::rdist.earth(as.matrix(idcsv[,c('First.lon', 'First.lat')]), as.matrix(idcsv[,c('Second.lon', 'Second.lat')]), miles=FALSE, R=6371) # see http://www.r-bloggers.com/great-circle-distance-calculations-in-r/ # slow because it does ALL pairwise distances, instead of just in order
idcsv$distkm <- diag(alldists)
||||||| merged common ancestors
library(fields)
# source('greatcircle_funcs.R') # alternative, probably faster
alldists <- rdist.earth(as.matrix(match[,c('First.Lon', 'First.Lat')]), as.matrix(match[,c('Second.Lon', 'Second.Lat')]), miles=FALSE, R=6371) # see http://www.r-bloggers.com/great-circle-distance-calculations-in-r/ # slow because it does ALL pairwise distances, instead of just in order
match$distkm <- diag(alldists)
=======
alldists <- fields::rdist.earth(as.matrix(idcsv[,c('First.lon', 'First.lat')]), as.matrix(idcsv[,c('Second.lon', 'Second.lat')]), miles=FALSE, R=6371) # see http://www.r-bloggers.com/great-circle-distance-calculations-in-r/ # slow because it does ALL pairwise distances, instead of just in order
idcsv$distkm <- diag(alldists)
>>>>>>> f1e79422a31d500e4761ffefeeb12d342066f1c9

idcsv$disteval <- NA # placeholder
for(i in 1:nrow(idcsv)){
  if(!is.na(idcsv$distkm[i]) & 0.250 <= idcsv$distkm[i]){
    idcsv$disteval[i] <- "FAIL"
  }
}


# Flag idcsves where size decreases by more than 1.5cm --------------------

<<<<<<< HEAD
idcsv$size_eval <- NA
for (i in 1:nrow(idcsv)){
  if(!is.na(idcsv$First.Date[i]) & !is.na(idcsv$Second.Date[i])  & idcsv$First.Date[i] < idcsv$Second.Date[i]) {
    if(!is.na(idcsv$First.Size[i]) & !is.na(idcsv$Second.Size[i]) & (idcsv$First.Size[i] - 1.5) > idcsv$Second.Size[i]){
      idcsv$size_eval[i] <- "FAIL"
    }
  }
}
  
for (i in 1:nrow(idcsv)){
  if(!is.na(idcsv$First.Date[i]) & !is.na(idcsv$Second.Date[i])  & idcsv$First.Date[i] > idcsv$Second.Date[i]) {
    if(!is.na(idcsv$First.Size[i]) & (idcsv$First.Size[i] + 1.5) < idcsv$Second.Size[i]){
      idcsv$size_eval[i] <- "FAIL"
||||||| merged common ancestors
match$size_eval <- NA
for (i in 1:nrow(match)){
  if(!is.na(match$First.Date[i]) & !is.na(match$Second.Date[i])  & match$First.Date[i] < match$Second.Date[i]) {
    if(!is.na(match$First.Size[i]) & !is.na(match$Second.Size[i]) & (match$First.Size[i] - 1.5) > match$Second.Size[i]){
      match$size_eval[i] <- "FAIL"
    }
  }
}
  
for (i in 1:nrow(match)){
  if(!is.na(match$First.Date[i]) & !is.na(match$Second.Date[i])  & match$First.Date[i] > match$Second.Date[i]) {
    if(!is.na(match$First.Size[i]) & (match$First.Size[i] + 1.5) < match$Second.Size[i]){
      match$size_eval[i] <- "FAIL"
=======
idcsv$size_eval <- NA
for (i in 1:nrow(idcsv)){
  if(!is.na(idcsv$First.Date[i]) & !is.na(idcsv$Second.Date[i])  & idcsv$First.Date[i] < idcsv$Second.Date[i]) {
    if(!is.na(idcsv$First.Size[i]) & !is.na(idcsv$Second.Size[i]) & (idcsv$First.Size[i] - 1.5) > idcsv$Second.Size[i]){
      idcsv$size_eval[i] <- "FAIL"
>>>>>>> f1e79422a31d500e4761ffefeeb12d342066f1c9
    }
  }
}



# Write output ------------------------------------------------------------

write.csv(idcsv, file = paste(Sys.Date(), "_idanalyis.csv", sep = ""), row.names = F)


# Open genepop ------------------------------------------------------------

genfile <- ""
genedf <- readGenepop(genfile)

### WAIT ###

genedf[,1] <- NULL # remove the pop column from the data file
# TEST - make sure the first 2 columns are names and a contig and get number of rows
names(genedf[,1:2]) # [1] "names" "dDocent_Contig_107_30"
nrow(genedf) # 1651


# Calculate the number of loci for analysis -------------------------------

# convert 0000 to NA in the genepop data
genedf[genedf == "0000"] = NA
# TEST - make sure there are no "0000" left
which(genedf == "0000") # should return integer(0)

# count the number of loci per individual
for(h in 1:nrow(genedf)){
  genedf$numloci[h] <- sum(!is.na(genedf[h,]))
}
# TEST - make sure all of the numloci were populated
which(is.na(genedf$numloci)) # should return integer(0)


genedf$drop <- NA

# Remove problem samples --------------------------------------------------

for (i in 1:nrow(match)){
  if(!is.na(match$size_eval[i]) & match$size_eval[i] == "FAIL" | !is.na(match$disteval[i]) & match$disteval[i] == "FAIL" | match$date_eval[i] == "FAIL" & !is.na(match$date_eval[i])){
    a <- which(genedf$names == match$First.ID[i])
    b <- which(genedf$names == match$Second.ID[i])
    genedf$drop[a] <- "DROP"
    genedf$drop[b] <- "DROP"
  }
}

# remove the special cases above

# # TEST - compare the ligation IDs that were marked to be dropped from the genepop to the IDs in the match 
# genos <- genedf$names[which(genedf$drop == "DROP")]
# firsts <- match$First.ID[which(match$disteval == "FAIL")]
# secs <- match$Second.ID[which(match$disteval == "FAIL")]
# firdif <- setdiff(genos,firsts)
# secdif <- setdiff(firdif, secs)

# Run through id analysis and compare to determine which to remove --------
for(i in 1:nrow(match)){
  # a & b are  the line numbers from genepop file that matches an the first and second ID in the match table
  a <- which(genedf$names == match$First.ID[i])
  b <- which(genedf$names == match$Second.ID[i])
if (genedf$numloci[a] > genedf$numloci[b]){
  genedf$drop[b] <- "DROP"
} else{
  genedf$drop[a] <- "DROP"
}
}

# Make a dataframe of the samples that will be dropped
drops <- genedf[!is.na(genedf$drop),]

# Make a dataframe of the samples to keep for parentage analysis
keep <- genedf[is.na(genedf$drop),]

keep$numloci <- NULL
keep$drop <- NULL

# TODO -  Look for regenotypes again:

# convert all the NA genotypes to 0000
keep[is.na(keep)] = "0000"
# TEST - make sure there are no NA's left
which(keep == NA) # should return integer(0)

# Write out genepop  ------------------------------------------------------

# Build the genepop components
msg <- c("This genepop file was generated using a script called identity_analysis.R written by Michelle Stuart with help from Malin Pinsky and Ryan Batt")

loci <- paste(names(keep[,2:ncol(keep)]), collapse =",")

gene <- vector()
sample <- vector()
for (i in 1:nrow(keep)){
  gene[i] <- paste(keep[i,2:ncol(keep)], collapse = " ")
  sample[i] <- paste(keep[i,1], gene[i], sep = ", ")
}

  ### WAIT ###

out <- c(msg, loci, 'pop', sample)

write.table(out, file = paste(Sys.Date(), 'norecap.genepop', sep = '_'), row.names=FALSE, quote=FALSE, col.names=FALSE)


