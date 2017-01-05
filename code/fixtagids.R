# a script to pull the old pit tag data from the june spreadsheet into the august spreadsheet where the pit tag data has been concatenated incorrectly.

clownfishcoltypes <- c("numeric", "date", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "numeric", "text", "text", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")

june <- readxl::read_excel("~/Documents/Philippines/Surveys_2016/GPSSurveys2016_20160610MRS.xlsx", sheet = "Clownfish", col_names=TRUE, col_types = clownfishcoltypes)

# remove extra rows that are added by excel 
june <- june[!is.na(june$DiveNum), ]

# make june a table of fish in single rows
# Remove extra columns
# june <- june[ , c(1:7, 11, 13:48,54)]

# strip out Excel default date from StartTime, EndTime, PauseStart, and PauseEnd
june$ObsTime <- gsub('1899-12-30 ', '', june$ObsTime)

# Remove extra NA rows
june <- june[!is.na(june$DiveNum),]

# base <- 0
# 
june$anem_table_id <- NA
# for (i in 1:nrow(june)){
#   june$anem_table_id[i] <- june$id[i] + base
# }
# 
# june$id <- NULL 



# split out the lines for individual fish ---------------------------------



# Get the Size1 fish
allfish <- june[ , c("anem_table_id", "Spp", "Size1", "ID1", "Col1", "Recap1", "TagID1", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish <- allfish[!is.na(allfish$Size1), ]

# rename the columns
names(allfish) <- c("anem_table_id", "Spp", "Size", "ID", "Col", "Recap", "TagID", "Notes", "Collector")
names <- names(allfish)


# Get the Size2 fish
allfish2 <- june[ , c("anem_table_id", "Spp", "Size2", "ID2", "Col2", "Recap2", "TagID2", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size2), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size3 fish
allfish2 <- june[ , c("anem_table_id", "Spp", "Size3", "ID3", "Col3", "Recap3", "TagID3", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size3), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size4 fish
allfish2 <- june[ , c("anem_table_id", "Spp", "Size4", "ID4", "Col4", "Recap4", "TagID4", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size4), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size5 fish
allfish2 <- june[ , c("anem_table_id", "Spp", "Size5", "ID5", "Col5", "Recap5", "TagID5", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size5), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size6 fish
allfish2 <- june[ , c("anem_table_id", "Spp", "Size6", "ID6", "Col6", "Recap6", "TagID6", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size6), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)

# Get the Size7 fish
allfish2 <- june[ , c("anem_table_id", "Spp", "Size7", "ID7", "Col7", "Recap7", "TagID7", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size), ]

# Size7: may need to be split
if(length(allfish2)>0){
  i <- sapply(allfish2, is.factor); allfish2[i] = lapply(allfish2[i], as.character) # converts factors to characters
  allfish22 = allfish2[0,] # to hold the final values from Size7/ID7 (after disaggregating multiple entries on the same line)
  for(i in 1:nrow(allfish2)){ # check each row for multiple entries
    ids = gsub(' ', '', unlist(strsplit(as.character(allfish2$ID7[i]), split=','))) # split apart on comma and remove spaces
    sizes = gsub(' ', '', unlist(strsplit(as.character(allfish2$Size7[i]), split=','))) # split apart on comma
    cols = gsub(' ', '', unlist(strsplit(as.character(allfish2$Col7[i]), split=','))) # split apart on comma
    tags <- gsub(' ', '', unlist(strsplit(as.character(allfish2$TagID7[i]), split=','))) # split apart on comma
    if(length(sizes)>0){
      for(j in 1:length(sizes)){ # then check for non-NA, non-blank entries
        if(!is.na(sizes[j]) & sizes[j] != '' & sizes[j] != 'NA'){
          allfish22 = rbind(allfish22, allfish2[i,])
          allfish22$ID7[nrow(allfish22)] = ids[j]
          allfish22$Size7[nrow(allfish22)] = sizes[j]
          allfish22$Col7[nrow(allfish22)] = cols[j]
          allfish22$TagID7[nrow(allfish22)] = tags[j]
        }}
    }
  }
  names(allfish22)= names
  junefish <- rbind(allfish, allfish22)
}

clownfishcoltypes <- c("numeric", "numeric", "date", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")

aug <- readxl::read_excel("~/Documents/Philippines/Surveys_2016/GPSSurveys2016_20160801MRS.xlsx", sheet = "Clownfish", col_names=TRUE, col_types = clownfishcoltypes)

# break out fish into individual rows on the august table
# Remove extra columns
aug <- aug[ , c(1:7, 11, 13:48,54)]

# strip out Excel default date from StartTime, EndTime, PauseStart, and PauseEnd
aug$ObsTime <- gsub('1899-12-30 ', '', aug$ObsTime)

# Remove extra NA rows
aug <- aug[!is.na(aug$DiveNum),]

base <- 0

aug$anem_table_id <- NA
for (i in 1:nrow(aug)){
  aug$anem_table_id[i] <- aug$id[i] + base
}

aug$id <- NULL 


# split out the lines for individual fish ---------------------------------



# Get the Size1 fish
allfish <- aug[ , c("anem_table_id", "Spp", "Size1", "ID1", "Col1", "Recap1", "TagID1", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish <- allfish[!is.na(allfish$Size1), ]

# rename the columns
names(allfish) <- c("anem_table_id", "Spp", "Size", "ID", "Col", "Recap", "TagID", "Notes", "Collector")
names <- names(allfish)


# Get the Size2 fish
allfish2 <- aug[ , c("anem_table_id", "Spp", "Size2", "ID2", "Col2", "Recap2", "TagID2", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size2), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size3 fish
allfish2 <- aug[ , c("anem_table_id", "Spp", "Size3", "ID3", "Col3", "Recap3", "TagID3", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size3), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size4 fish
allfish2 <- aug[ , c("anem_table_id", "Spp", "Size4", "ID4", "Col4", "Recap4", "TagID4", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size4), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size5 fish
allfish2 <- aug[ , c("anem_table_id", "Spp", "Size5", "ID5", "Col5", "Recap5", "TagID5", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size5), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size6 fish
allfish2 <- aug[ , c("anem_table_id", "Spp", "Size6", "ID6", "Col6", "Recap6", "TagID6", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size6), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)

# Get the Size7 fish
allfish2 <- aug[ , c("anem_table_id", "Spp", "Size7", "ID7", "Col7", "Recap7", "TagID7", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size), ]

# Size7: may need to be split
if(length(allfish2)>0){
  i <- sapply(allfish2, is.factor); allfish2[i] = lapply(allfish2[i], as.character) # converts factors to characters
  allfish22 = allfish2[0,] # to hold the final values from Size7/ID7 (after disaggregating multiple entries on the same line)
  for(i in 1:nrow(allfish2)){ # check each row for multiple entries
    ids = gsub(' ', '', unlist(strsplit(as.character(allfish2$ID7[i]), split=','))) # split apart on comma and remove spaces
    sizes = gsub(' ', '', unlist(strsplit(as.character(allfish2$Size7[i]), split=','))) # split apart on comma
    cols = gsub(' ', '', unlist(strsplit(as.character(allfish2$Col7[i]), split=','))) # split apart on comma
    tags <- gsub(' ', '', unlist(strsplit(as.character(allfish2$TagID7[i]), split=','))) # split apart on comma
    if(length(sizes)>0){
      for(j in 1:length(sizes)){ # then check for non-NA, non-blank entries
        if(!is.na(sizes[j]) & sizes[j] != '' & sizes[j] != 'NA'){
          allfish22 = rbind(allfish22, allfish2[i,])
          allfish22$ID7[nrow(allfish22)] = ids[j]
          allfish22$Size7[nrow(allfish22)] = sizes[j]
          allfish22$Col7[nrow(allfish22)] = cols[j]
          allfish22$TagID7[nrow(allfish22)] = tags[j]
        }}
    }
  }
  names(allfish22)= names
  allfish <- rbind(allfish, allfish22)
}

rm(allfish2, allfish22)

allfish <- allfish[!is.na(allfish$anem_table_id), ]
strange <- allfish[which(nchar(allfish$TagID) != 15), ]

for (i in 26:nrow(strange)){
  strange$TagID[i] <- junefish$TagID[which(junefish$ID == strange$ID[i])]
}

write.csv(strange, file = paste("data/",Sys.Date(), "fixtagid.csv", sep = ""), row.names = F)

# remove the rows that we couldn't fix because they were caught after the date of file
strange <- strange[26:nrow(strange), ]

# reduce number of columns before the merge
strange <- strange[ , c(4,7)]

# pull in the november data, which has fixed the fish caught after june
clownfishcoltypes <- c("numeric", "numeric", "date", "text", "numeric", "numeric", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "numeric", "text", "text", "text", "numeric", "text", "text", "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "text")

nov <- readxl::read_excel("~/Documents/Philippines/Surveys_2016/GPSSurveys2016_20161104MRS.xlsx", sheet = "Clownfish", col_names=TRUE, col_types = clownfishcoltypes)

# break out fish into individual rows on the august table
# Remove extra columns
nov <- nov[ , c(1:7, 11, 13:48,54)]

# strip out Excel default date from StartTime, EndTime, PauseStart, and PauseEnd
nov$ObsTime <- gsub('1899-12-30 ', '', nov$ObsTime)

# Remove extra NA rows
nov <- nov[!is.na(nov$DiveNum),]

base <- 0

nov$anem_table_id <- NA
for (i in 1:nrow(nov)){
  nov$anem_table_id[i] <- nov$id[i] + base
}

nov$id <- NULL 


# split out the lines for individual fish ---------------------------------



# Get the Size1 fish
allfish <- nov[ , c("anem_table_id", "Spp", "Size1", "ID1", "Col1", "Recap1", "TagID1", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish <- allfish[!is.na(allfish$Size1), ]

# rename the columns
names(allfish) <- c("anem_table_id", "Spp", "Size", "ID", "Col", "Recap", "TagID", "Notes", "Collector")
names <- names(allfish)


# Get the Size2 fish
allfish2 <- nov[ , c("anem_table_id", "Spp", "Size2", "ID2", "Col2", "Recap2", "TagID2", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size2), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size3 fish
allfish2 <- nov[ , c("anem_table_id", "Spp", "Size3", "ID3", "Col3", "Recap3", "TagID3", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size3), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size4 fish
allfish2 <- nov[ , c("anem_table_id", "Spp", "Size4", "ID4", "Col4", "Recap4", "TagID4", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size4), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size5 fish
allfish2 <- nov[ , c("anem_table_id", "Spp", "Size5", "ID5", "Col5", "Recap5", "TagID5", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size5), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)


# Get the Size6 fish
allfish2 <- nov[ , c("anem_table_id", "Spp", "Size6", "ID6", "Col6", "Recap6", "TagID6", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size6), ]

# rename the columns
names(allfish2) <- names

# Join the 2 tables
allfish <- rbind(allfish, allfish2)

# Get the Size7 fish
allfish2 <- nov[ , c("anem_table_id", "Spp", "Size7", "ID7", "Col7", "Recap7", "TagID7", "Notes", "Collector")]

# remove the lines where no fish were observed
allfish2 <- allfish2[!is.na(allfish2$Size), ]

# Size7: may need to be split
if(length(allfish2)>0){
  i <- sapply(allfish2, is.factor); allfish2[i] = lapply(allfish2[i], as.character) # converts factors to characters
  allfish22 = allfish2[0,] # to hold the final values from Size7/ID7 (after disaggregating multiple entries on the same line)
  for(i in 1:nrow(allfish2)){ # check each row for multiple entries
    ids = gsub(' ', '', unlist(strsplit(as.character(allfish2$ID7[i]), split=','))) # split apart on comma and remove spaces
    sizes = gsub(' ', '', unlist(strsplit(as.character(allfish2$Size7[i]), split=','))) # split apart on comma
    cols = gsub(' ', '', unlist(strsplit(as.character(allfish2$Col7[i]), split=','))) # split apart on comma
    tags <- gsub(' ', '', unlist(strsplit(as.character(allfish2$TagID7[i]), split=','))) # split apart on comma
    if(length(sizes)>0){
      for(j in 1:length(sizes)){ # then check for non-NA, non-blank entries
        if(!is.na(sizes[j]) & sizes[j] != '' & sizes[j] != 'NA'){
          allfish22 = rbind(allfish22, allfish2[i,])
          allfish22$ID7[nrow(allfish22)] = ids[j]
          allfish22$Size7[nrow(allfish22)] = sizes[j]
          allfish22$Col7[nrow(allfish22)] = cols[j]
          allfish22$TagID7[nrow(allfish22)] = tags[j]
        }}
    }
  }
  names(allfish22)= names
  allfish <- rbind(allfish, allfish22)
}

# import all fish from database
clownfish <- leyte %>% tbl("clownfish") %>% collect()

# save a copy of the table
write.csv(clownfish, file = paste(Sys.time(), "clownfishtable.csv", sep = ""), row.names = F)

# finish prepping the allfish table for database format

# Create a sample ID
allfish$Sample_ID <- NA
for (i in 1:nrow(allfish)){
  if (!is.na(allfish$ID[i]) & allfish$ID[i] != "NA"){
    allfish$ID[i] <- formatC(as.numeric(allfish$ID[i]), width = 3, format = "d", flag = "0")
    allfish$Sample_ID[i] <- paste("APCL16_", allfish$ID[i], sep = "")
  }
}

# remove fish that don't have sample_ids
allfish <- allfish[!is.na(allfish$Sample_ID), ]

# update the clownfish table data with the fixed data
# test i <- 1
for (i in 1:nrow(allfish)){
  clownfish$tagid[which(clownfish$sample_id == allfish$Sample_ID[i])] <- allfish$TagID[i]
}

# reload the clownfish table into the database
library(RMySQL)
leyte <- dbConnect(MySQL(), dbname="Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# Send data to database
dbWriteTable(leyte,"clownfish",data.frame(clownfish), row.names = FALSE, overwrite = T)

dbDisconnect(leyte)
rm(leyte)
