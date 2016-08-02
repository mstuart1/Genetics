# 7/27/2016 (MRS) - A script to examine all samples (no regenos or recaps have been removed from the genepop) to check for lab errors.  Uses the match table generated in identity_analysis.R

# Lightning
setwd('/Users/macair/Documents/Philippines/Genetics/identity')
source("../readGenepop_space.R")

# Import cervus identity results ------------------------------------------

idcsv <- read.csv("allsamples_ID.csv", stringsAsFactors = F)


# strip down to just ligation numbers -------------------------------------

idcsv$First.ID <- substr(idcsv$First.ID,11,15)
idcsv$Second.ID <- substr(idcsv$Second.ID,11,15)

# Connect to database -----------------------------------------------------

library(RMySQL)
leyte <- dbConnect(MySQL(), host="amphiprion.deenr.rutgers.edu", user="michelles", password="larvae168", dbname="Leyte", port=3306)
labor <- dbConnect(MySQL(), host="amphiprion.deenr.rutgers.edu", user="michelles", password="larvae168", dbname="Laboratory", port=3306)


# add lab IDs

lab <- dbSendQuery(labor, "select ligations.`ligation_ID`, digests.digest_ID, extractions.extraction_ID, extractions.Sample_ID from extractions 
join digests on extractions.extraction_ID = digests.extraction_ID
  join ligations on digests.digest_ID = ligations.digest_ID;")
lab <- fetch(lab, n=-1)

# For First.IDs
lab1 <- lab
names(lab1) <- paste("First.", names(lab1), sep = "")






# create a table that can be imported to update notes about samples that are not recaptures

examine <- match[which(match$date_eval == "FAIL"), ]

# create a table of regenotyped samples
regeno <- examine[which(examine$Second.Sample_ID == examine$First.Sample_ID), ]

# strip the number of ligation from the letter L for First half of match
regeno$First.lig <- substr(regeno$First.ID,2,5)

# strip the number of ligation from the letter L for Second half of match
regeno$Second.lig <- substr(regeno$Second.ID,2,5)

# Write a note about ligation
regeno$Note <- NA
for (i in 1:nrow(regeno)){
  if (regeno$First.lig[i] < regeno$Second.lig[i]){
    regeno$Note[i] <- paste(regeno$First.ID[i], "was regenotyped as", regeno$Second.ID[i])
  }else{
    regeno$Note[i] <- paste(regeno$Second.ID[i], "was regenotyped as", regeno$First.ID[i])
  }
}

# Add to note about source of regenotype
for (i in 1:nrow(regeno)){
  if (regeno$First.Digest_ID[i] != regeno$Second.Digest_ID[i]){
    regeno$Note[i] <- paste(regeno$Note[i], "from extract")
  }else{
      regeno$Note[i] <- paste(regeno$Note[i], "from digest")
  }
}

# split the data
out <- subset(regeno, select = c(First.ID, Note))
names <- names(out)

x <- subset(regeno, select = c(Second.ID, Note))
names(x) <- names

out <- rbind(out, x)

names(out) <- c("Ligation_ID", "Note")

# Write out a csv that can be imported and merged into the notes of the ligations table
write.csv(out, file = "regeno_notes.csv", row.names = F)

# remove regenotyped samples from examine
examine <- examine[which(examine$Second.Sample_ID != examine$First.Sample_ID), ]

### STOP ###

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
