# this script takes the output csv of identity analysis and creates a table to track fish over time.

# Connect to database
suppressMessages(library(dplyr))
leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)


# Initial Setup -----------------------------------------------------------


# import the results of the identity_analysis script used to remove recaptured fish from parentage
filename <- "identity/2016-11-02_idanalyis.csv"

idcsv <- read.csv(filename, stringsAsFactors = F)

# strip down to sample_id and field data
idsimp <- idcsv[ , c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon", "First.ID", "Second.ID")]

# pull anem_id from database
suppressWarnings(c1 <- leyte %>% tbl("anemones") %>% select(anem_table_id, anem_id, old_anem_id) %>% collect()) 

idsimp <- left_join(idsimp, c1, by = c("First.anem_table_id" = "anem_table_id"))
colnames(idsimp) <- c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon", "First.ID", "Second.ID", "first_anem_id", "first_old_anem_id")

idsimp <- left_join(idsimp, c1, by = c("Second.anem_table_id" = "anem_table_id"))
colnames(idsimp) <- c("First.sample_id", "First.anem_table_id", "First.fish_table_id", "First.Size", "First.dive_table_id", "First.ObsTime",  "First.id", "First.Date", "First.Name", "First.lat",  "First.lon","Second.sample_id", "Second.anem_table_id", "Second.fish_table_id", "Second.Size",  "Second.dive_table_id", "Second.ObsTime", "Second.id",  "Second.Date", "Second.Name",  "Second.lat", "Second.lon", "First.ID", "Second.ID", "first_anem_id", "first_old_anem_id", "second_anem_id", "second_old_anem_id")

rm(c1)

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


# widen the table to find fish that were caught in more than 2 years

idsimp$sample12 <- NA
 idsimp$sample13 <- NA
 idsimp$sample14 <- NA
 idsimp$sample15 <- NA
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
 

 twel <- grep("12", colnames(idsimp))
 thirt <- grep("13", colnames(idsimp)) 
 four <- grep("14", colnames(idsimp))
fift <-  grep("15", colnames(idsimp))
 
wide <- idsimp[ , c(twel, thirt, four, fift)]
wide$fish <- idsimp$fish
 
twel <- grep("12", colnames(wide))
thirt <- grep("13", colnames(wide)) 
four <- grep("14", colnames(wide))
fift <-  grep("15", colnames(wide))

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


# # save wide and idsimp for later
# write.csv(wide, file = paste("data/", Sys.Date(), "wide", sep = ""), row.names = F)
# write.csv(idsimp, file = paste("data/", Sys.Date(),"idsimp.csv", sep = ""), row.names = F)
# idsimp <- read.csv("data/2016-11-03idsimp.csv", stringsAsFactors = F)

wide$year_12 <- 2012
wide$year_13 <- 2013
wide$year_14 <- 2014
wide$year_15 <- 2015


# make long again

# pull out each set of data by year
twelve <- wide[ , grep("12", colnames(wide))]
twelve$fish <- wide$fish
twelve <- twelve[!is.na(twelve$sample12), ]
colnames(twelve) <- c("sample", "size", "anem", "site", "date", "year", "fish")

thirteen <- wide[ , grep("13", colnames(wide))]
thirteen$fish <- wide$fish
thirteen <- thirteen[!is.na(thirteen$sample13), ]
colnames(thirteen) <- c("sample", "size", "anem", "site", "date", "year", "fish")

fourteen <- wide[ , grep("14", colnames(wide))]
fourteen$fish <- wide$fish
fourteen <- fourteen[!is.na(fourteen$sample14), ]
colnames(fourteen) <- c("sample", "site", "date", "anem","size", "year", "fish")

fifteen <- wide[ , grep("15", colnames(wide))]
fifteen$fish <- wide$fish
fifteen <- fifteen[!is.na(fifteen$sample15), ]
colnames(fifteen) <- c("sample", "site", "date", "anem","size", "year", "fish")

long <- rbind(twelve, thirteen, fourteen, fifteen)
long$growth <- NA

# long is a table with a fish id number for each fish, which is repeated every time that fish was caught

# save for later
# write.csv(long, file = paste("data/", Sys.Date(), "long.csv", sep = ""), row.names = F)
# long <- read.csv("data/2016-11-03long.csv", stringsAsFactors = F)



# GROWTH ------------------------------------------------------------------
# read in "long" data file

################################################################
#############           GROWTH           #######################

# # calculate how much a fish grew over time
# idsimp$growth <- idsimp$Second.Size - idsimp$First.Size
# 
# # test i <- 11
# for (i in 1:nrow(idsimp)){
#   if(idsimp$Second.Date[i] < idsimp$First.Date[i]){
#     idsimp$growth[i] <- abs(idsimp$growth[i])
#   }
# }


# Malin wants graphs by year that show growth over 1 year
# this code adds a growth number to the most recent catch of the fish, showing how much it grew in 1 year
for (i in 1:max(long$fish)){
  X <- subset(long, long$fish == long$fish[i])
  if(nrow(X) > 1){
    # sort in order of year
    X <- X[order(X$year), ]
    if(X$year[2] == X$year[1]+1){
      X$growth[2] <- X$size[2] - X$size[1]
      long$growth[which(long$sample == X$sample[2])] <- X$growth[2]
    }
  }
}
twice <- subset(long, !is.na(long$growth))
plot(twice$size, twice$growth)

# repeat to account for if there are three rows in X
for (i in 1:max(long$fish)){
  X <- subset(long, long$fish == long$fish[i])
  if(nrow(X) > 2){
    # sort in order of year
    X <- X[order(X$year), ]
    if(X$year[3] == X$year[2]+1){
      X$growth[3] <- X$size[3] - X$size[2]
      long$growth[which(long$sample == X$sample[3])] <- X$growth[3]
    }
  }
}
three <- subset(long, !is.na(long$growth))
# there was no difference in number of rows between twice and three

plot(three$size, three$growth, ylab = "delta growth", xlab = "size in cm", main = "Change in one year of growth of clownfish plotted against size")
regr <-lm(growth~size, data=three)
summary(regr)
abline(coef = coef(regr))
# three plot is saved in the plots directory

# next need to pull in tail color for samples with growth from database
tail <- leyte %>% tbl("clownfish") %>% select(sample_id, col)

growth <- left_join(three, tail, by = c("sample" = "sample_id"), copy = T)

growth$color[growth$col == "O"] <- "#D53E4F"
growth$color[growth$col != "O"] <- "#3288BD"
plot(growth$size, growth$growth, ylab = "Growth (cm)", xlab = "Size (cm)", main = "Change in one year of growth of clownfish plotted against size", col= growth$color, xlim = c(4,15), ylim = c(-3, 10), pch = 16, cex = 0.75)
regr <-lm(growth~size, data=growth)
summary(regr)
abline(coef = coef(regr), lty = 3)
abline(h = 0)

# the next step is to see if there is a larger fish on the anemone
# pull all of the fish data from the database and match up anem_id and old_anem_id
bigfish <- leyte %>% tbl("clownfish") %>% select(anem_table_id, sample_id, size) %>% collect()
bigfish <- bigfish[!is.na(bigfish$sample_id), ]
anems <- leyte %>% tbl("anemones") %>% filter(anem_table_id %in% bigfish$anem_table_id) %>% select(anem_table_id, anem_id, old_anem_id) %>% collect()

bigfish1 <- left_join(bigfish, anems, by = "anem_table_id")
bigfish <- bigfish1[ , c("sample_id", "anem_id", "old_anem_id")]


# join growth and big fish
growth1 <- left_join(growth, bigfish, by = c("sample" = "sample_id"))
growth1$biggest <- NA

# find other fish on same anemone
bigger <- c()
for (i in 1:nrow(growth1)){
    X <- subset(bigfish1, bigfish1$anem_id == growth1$anem_id[i])
    if(nrow(X) > 1){
      # find a bigger fish
      for (j in 1:nrow(X)){
        if (X$size[j] > growth1$size[i]){
          growth1$biggest[i] <- FALSE
          bigger <- c(bigger, X$sample_id[j])
        }
      }
    }
    if(!is.na(X$old_anem_id[i])){
      Y <- subset(bigfish, bigfish$anem_id == growth1$old_anem_id[i])
      if(nrow(Y) > 0){
        print(growth1$sample[i])
      }
    }
}

# 36 fish have bigger fish on the anemone

# # save for the night
# write.csv(growth1, file = paste("data/", Sys.Date(), "growth1.csv", sep = ""), row.names = F)
# write.csv(bigfish, file = paste("data/", Sys.Date(), "bigfish.csv", sep = ""), row.names = F)
# write.csv(bigfish1, file = paste("data/", Sys.Date(), "bigfish1.csv", sep = ""), row.names = F)

########################################################################
# Malin wants me to look at the PIT data and compare
tag <- leyte %>% tbl("clownfish") %>% filter(!is.na(tagid)) %>% select(tagid, sample_id, size, col, recap, anem_table_id, fish_table_id) %>% collect()

# find dates for anem_table_ids
dive <- leyte %>% tbl("anemones") %>% select(anem_table_id, dive_table_id)
suppressWarnings(date <- leyte %>% tbl("diveinfo") %>% select(id, date))
date <- left_join(dive, date, by = c("dive_table_id" = "id"))
tag <- left_join(tag, date, by = "anem_table_id", copy = T)
tag$dive_table_id <- NULL
tag$id <- NULL
tag$anem_table_id <- NULL
tag$growth <- NA

tag$year <- substr(tag$date, 1, 4)

# Find all of the repeating tags test i <- 1
for (i in 1:nrow(tag)){
  X <- subset(tag, tag$tagid == tag$tagid[i])
  if(nrow(X) > 1){
    # sort in order of year
    X <- X[order(X$date), ]
    if(X$year[2] > X$year[1]){
      X$growth[2] <- X$size[2] - X$size[1]
      tag$growth[which(tag$fish_table_id == X$fish_table_id[2])] <- X$growth[2]
    }
  }
}
twice <- subset(tag, !is.na(tag$growth))
# there was no difference in number of rows between twice and three

############################################################################
plot(twice$size, twice$growth, ylab = "delta growth", xlab = "size in cm", main = "Change in one year of growth of clownfish plotted against size")
regr <-lm(growth~size, data=twice)
summary(regr)
abline(coef = coef(regr))
abline(h = 0)

# no difference when trying for fish caught 3 times because they were caught once in 2015 and twice in 2016.


# include tail color for samples with growth from database
# tail <- leyte %>% tbl("clownfish") %>% select(fish_table_id, col)

# twice <- left_join(twice, tail, by = "fish_table_id", copy = T)

twice$color[twice$col == "O"] <- "#D53E4F"
twice$color[twice$col != "O"] <- "#3288BD"

plot(twice$size, twice$growth, ylab = "Growth (cm)", xlab = "Size (cm)", main = "Change in one year of growth of clownfish plotted against size", col= twice$color, xlim = c(4,15), ylim = c(-3, 10), pch = 16, cex = 0.75, bty = "l")
regr <-lm(growth~size, data=twice)
summary(regr)
abline(coef = coef(regr), lty = 3)
abline(h = 0)
legend("topleft", legend = c("Female", "Male"), fill = c("#D53E4F", "#3288BD"), cex = 1, bty = "n")



# plotting growth against initial size ------------------------------------


#############################################
# 2016-11-08 the difference with Malin's graphs was that he plotted initial size and subsequent growth and I plotted final size and overall growth

# plotting Malin's way
long <- read.csv("data/2016-11-14long.csv", stringsAsFactors = F)

# Malin wants graphs by year that show growth over 1 year
# for all of the fish that were recaptured
for (i in 1:max(long$fish)){
  # pull out all of the rows with matching fish numbers
  X <- subset(long, long$fish == long$fish[i])
  # if the fish was caught in more than one year
  if(nrow(X) > 1){
    # sort in order of year
    X <- X[order(X$year), ]
    # if there is only one year separating the measurements
    if(X$year[2] == X$year[1]+1){
      # assign growth as the second measurement minus the first
      X$growth[1] <- X$size[2] - X$size[1]
      # assign the growth value to the table of all fish to the first measurement
      long$growth[which(long$sample == X$sample[1])] <- X$growth[1]
    }
  }
}
# all fish caught twice (68 fish)
twice <- subset(long, !is.na(long$growth))
plot(twice$size, twice$growth)

# repeat to account for if there are three rows in X
for (i in 1:max(long$fish)){
  X <- subset(long, long$fish == long$fish[i])
  if(nrow(X) > 2){
    # sort in order of year
    X <- X[order(X$year), ]
    if(X$year[3] == X$year[2]+1){
      X$growth[2] <- X$size[3] - X$size[2]
      long$growth[which(long$sample == X$sample[2])] <- X$growth[2]
    }
  }
}
three <- subset(long, !is.na(long$growth))
# 72 fish - 68 fish = 4 fish were caught 3 times between 2012 and 2015.

plot(three$size, three$growth, ylab = "delta growth", xlab = "size in cm", main = "Change in one year of growth of clownfish plotted against size")
regr <-lm(growth~size, data=three)
summary(regr)
abline(coef = coef(regr))
abline(h=0)
# three plot is saved in the plots directory

tail <- leyte %>% tbl("clownfish") %>% select(sample_id, col)

three <- left_join(three, tail, by = c("sample" = "sample_id"), copy = T)

three$color[three$col == "O"] <- "#D53E4F"
three$color[three$col != "O"] <- "#3288BD"
plot(three$size, three$growth, ylab = "Growth (cm)", xlab = "Initial Size (cm)", main = "Change in one year of growth of clownfish plotted against size", col= three$color, xlim = c(0,15), ylim = c(-3, 10), pch = 16, cex = 0.75)
regr <-lm(growth~size, data=three)
summary(regr)
abline(coef = coef(regr), lty = 3)
abline(h = 0)

# the next step is to see if there is a larger fish on the anemone
# pull all of the fish data from the database and match up anem_id and old_anem_id
bigfish <- leyte %>% tbl("clownfish") %>% select(anem_table_id, sample_id, size) %>% collect()
# remove all fish without a sample ID
bigfish <- bigfish[!is.na(bigfish$sample_id), ]
# pull in anemone id info
anems <- leyte %>% tbl("anemones") %>% filter(anem_table_id %in% bigfish$anem_table_id) %>% select(anem_table_id, anem_id, old_anem_id) %>% collect()

# connect sample ids and anem ids
bigfish1 <- left_join(bigfish, anems, by = "anem_table_id")
# reduce number of columns
bigfish <- bigfish1[ , c("sample_id", "anem_id", "old_anem_id")]

# join growth and big fish
three1 <- left_join(three, bigfish, by = c("sample" = "sample_id"))

# create a column for biggest fish
three1$biggest <- NA

# find other fish on same anemone

# create an empty data frame
bigger <- c()

# for all of the fish in the growth analysis
for (i in 1:nrow(three1)){
  # pull all of the rows from all fish who share an anemone id
  X <- subset(bigfish1, bigfish1$anem_id == three1$anem_id[i])
  # if there is more than one fish on that anemone
  if(nrow(X) > 1){
    # find a bigger fish
    for (j in 1:nrow(X)){
      # if this fish is bigger than our recaptured fish
      if (X$size[j] > three1$size[i]){
        # mark our recaputred fish as not biggest
        three1$biggest[i] <- FALSE
        # put the bigger fish into a dataframe
        bigger <- c(bigger, X$sample_id[j])
      }
    }
  }
  if(!is.na(X$old_anem_id[i])){
    Y <- subset(bigfish, bigfish$anem_id == three1$old_anem_id[i])
    if(nrow(Y) > 0){
      print(three1$sample[i])
    }
  }
}

# 36 fish have bigger fish on the anemone
library(RColorBrewer)
colors <- brewer.pal(n = 11, "Spectral")
display.brewer.pal(n = 11, "Spectral")
colors

three1$color[three1$col == "O"] <- "#F46D43"
three1$color[three1$col != "O"] <- "#66C2A5"
three1$color[three$col == "O" & three1$biggest == "FALSE"] <- "#9E0142"
three1$color[three$col != "O" & three1$biggest == "FALSE"] <- "#5E4FA2"


plot(three$size, three1$growth, ylab = "Growth (cm)", xlab = "Initial Size (cm)", main = "Change in one year of growth of clownfish plotted against size", col= three1$color, xlim = c(0,15), ylim = c(-3, 10), pch = 16, cex = 0.75)
regr <-lm(growth~size, data=three1)
summary(regr)
abline(coef = coef(regr), lty = 3)
abline(h = 0)
legend("topleft", legend = c("Male Biggest", "Female Biggest", "Male", "Female"), fill = c("#5E4FA2", "#9E0142", "#66C2A5", "#F46D43" ), cex = 0.75, bty = "n")

########################################################
# plot the growth of a fish 

# create a list of colors for the fish
spectral <- colorRampPalette(brewer.pal(n = 11, name = "Spectral"))(max(long$fish))
plot(1:max(long$fish), rep(1,max(long$fish)), pch = 15, cex = 50, col = spectral)

# make a fake graph to hold the data (have to do this so the x axis is long enough)
i <- 43
X <- subset(long, long$fish == long$fish[i])
X[4, ] <- c("APCL12_fake", 10, 0, "Tamakin Fake", "2012-05-01", 2012, 16, NA)
X$date <- as.Date(X$date)
X <- X[order(X$date), ]

plot(X$date, X$size, ylab = "Size (cm)", xlab = "Date", type = "b", col= "white", pch = 16, bty = "l", ylim = c(0, 15))

# take a look at fish that were caught more than twice

for (i in 1:max(long$fish)){
  X <- subset(long, long$fish == long$fish[i])
  if(nrow(X) > 2){
    X$date <- as.Date(X$date)
    points(X$date, X$size, col= spectral[i], type = "b", pch = 16)
  }
}

# it looks like once fish get larger than 5cm, they don't grow much.  Look at just fish smaller than 5 cm, except APCL12_189/APCL13_060 (fish 49).  This fish is small and stays small.  
for (i in 1:max(long$fish)){
  X <- subset(long, long$fish == long$fish[i])
  if(X$size[1] <= 5){
    # print(i)
    X$date <- as.Date(X$date)
    points(X$date, X$size, col= spectral[i], type = "b", pch = 16)
  }
}

# all of the less than 5cm fish have lines with a slope

# clear graph
# for fish larger than 5cm
for (i in 1:max(long$fish)){
  X <- subset(long, long$fish == long$fish[i])
  if(X$size[1] >= 5){
    X$date <- as.Date(X$date)
    points(X$date, X$size, col= spectral[i], type = "b", pch = 16)
  }
}

# lots of these are horizontal lines except for the 2014-2015 data

