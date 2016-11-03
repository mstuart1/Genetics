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

# create a idsimp format table to find fish that were caught in more than 2 years

# test i <- 1
# fields <- c("sample", "size", "anem", "site", "date")
# fields1 <- NA
# for (i in 1:length(fields)){
#   n <- paste(fields[i], 12:15, sep = "")
#   fields1 <- c(fields1, n)
# }
# fields1 <- fields1[2:21]
# 

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
write.csv(wide, file = paste("data/", Sys.Date(), "wide", sep = ""), row.names = F)
write.csv(idsimp, file = paste("data/", Sys.Date(),"idsimp.csv", sep = ""), row.names = F)
# idsimp <- read.csv("data/2016-11-03idsimp.csv", stringsAsFactors = F)

wide$year_12 <- 2012
wide$year_13 <- 2013
wide$year_14 <- 2014
wide$year_15 <- 2015


# make long again
# long <- reshape2::melt(wide, id.vars = c("fish", "size12", "anem12", "site12", "date12", "size13", "anem13", "site13", "date13", "site14", "date14", "anem14", "size14", "site15", "date15", "anem15", "size15", "year_12", "year_13", "year_14"  ,"year_15"), varialbe.name = "year", value.name = "sample")
# 
# # data1 <- melt(dm.data, id.vars = c("site", "nome"), variable.name = "year", value.name = "dm")

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

# save for later
write.csv(long, file = paste("data/", Sys.Date(), "long.csv", sep = ""), row.names = F)

# Malin wants graphs by year that show growth over 1 year
# this code creates 
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

# save for the night
write.csv(growth1, file = paste("data/", Sys.Date(), "growth1.csv", sep = ""), row.names = F)
write.csv(bigfish, file = paste("data/", Sys.Date(), "bigfish.csv", sep = ""), row.names = F)
write.csv(bigfish1, file = paste("data/", Sys.Date(), "bigfish1.csv", sep = ""), row.names = F)
