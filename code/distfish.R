# what is the average distance between capture events for the same fish?

source("../code/conleyte.R")
leyte <- conleyte()

# find fish with capids
fish <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid)) %>% select(sample_id, anem_table_id, capid) %>% collect()

# get the lat longs
anem <- leyte %>% tbl("anemones") %>% filter(anem_table_id %in% fish$anem_table_id) %>% select(dive_table_id, ObsTime, anem_table_id) %>% collect() 
fish <- left_join(fish, anem, by = "anem_table_id")
rm(anem)
suppressWarnings(dive <- leyte %>% tbl("diveinfo") %>% filter(id %in% fish$dive_table_id) %>% select(id, date, name) %>% collect())
fish <- left_join(fish, dive, by = c("dive_table_id" = "id"))
latlong <- leyte %>% tbl("GPX") %>% collect()
# Add lat long  i <- 1
for(i in 1:nrow(fish)){  
  #Get date and time information for the anemone
  date <- as.character(fish$date[i])
  datesplit <- strsplit(date,"-", fixed = T)[[1]]
  year <- as.numeric(datesplit[1])
  month <- as.numeric(datesplit[2])
  day <- as.numeric(datesplit[3])
  time <- as.character(fish$ObsTime[i])
  timesplit <- strsplit(time, ":", fixed = T)[[1]]
  hour <- as.numeric(timesplit[1])
  min <- as.numeric(timesplit[2])
  sec <- as.numeric(timesplit[3])
  
  # Convert time to GMT
  hour <- hour - 8
  if(!is.na(hour) & hour < 0){
    day <- day - 1
    hour <- hour + 24
  }
  
  # Find the location records that match the date/time stamp (to nearest second)
  latlongindex <- which(latlong$year == year & latlong$month == month & latlong$day == day & latlong$hour == hour & latlong$min == min)
  i2 <- which.min(abs(latlong$sec[latlongindex] - sec))
  
  # Calculate the lat/long for this time
  if(length(i2)>0){
    fish$lat[i] = latlong$lat[latlongindex][i2]
    fish$lon[i] = latlong$long[latlongindex][i2]
  }
}

# calculate the distance between capture events
distable <- data.frame()

for (i in 1:max(fish$capid)){
  X <- subset(fish, fish$capid == i)
  
}
  