# Open the csv from identity analysis and look for resolutions to issues

dat <- read.csv("data/2016-11-18_idanalyis.csv", stringsAsFactors = F)
z <- nrow(dat) #166

# find regenotyped samples
regeno <- dat[dat$First.sample_id == dat$Second.sample_id, ]
y <- nrow(regeno) #48

# remove regenotyped samples from dat
dat <- dat[dat$First.sample_id != dat$Second.sample_id, ]
nrow(dat) == z-y

# find the samples that are on the known issues list
