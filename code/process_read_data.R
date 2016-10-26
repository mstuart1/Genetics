# This script imports the read number data from the tsv created on amphiprion.
# It assigns Ligation ID to barcode
# It appends the existing data file that keeps a running tally of read number stats

# look up the data files
pools <- list.files(path = "data/", pattern = "process.log.tsv")
names <- list.files(path = "data/", pattern = "names-")
seq <- 17

# create an empty dataframe
reads <- data.frame()


# assign the nth file to filename
# read in the data file
# rename the columns
# assign the name file to a filename1
# read in the names for the barcodes in the above file
# rename columns in name file
# add the ligation names to the read data
# add the finished product to the running tally

# TODO: grep the file names to make sure that for example pool 69 data is being matched to pool69 names
for (i in 1:length(pools)){
  filename1 <- paste("data/", pools[i], sep = "")
  data <- read.delim(filename1, header = F)
  colnames(data) <- c("barcode", "total_reads", "no_rad_tag", "low_quality", "retained")
  filename2 <- paste("data/", names[i], sep = "")
  name <- read.delim(filename2, header = F)
  colnames(name) <- c("ligation_id", "barcode")
  read <- dplyr::left_join(data, name, by = "barcode")
  read$seq <- seq
  read$pool <- substr(filename1, 6,7)
  read$barcode <- NULL
  read$percent_retained <- read$retained/read$total_reads
  reads <- rbind(reads, read)
}

# read in the old data
old <- read.csv("data/APCL_read_data.csv", stringsAsFactors = F)

# add new data
old <- rbind(old, reads)

# write to file to keep running tally
write.csv(old, "data/APCL_read_data.csv", row.names = F)

# analyze the read data
plot(x = old$total_reads, y = old$percent_retained)

# the above plot shows that most of our samples retain the majority of their reads.
# Let's make a histogram of total reads to see where our low end performers are
brk <- seq(0,10000000,100000)
x <- hist(old$total_reads, breaks = brk)


# I want to take a look at the low performers
plot(x = old$total_reads, y = old$percent_retained, type = "p", xlim = c(0,50000))

# I want a list of samples where total reads are less than 25,000
suppressMessages(library(dplyr))
old_low_tot <- old %>% filter(total_reads < 50000)

# These samples need to be evaluated to see if they have been genotyped in the past or if they need to be regenotyped
write.csv(old_low_tot, file = "data/low_performers.csv", row.names = F)
