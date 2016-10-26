# This script imports the read number data from the tsv created on amphiprion.
# It assigns Ligation ID to barcode
# It appends the existing data file that keeps a running tally of read number stats

# look up the data files
pools <- list.files(path = "data/", pattern = "process.log.tsv")
names <- list.files(path = "data/", pattern = "names-")

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
  read$seq <- 17
  read$pool <- substr(filename1, 6,7)
  read$barcode <- NULL
  read$percent_retained <- read$retained/read$total_reads
  reads <- rbind(reads, read)
}

# read in the old data
old <- read.csv("data/APCL_read_data.csv", stringsAsFactors = F)

# add new data
old <- rbind(old, reads)


write.csv(old, "data/APCL_read_data.csv", row.names = F)
