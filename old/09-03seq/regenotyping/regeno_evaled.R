# Once the regenotyped files have been evaluated for "KEEP" vs "DROP", run this script to edit the genepop file and remove duplicate representations of samples

# Lightning
setwd('/Users/macair/Documents/Philippines/Genetics/regenotyping')
source('/Users/macair/Documents/Philippines/Genetics/code/readGenepop_space.R')
genfile <- '../rosetta stone genepop/2016-02-29_renamed.genepop'
library(RCurl)

# create a df of the evaluation of each regenotyped sample
eval <- read.csv('2016-02-29_num_loci_eval.csv')

### !!! not reading in the genepop properly, which means it isn't formatted properly
# create a df of the renamed_genepop
dat <- readGenepop(genfile)

# merge the decision with the genepop, make sure no files have been lost (nrow dat vs nrow decision)
dec <- merge(dat, eval[c('names', 'drop')], all.x = TRUE)
	nrow(dat)
	nrow(dec)

# # # TRUE = DROP, FALSE = KEEP
# # # convert all of the NA's to KEEP
dec$drop[is.na(dec$drop)] <- "KEEP"	
# summary(dec$drop)

# # remove all of the drop lines, keep the rest (make sure the nrows make sense)
# edited <- dec[!dec$drop,]

edited <- subset(dec, drop =="KEEP")
	nrow(dec)
	nrow(edited)

# remove the drop column
edited[,ncol(edited)] <- NULL

#### write out the new genepop ####
# Make the genepop header
msg <- c("This genepop file was generated using a script called regeno_evaled.R written by Michelle Stuart with help from Malin Pinsky")

# create a list of loci names, separated by commas
loci <- paste(names(edited[,3:ncol(edited)]), collapse =",")

# create a table of genotyped samples
gene <- vector()
sample <- vector()
for (i in 1:nrow(edited)){
	gene[i] <- paste(edited[i,3:ncol(edited)], collapse = " ")
	sample[i] <- paste(edited[i,1], gene[i], sep = ", ")
}

out <- c(msg, loci, 'pop', sample)

write.table(out, file = paste(Sys.Date(), 'noregenos.genepop', sep = '_'), row.names=FALSE, quote=FALSE, col.names=FALSE)

