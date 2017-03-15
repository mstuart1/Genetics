

###############################################
## compare genotypes at pairs of individuals
###############################################
source("code/readGenepop_space.R")
source("~/Documents/Philippines/Phil_code/conleyte.R")
library(RCurl)

# # read in list of troublesome samples to analyze instead of the totest list below
# comparisons <- ____

genfile = "~/Downloads/seq17_03_58loci.gen"

### add 'pop' to 3rd line of genepop file ###

dat = readGenepop(genfile)

ncol(dat)-2 # number of loci

# remove pop column from dat
dat$pop <- NULL

# # get list of recaptured individuals 

# can't use database right now, using fake data
recaps <- read.csv("~/Desktop/Workbook1.csv")

# leyte <- conleyte()
# recaps <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid)) %>% select(sample_id, capid) %>% collect() 

# widen the data
wide <- data.frame()
for (i in 1:nrow(recaps)){
  X <- recaps[recaps$capid == recaps$capid[i], ]
  X$indiv_2[1] <- as.character(X$sample_id[2])  # put the second indiv on the same line as the first
  if (nrow(X) > 2){
    X$indiv_3[1] <- as.character(X$sample_id[3])
  }else{
    X$indiv_3[1] <- NA
  }
  wide <- rbind(wide, X[1, ])
}
# change the column names to fit the code below
totest <- wide
names(totest) <- c("capid","ind1","ind2","ind3")

# strip genepop down to sample_id

# have to move the underscore to the right in sample_ids
dat$names <- paste("APCL", substr(dat$names,6,7), "_", substr(dat$names, 8, 10), sep = "")

	# to hold the results
a = rep(NA, nrow(totest))
out = data.frame(indivs = a, matches=a, mismatches=a, perc=a, hetmatch=a, hetmism=a, perchet=a)

for(i in 1:nrow(totest)){
	datrow = which(as.character(dat$names) %in% c(as.character(totest$ind1[i]), as.character(totest$ind2[i])))
	out$indivs[i] = paste(dat$names[datrow], collapse = ', ')

	genosone = dat[datrow[1], 3:ncol(dat)]
	genostwo = dat[datrow[2], 3:ncol(dat)]
	matches = genosone == genostwo # where the two genotypes match or not
	matches[genosone == '0000' | genostwo == '0000'] = NA # remove missing data from calculations

	out$matches[i] = sum(matches, na.rm=TRUE) # number of matching loci
	out$mismatches[i] = sum(!matches, na.rm=TRUE) # number of mismatching loci
	out$perc[i] = 100*signif(sum(!matches, na.rm=TRUE)/(sum(matches, na.rm=TRUE) + sum(!matches, na.rm=TRUE)),2) # proportion mismatching


	alone1 = substr(genosone, 1,2) # first allele in individual one
	alone2 = substr(genosone, 3,4) # second allele in individual one
	altwo1 = substr(genostwo, 1,2) # first allele in individual one
	altwo2 = substr(genostwo, 3,4) # second allele in individual one

	hets = (alone1 != alone2) | (altwo1 != altwo2)
	hets[alone1 == '00' | alone2 == '00' | altwo1 == '00' | altwo2 == '00'] = NA

	out$hetmatch[i] = sum(hets & matches, na.rm=TRUE) # number of matching heterozygote loci
	out$hetmism[i] = sum(hets & !matches, na.rm=TRUE) # number of mismatching loci where at least one indiv is het
	out$perchet[i] = 100*signif(sum(hets & !matches, na.rm=TRUE)/(sum(hets & !matches, na.rm=TRUE)+sum(hets & matches, na.rm=TRUE)),2)

}

out

write.csv(out, file="regenotyped_mismatch.csv")

onematch = (alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1) # does one allele match but not the other?
homvhet = ((alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1)) & (alone1 == alone2 | altwo1 == altwo2) # a onematch where one of the genotypes is a homozygote (hom vs. het mismatch)
sum(onematch)
sum(homvhet) # the same, if all one allele matches are hom vs het mismatches
sum(!onematch)

rbind(genosone[which(!matches)], genostwo[which(!matches)]) # visually inspect
rbind(genosone[which(!matches)][onematch], genostwo[which(!matches)][onematch]) # visually inspect cases where they match on one allele
rbind(genosone[which(!matches)][!onematch], genostwo[which(!matches)][!onematch]) # visually inspect where no alleles match




