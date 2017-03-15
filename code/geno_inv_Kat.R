

###############################################
## compare genotypes at pairs of individuals
###############################################
# setwd("~/Documents/GradSchool/parentage")
# source("~/Documents/GradSchool/parentage/readGenepop_space.R")
source("code/readGenepop_space.R")
source("code/sampfromlig.R")
source("../../myRcode/Laboratory/R/conlabor.R")
source("../Phil_code/conleyte.R")
library(RCurl)
suppressMessages(library(dplyr))

# # read in list of troublesome samples to analyze instead of the totest list below
# comparisons <- ____

genfile <- "data/seq17_03_58loci_kat.gen"
# genfile <- "~/Documents/GradSchool/parentage/seq17_03_58loci.gen"
gen <- readGenepop(genfile)

### add 'pop' to 3rd line of genepop file ###
### the genepop uses ligation IDs, but this code uses sample ids, so need to make a genepop the uses sample ids or vice versa make this use ligation IDs. For simplicity, I will make a genepop with sample IDs

labor <- conlabor()
c5 <- sampfromlig(gen)


# labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
# c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
# c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
# c3 <- left_join(c2, c1, by = "extraction_id")
# c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
# c5 <- collect(left_join(c4, c3, by = "digest_id"))

dat <- left_join(gen, c5, by=c(names = "ligation_id"))

ncol(dat)-3 # number of loci minus the pop, names, and sample_id columns

# remove pop column from dat, remove extraction ID
dat$pop <- NULL
dat$extraction_id <- NULL

# clean up
rm (c5, gen, genfile, labor)

# remove repeat individuals
regeno <- dat[duplicated(dat$sample_id), c("sample_id", "names")]
noregeno <- anti_join(dat, regeno, by = "sample_id")

# get list of recaptured individuals --------------------------------------

leyte <- conleyte()
# leyte <- src_mysql(dbname = "Leyte", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

recaps <- leyte %>% tbl("clownfish") %>% filter(!is.na(capid)) %>% select(sample_id, capid) %>% collect() 

# widen the data (for each capid, include all fish in one row)
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
names(totest) <- c("ind1","capid","ind2","ind3")
totest <- distinct(totest)

# cleanup
rm(recaps, wide, X, i, leyte)

# remove pairs of individuals who are not in the genepop




# strip genepop down to sample_id

# might have to move the underscore to the right in sample_ids - don't have to do this with your current genepop Katrina
# dat$sample_id <- paste("APCL", substr(dat$sample_id,6,7), "_", substr(dat$sample_id, 8, 10), sep = "") #I didn't do this, because when I did underscores erased the year portion of my sample IDs, and that didn't seem right

	# to hold the results
a <- rep(NA, nrow(totest))
out <- data.frame(indivs = a, matches=a, mismatches=a, perc=a, hetmatch=a, hetmism=a, perchet=a)
X <- out[1, ]
out <- X
for(i in 1:nrow(totest)){
	datrow <- which(as.character(dat$sample_id) %in% c(as.character(totest$ind1[i]), as.character(totest$ind2[i])))
	# make sure both individuals are in the genepop
	if (length(datrow) > 1){
	X$indivs <- paste(dat$sample_id[datrow], collapse = ', ')

	genosone <- dat[datrow[1], 3:ncol(dat)]#works
	genostwo <- dat[datrow[2], 3:ncol(dat)]#works
	matches <- genosone == genostwo # where the two genotypes match or not #works
	matches[genosone == '0000' | genostwo == '0000'] <- NA # remove missing data from calculations #works

	X$matches <- sum(matches, na.rm=TRUE) # number of matching loci
	X$mismatches <- sum(!matches, na.rm=TRUE) # number of mismatching loci  
	X$perc <- 100*signif(sum(!matches, na.rm=TRUE)/(sum(matches, na.rm=TRUE) + sum(!matches, na.rm=TRUE)),2) # proportion mismatching


	alone1 <- substr(genosone, 1,2) # first allele in individual one
	alone2 <- substr(genosone, 3,4) # second allele in individual one
	altwo1 <- substr(genostwo, 1,2) # first allele in individual one
	altwo2 <- substr(genostwo, 3,4) # second allele in individual one

	hets <- (alone1 != alone2) | (altwo1 != altwo2)
	hets[alone1 == '00' | alone2 == '00' | altwo1 == '00' | altwo2 == '00'] <- NA

	X$hetmatch <- sum(hets & matches, na.rm=TRUE) # number of matching heterozygote loci
	X$hetmism <- sum(hets & !matches, na.rm=TRUE) # number of mismatching loci where at least one indiv is het
	X$perchet <- 100*signif(sum(hets & !matches, na.rm=TRUE)/(sum(hets & !matches, na.rm=TRUE)+sum(hets & matches, na.rm=TRUE)),2)
	out <- rbind(out, X)
}
}

out

write.csv(out, file="regenotyped_mismatch.csv")

onematch <- (alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1) # does one allele match but not the other?
homvhet <- ((alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1)) & (alone1 == alone2 | altwo1 == altwo2) # a onematch where one of the genotypes is a homozygote (hom vs. het mismatch)
sum(onematch)
sum(homvhet) # the same, if all one allele matches are hom vs het mismatches
sum(!onematch)

rbind(genosone[which(!matches)], genostwo[which(!matches)]) # visually inspect
rbind(genosone[which(!matches)][onematch], genostwo[which(!matches)][onematch]) # visually inspect cases where they match on one allele
rbind(genosone[which(!matches)][!onematch], genostwo[which(!matches)][!onematch]) # visually inspect where no alleles match




