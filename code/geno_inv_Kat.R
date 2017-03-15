###############################################
## compare genotypes between pairs of individuals
###############################################


# Set up working space ----------------------------------------------------

# setwd("~/Documents/GradSchool/parentage")
# source("~/Documents/GradSchool/parentage/readGenepop_space.R")
source("code/readGenepop_space.R")
source("code/sampfromlig.R")
source("../../myRcode/Laboratory/R/conlabor.R")
source("../Phil_code/conleyte.R")
library(RCurl)
suppressMessages(library(dplyr))



# Prepare the genepop -----------------------------------------------------

### add 'pop' to 3rd line of genepop file if it isn't already there ###
genfile <- "data/seq17_03_58loci_kat.gen"
# genfile <- "~/Documents/GradSchool/parentage/seq17_03_58loci.gen"
gen <- readGenepop(genfile)
ncol(gen)-2 # number of loci minus the pop, names

# remove the pop column
gen$pop <- NULL

### our genepops have inconsistent naming schemes.  Make sure the names column contains only 5 character ligation ids ###
for (i in 1:nrow(gen)){
  if (nchar(gen$names[i]) == 8){
  gen$names[i] <- substr(gen$names[i], 4, 8)
  }
  if (nchar(gen$names[i]) == 9){
    gen$names[i] <- substr(gen$names[i], 5, 9)
  }
}

# remove ligations with issues 
leyte <- conleyte()
iss <- leyte %>% tbl("known_issues") %>% collect()
gen <- gen %>% filter(!names %in% iss$Ligation_ID)

# cleanup
rm(iss, i, leyte)




# Add sample ids ----------------------------------------------------------
labor <- conlabor()
c5 <- sampfromlig(gen)

# labor <- src_mysql(dbname = "Laboratory", default.file = path.expand("/Users/kat1/Documents/GradSchool/parentage/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
# c1 <- labor %>% tbl("extraction") %>% select(extraction_id, sample_id)
# c2 <- labor %>% tbl("digest") %>% select(digest_id, extraction_id)
# c3 <- left_join(c2, c1, by = "extraction_id")
# c4 <- labor %>% tbl("ligation") %>% select(ligation_id, digest_id)
# c5 <- collect(left_join(c4, c3, by = "digest_id"))

dat <- left_join(gen, c5, by=c(names = "ligation_id"))

# make sure all of the Ligation ids have sample ids
which(is.na(dat$sample_id)) # 1972- L3118 has no sample id, is a mixture of samples

dat <- dat %>% filter(!is.na(dat$sample_id))

# clean up
rm (c5, gen, genfile, labor)





# Remove regenotyped samples ----------------------------------------------

# convert 0000 to NA in the genepop data
dat[dat == "0000"] = NA

# # TEST - make sure there are no "0000" left
# which(dat == "0000") # should return integer(0)

# count the number of loci per individual (have to use for loop)
for(i in 1:nrow(dat)){
  dat$numloci[i] <- sum(!is.na(dat[i,]))
}

### WAIT ###

# # TEST - make sure all of the numloci were populated
# which(is.na(dat$numloci)) # should return integer(0)

# make a list of all of the sample ID's that have duplicates (some on this list occur more than once because there are 3 regenos)
# this line of code keeps any sample_id that comes up as TRUE for being duplicated
dups <- dat %>%
  filter(duplicated(dat$sample_id)) %>%
  select(sample_id)

# make sure there is only one copy of each sample id
dups <- distinct(dups)

# # TEST - make sure a list was generated
k <- nrow(dups)
k # 96

dat$drop <- NA # place holder
#run through all of the SampleIDs that are found more than once and keep the one with the most loci
# for testing b <- 1
for(j in 1:nrow(dups)){
  # regeno_drop is the line number from dat that matches an ID in the regeno_match list
  regeno_drop <- which(dat$sample_id == dups[j,]) 
  # df is the data frame that holds all of the regenotyped versions of the sample, pulled from dat
  df <- dat[regeno_drop, ]
  # update the drop column so that the drop code below works
  df$drop[which.max(df$numloci)] <- "KEEP"
  # convert the drop column of large df to not na
  dat$drop[regeno_drop[which.max(df$numloci)]] <- "KEEP"
  
  
  # find the row numbers of dat that need to be dropped
  # test e <- 2
  for(e in 1:nrow(df)){
    if(is.na(df$drop[e])){
      dat$drop[regeno_drop[e]] <- "DROP"
    }
  }
}


# TEST - make sure all of the regenos were dropped ----------------------------
a <- length(which(dat$drop == "KEEP")) # num keeps
a == nrow(dups) # should return TRUE

# convert all of the KEEPs to NAs 
for(g in 1:nrow(dat)){
  if(!is.na(dat$drop[g]) && dat$drop[g]=="KEEP"){
    dat$drop[g] <- NA
  }
}

# create a new data frame with none of the "DROP" rows
dat <- dat[is.na(dat$drop),]
# TEST - make sure no drop rows made it
which(dat$drop == "DROP") # should return integer(0)
# TEST - check to see if there are any regenos that were missed
dat$sample_id[duplicated(dat$sample_id)]
# should return character(0)  
# If it doesn't, look deeper: noregeno[which(noregeno$SampleID == "APCL15_403"),], dat[which(dat$sample_ID == "APCL15_403"),]

# remove the extra columns from noregeno
dat[,c("drop", "numloci")] <- NULL

# convert all the NA genotypes to 0000
dat[is.na(dat)] = "0000"
# TEST - make sure there are no NA's left
which(is.na(dat)) # should return integer(0)

# cleanup
rm(df, dups, a, b, e, g, i, j, k, noregeno_match, regeno_drop)

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




