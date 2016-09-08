

###############################################
## compare genotypes at pairs of individuals
###############################################
# # Mr. Whitmore
# setwd('~/Documents/Rutgers/Philippines/Genetics/parentage/Cervus_2016-01-06/')
# Lightning
setwd('~/Documents/Philippines/Genetics/parentage/Cervus_2016-01-06/')

source('code/readGenepop_space.R')
library(RCurl)

genfile = '/Users/macair/Documents/Philippines/Genetics/DP10g95.genepop'
# genfile = '/Users/macair/Documents/Philippines/Genetics/L1055_L1648.genepop'
dat = readGenepop(genfile) #pop must be lowercase in order for ReadGenepop to work

ncol(dat)-2 # number of loci

totest = vector('list', 1)

totest[[1]] = data.frame(ind1 = 'APCL_13385L1668',  ind2 = 'APCL_13393L1667')

	# to hold the results
a = rep(NA, length(totest))
out = data.frame(indivs = a, matches=a, mismatches=a, perc=a, hetmatch=a, hetmism=a, perchet=a)

for(i in 1:length(totest)){
	datrow = which(as.character(dat$names) %in% c(as.character(totest[[i]]$ind1), as.character(totest[[i]]$ind2)))
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

write.csv(out, file=(paste("regenotyped_mismatch", Sys.Date(), ".csv")))

