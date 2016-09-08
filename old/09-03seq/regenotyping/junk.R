# This script is intended to be used after a genepop file has been modified by the script 'rosetta_genepop.R' to fix erroneous sample IDs.  

# this script can be run to find regenotyped and recaptured samples and determine which sampling event produced the most loci and determine genotype error rate.



###############################################
## compare genotypes at pairs of individuals
###############################################
# # Malin's computer
# setwd('/Users/mpinsky/Documents/Rutgers/Philippines/Genetics/genotyping/stacks_sensitivity_2015-07-17')

# Lightning
setwd('/Users/macair/Documents/Philippines/Genetics/regenotyping')
source('/Users/macair/Documents/Philippines/Genetics/code/readGenepop_space.R')
genfile <- '../rosetta stone genepop/2016-02-29_renamed.genepop'



library(RCurl)

# read in rosetta stone
library(googlesheets)
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1yhMEwka68eIAMbFKG4-KFWbmNb0JqzlML91mlX8mWj4' # for Rosetta Stone file
stone <-gs_key(mykey)
rosetta <-gs_read(stone, ws='Rosetta')

### change the names of samples to match the "fixed" genepop file

# make a copy of the rosetta df to mess with
renamed <- rosetta[c('names', 'Sample.ID', 'Reason','regenotype')]

# create an index of rows where the reason is not na and for the names in that index [inds], paste the sample id on with a _ in between
inds <- !is.na(renamed$Reason)
renamed$names[inds] <- paste(renamed$names[inds], renamed$Sample.ID[inds], sep='_')

# create a new df that only contains rows that have been regenotyped
regeno <- subset(renamed, regenotype=="TRUE")

dat <- readGenepop(genfile)

#### calculate the number of genotyped loci for each sample ####

### currently this doesn't include recaptures...have to update for that ### - maybe new script for matches supposing that in the future, samples won't have this much error in labeling :)

# convert 0000 to NA in the genepop data
dat[dat == "0000"] = NA

# merge list of regenotyped samples with the genepop file to fill in regenotyped loci that are genotyped
compare <- merge(regeno, dat) 

# ####count the number of snps per individual in the genepop only
# for(h in 1:nrow(dat)){
	# dat$numloci[h] <- sum(!is.na(dat[h,]))
# }
# num_snps <- dat[,c('names', 'numloci')]
# write.csv(num_snps, file = paste(Sys.Date(), 'num_snps.csv', sep = '_'))

# #look for missing names - loses some because those ligations didn't seq well 
# setdiff(regeno$names, dat$names)

for(h in 1:nrow(compare)){
	compare$numloci[h] <- sum(!is.na(compare[h,]))

}

# drop the loci columns for the csv

num_loci <- compare[,c('names', 'Sample.ID', 'numloci')]
num_loci_sorted <- order(num_loci$numloci, num_loci$names, num_loci$Sample.ID)

write.csv(num_loci, file = paste(Sys.Date(), 'num_loci.csv', sep = '_'))

##### to determine genotype error rate #####

# Loop through the regeno list and make a temporary table of individual regenotypes to compare matches, mismatches
		
		# to hold the results
a <- rep(NA, length(4))
out <- data.frame(indivs = a, matches=a, mismatches=a, perc=a, hetmatch=a, hetmism=a, perchet=a)
final <- c(out)

# create a temporary table of all of the regenotypes for a single sample
# !!! need to figure out to do all pairwise comparisons in temp, not just the top two
for (j in 1:nrow(regeno)){
	temp <- subset(regeno, Sample.ID == Sample.ID[j])
	# totest[[1]]=data.frame(ind1=temp$names[1], ind2=temp$names[2])
	for(i in 1:(nrow(temp)-1)){
		for(k in (i+1):nrow(temp)){
			totest <- data.frame(ind1=temp$names[i], ind2=temp$names[k])
			for(m in 1:nrow(totest)){
				datrow = which(as.character(dat$names) %in% c(as.character(totest[[m]]["ind1"]), as.character(totest[[m]]["ind2"])))
				out$indivs[m] = paste(dat$names[datrow], collapse = ', ')
				genosone = dat[datrow[1], 3:ncol(dat)]
				genostwo = dat[datrow[2], 3:ncol(dat)]
				matches = genosone == genostwo # where the two genotypes match 
				matches[genosone == '0000' | genostwo == '0000'] = NA # remove missing data from calculations
				
				out$matches[m] = sum(matches, na.rm=TRUE) # number of matching loci
				out$mismatches[m] = sum(!matches, na.rm=TRUE) # number of mismatching loci
				out$perc[m] = 100*signif(sum(!matches, na.rm=TRUE)/(sum(matches, na.rm=TRUE) + sum(!matches, na.rm=TRUE)),2) # proportion mismatching
				
				
				alone1 = substr(genosone, 1,2) # first allele in individual one
				alone2 = substr(genosone, 3,4) # second allele in individual one
				altwo1 = substr(genostwo, 1,2) # first allele in individual one
				altwo2 = substr(genostwo, 3,4) # second allele in individual one
				
				hets = (alone1 != alone2) | (altwo1 != altwo2)
				hets[alone1 == '00' | alone2 == '00' | altwo1 == '00' | altwo2 == '00'] = NA
				
				out$hetmatch[m] = sum(hets & matches, na.rm=TRUE) # number of matching heterozygote loci
				out$hetmism[m] = sum(hets & !matches, na.rm=TRUE) # number of mismatching loci where at least one indiv is het
				out$perchet[m] = 100*signif(sum(hets & !matches, na.rm=TRUE)/(sum(hets & !matches, na.rm=TRUE)+sum(hets & matches, na.rm=TRUE)),2)
				
				# ifelse(out$matches==!0, final <- rbind(final, out), out)
				final <- rbind(final, out)
				}
				
				}
				
		}
		
}



# sort by % mismatch, not working, gives only a column of numbers
 # final_sorted <- order(final$perc, final$indivs, final$matches, final$mismatches)
# final

write.csv(final, file = paste(Sys.Date(), 'error_rate.csv', sep = "_"))

# plot mismatch prop vs matches
png('~/Desktop/regeno.png')
plot(final$matches, final$perc, main = 'Proportion of mismatching loci compared to matching loci', xlab = 'Matching loci', ylab = 'Proportion mismatching loci', col = 'brown')
abline(h=0.005)
dev.off()
#### old code from Malin ####
# onematch = (alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1) # does one allele match but not the other?
# homvhet = ((alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1)) & (alone1 == alone2 | altwo1 == altwo2) # a onematch where one of the genotypes is a homozygote (hom vs. het mismatch)
# sum(onematch)
# sum(homvhet) # the same, if all one allele matches are hom vs het mismatches
# sum(!onematch)

# rbind(genosone[which(!matches)], genostwo[which(!matches)]) # visually inspect
# rbind(genosone[which(!matches)][onematch], genostwo[which(!matches)][onematch]) # visually inspect cases where they match on one allele
# rbind(genosone[which(!matches)][!onematch], genostwo[which(!matches)][!onematch]) # visually inspect where no alleles match




