# sibs
# source("~/Documents/myRcode/Laboratory/R/conlabor.R")
source("code/sampforlig.R")

# connect to the lab database
# labor <- conlabor()

# read the text doc with full sibs:
sibs <- read.delim("data/michellenote.txt", sep = "\t", header = F)
names(sibs) <- c("sib1", "sib2", "prob")
sibs$sib1 <- as.character(sibs$sib1)
sibs$sib2 <- as.character(sibs$sib2)

# get sample ids for ligation ids
sib1 <- sampforlig(sibs$sib1)

sib2 <- sampforlig(sibs$sib2)

sibs <- left_join(sibs, sib1, by = c("sib1" = "ligation_id"))
names(sibs) <- c("sib1", "sib2", "prob", "sib1_samp")
sibs <- left_join(sibs, sib2, by = c("sib2" = "ligation_id"))
names(sibs) <- c("sib1", "sib2", "prob", "sib1_samp", "sib2_samp")

