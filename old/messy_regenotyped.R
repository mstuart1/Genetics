

###############################################
## compare genotypes at pairs of individuals
###############################################
# # Malin's computer
# setwd('/Users/mpinsky/Documents/Rutgers/Philippines/Genetics/genotyping/stacks_sensitivity_2015-07-17')

# Lightning
setwd('/Users/macair/Documents/Philippines/Genetics/regenotyping')
source('/Users/macair/Documents/Philippines/Genetics/code/readGenepop_space.R')
library(RCurl)

# genfile = '../lax-rxstacks_2015-06-17/batch_1.r75m5.genepop' # original
# genfile = 'laxnor-oldcat/r75m5.genepop' # small set of 23, no -r in process_radtags, original catalog
# genfile = 'laxnor/batch_1.genepop' # small  set of 23, no -r in process_radtags, de novo catalog
# genfile = 'default/batch_1.genepop' # small  set of 23, no -r in process_radtags, de novo catalog
genfile <- '/Users/macair/Documents/Philippines/Genetics/DP10g95.genepop'

dat <- readGenepop(genfile)

ncol(dat)-2 # number of loci

	# set up individuals to compare
totest = vector('list', 169)

totest[[1]]=data.frame(ind1='APCL_14497L1273', ind2='APCL_14497L1750')
totest[[2]]=data.frame(ind1='APCL_13003L0449', ind2='APCL_13003L1673')
totest[[3]]=data.frame(ind1='APCL_13005L0455', ind2='APCL_13005L1675')
totest[[4]]=data.frame(ind1='APCL_13043L0831', ind2='APCL_13043L1687')
totest[[5]]=data.frame(ind1='APCL_13235L0834', ind2='APCL_13235L1690')
totest[[6]]=data.frame(ind1='APCL_13236L0269', ind2='APCL_13236L1628')
totest[[7]]=data.frame(ind1='APCL_13277L0310', ind2='APCL_13277L1629')
totest[[8]]=data.frame(ind1='APCL_13280L0313', ind2='APCL_13280L1630')
totest[[9]]=data.frame(ind1='APCL_13301L0332', ind2='APCL_13301L1631')
totest[[10]]=data.frame(ind1='APCL_13324L0351', ind2='APCL_13324L1663')
totest[[11]]=data.frame(ind1='APCL_13327L0642', ind2='APCL_13327L1664')
totest[[12]]=data.frame(ind1='APCL_13328L0835', ind2='APCL_13328L1691')
totest[[13]]=data.frame(ind1='APCL_13339L1306', ind2='APCL_13339L1692')
totest[[14]]=data.frame(ind1='APCL_13340L0361', ind2='APCL_13340L1632')
totest[[15]]=data.frame(ind1='APCL_13347L0836', ind2='APCL_13347L1693')
totest[[16]]=data.frame(ind1='APCL_13348L0367', ind2='APCL_13348L1633')
totest[[17]]=data.frame(ind1='APCL_13352L0837', ind2='APCL_13352L1659')
totest[[18]]=data.frame(ind1='APCL_13353L0838', ind2='APCL_13353L1694')
totest[[19]]=data.frame(ind1='APCL_13357L0839', ind2='APCL_13357L1651')
totest[[20]]=data.frame(ind1='APCL_13364L0378', ind2='APCL_13364L1634')
totest[[21]]=data.frame(ind1='APCL_13367L0381', ind2='APCL_13367L1665')
totest[[22]]=data.frame(ind1='APCL_13369L0383', ind2='APCL_13369L1666')
totest[[23]]=data.frame(ind1='APCL_13383L0840', ind2='APCL_13383L1695')
totest[[24]]=data.frame(ind1='APCL_13385L0396', ind2='APCL_13385L1668')
totest[[25]]=data.frame(ind1='APCL_13389L0399', ind2='APCL_13389L1635')
totest[[26]]=data.frame(ind1='APCL_13393L0841', ind2='APCL_13393L1667')
totest[[27]]=data.frame(ind1='APCL_13395L0648', ind2='APCL_13395L1669')
totest[[28]]=data.frame(ind1='APCL_13398L0404', ind2='APCL_13398L1604')
totest[[29]]=data.frame(ind1='APCL_13400L0649', ind2='APCL_13400L1670')
totest[[30]]=data.frame(ind1='APCL_13402L0650', ind2='APCL_13402L1671')
totest[[31]]=data.frame(ind1='APCL_13409L1307', ind2='APCL_13409L1696')
totest[[32]]=data.frame(ind1='APCL_13411L0842', ind2='APCL_13411L1697')
totest[[33]]=data.frame(ind1='APCL_13426L0450', ind2='APCL_13426L1674')
totest[[34]]=data.frame(ind1='APCL_13429L0452', ind2='APCL_13429L1636')
totest[[35]]=data.frame(ind1='APCL_13430L0453', ind2='APCL_13430L1637')
totest[[36]]=data.frame(ind1='APCL_13432L0843', ind2='APCL_13432L1723')
totest[[37]]=data.frame(ind1='APCL_13442L0850', ind2='APCL_13442L1698')
totest[[38]]=data.frame(ind1='APCL_13450L0856', ind2='APCL_13450L1699')
totest[[39]]=data.frame(ind1='APCL_13454L0877', ind2='APCL_13454L1700')
totest[[40]]=data.frame(ind1='APCL_13461L0466', ind2='APCL_13461L1676')
totest[[41]]=data.frame(ind1='APCL_13663L1008', ind2='APCL_13663L1726')
totest[[42]]=data.frame(ind1='APCL_13664L1010', ind2='APCL_13664L1727')
totest[[43]]=data.frame(ind1='APCL_14126L1321', ind2='APCL_14126L1701')
totest[[44]]=data.frame(ind1='APCL_14303L0595', ind2='APCL_14303L1638')
totest[[45]]=data.frame(ind1='APCL_14466L0828', ind2='APCL_14466L1683')
totest[[46]]=data.frame(ind1='APCL_14469L0637', ind2='APCL_14469L1684')
totest[[47]]=data.frame(ind1='APCL_14471L0638', ind2='APCL_14471L1685')
totest[[48]]=data.frame(ind1='APCL_14477L0829', ind2='APCL_14477L1686')
totest[[49]]=data.frame(ind1='APCL_14489L0830', ind2='APCL_14489L1677')
totest[[50]]=data.frame(ind1='APCL_14494L0968', ind2='APCL_14494L1713')
totest[[51]]=data.frame(ind1='APCL_14495L1012', ind2='APCL_14495L1749')
totest[[52]]=data.frame(ind1='APCL_14499L0766', ind2='APCL_14499L1678')
totest[[53]]=data.frame(ind1='APCL_14501L0746', ind2='APCL_14501L1702')
totest[[54]]=data.frame(ind1='APCL_14505L0938', ind2='APCL_14505L1703')
totest[[55]]=data.frame(ind1='APCL_14506L1325', ind2='APCL_14506L1752')
totest[[56]]=data.frame(ind1='APCL_14508L1011', ind2='APCL_14508L1704')
totest[[57]]=data.frame(ind1='APCL_14509L1326', ind2='APCL_14509L1705')
totest[[58]]=data.frame(ind1='APCL_14511L1327', ind2='APCL_14511L1706')
totest[[59]]=data.frame(ind1='APCL_14512L1298', ind2='APCL_14512L1707')
totest[[60]]=data.frame(ind1='APCL_14517L0811', ind2='APCL_14517L1682')
totest[[61]]=data.frame(ind1='APCL_14518L1328', ind2='APCL_14518L1708')
totest[[62]]=data.frame(ind1='APCL_14520L0767', ind2='APCL_14520L1680')
totest[[63]]=data.frame(ind1='APCL_14524L0970', ind2='APCL_14524L1681')
totest[[64]]=data.frame(ind1='APCL_14525L0939', ind2='APCL_14525L1709')
totest[[65]]=data.frame(ind1='APCL_14526L0971', ind2='APCL_14526L1714')
totest[[66]]=data.frame(ind1='APCL_14528L0940', ind2='APCL_14528L1710')
totest[[67]]=data.frame(ind1='APCL_14529L1296', ind2='APCL_14529L1753')
totest[[68]]=data.frame(ind1='APCL_14530L0972', ind2='APCL_14530L1715')
totest[[69]]=data.frame(ind1='APCL_14532L0941', ind2='APCL_14532L1711')
totest[[70]]=data.frame(ind1='APCL_14533L1294', ind2='APCL_14533L1712')
totest[[71]]=data.frame(ind1='APCL_14536L1334', ind2='APCL_14536L1716')
totest[[72]]=data.frame(ind1='APCL_14557L1350', ind2='APCL_14557L1717')
totest[[73]]=data.frame(ind1='APCL_13584L0677', ind2='APCL_13584L1736')
totest[[74]]=data.frame(ind1='APCL_14155L0730', ind2='APCL_14155L1741')
totest[[75]]=data.frame(ind1='APCL_14032L0947', ind2='APCL_14032L1739')
totest[[76]]=data.frame(ind1='APCL_14442L0966', ind2='APCL_14442L1732')
totest[[77]]=data.frame(ind1='APCL_14498L0969', ind2='APCL_14498L1751')
totest[[78]]=data.frame(ind1='APCL_14006L0722', ind2='APCL_14006L1738')
totest[[79]]=data.frame(ind1='APCL_13565L0670', ind2='APCL_13565L1735')
totest[[80]]=data.frame(ind1='APCL_14049L0726', ind2='APCL_14049L1740')
totest[[81]]=data.frame(ind1='APCL_13587L0678', ind2='APCL_13587L1737')
totest[[82]]=data.frame(ind1='APCL_14255L0737', ind2='APCL_14255L1742')
totest[[83]]=data.frame(ind1='APCL_14309L1018', ind2='APCL_14309L1728')
totest[[84]]=data.frame(ind1='APCL_14317L0806', ind2='APCL_14317L1743')
totest[[85]]=data.frame(ind1='APCL_14401L1005', ind2='APCL_14401L1731')
totest[[86]]=data.frame(ind1='APCL_14425L1343', ind2='APCL_14425L1729')
totest[[87]]=data.frame(ind1='APCL_13402L0650', ind2='APCL_13402L1671')
totest[[88]]=data.frame(ind1='APCL_14452L0744', ind2='APCL_14452L1754')
totest[[89]]=data.frame(ind1='APCL_13333L0357', ind2='APCL_13333L1721')
totest[[90]]=data.frame(ind1='APCL_13334L0358', ind2='APCL_13334L1722')
totest[[91]]=data.frame(ind1='APCL_13639L0979', ind2='APCL_13639L1724')
totest[[92]]=data.frame(ind1='APCL_13651L0980', ind2='APCL_13651L1725')
totest[[93]]=data.frame(ind1='APCL_14310L0597', ind2='APCL_14310L1748')
totest[[94]]=data.frame(ind1='APCL_14399L0798', ind2='APCL_14399L1730')
totest[[95]]=data.frame(ind1='APCL_14400L0799', ind2='APCL_14400L1745')
totest[[96]]=data.frame(ind1='APCL_14424L1342', ind2='APCL_14424L1747')
totest[[97]]=data.frame(ind1='APCL_13002L0448', ind2='APCL_13002L1672')
totest[[98]]=data.frame(ind1='APCL_14497L1273', ind2='APCL_14497L2201')
totest[[99]]=data.frame(ind1='APCL_13048L0881', ind2='APCL_13048L1032')
totest[[100]]=data.frame(ind1='APCL_13207L0444', ind2='APCL_13207L1024')
totest[[101]]=data.frame(ind1='APCL_13032L1025', ind2='APCL_13032L1591')
totest[[102]]=data.frame(ind1='APCL_13033L1026', ind2='APCL_13033L1592')
totest[[103]]=data.frame(ind1='APCL_13034L1027', ind2='APCL_13034L1639')
totest[[104]]=data.frame(ind1='APCL_13035L1028', ind2='APCL_13035L1640')
totest[[105]]=data.frame(ind1='APCL_13036L1029', ind2='APCL_13036L1593')
totest[[106]]=data.frame(ind1='APCL_13038L1030', ind2='APCL_13038L1594')
totest[[107]]=data.frame(ind1='APCL_13047L1031', ind2='APCL_13047L1595')
totest[[108]]=data.frame(ind1='APCL_13050L1033', ind2='APCL_13050L1597')
totest[[109]]=data.frame(ind1='APCL_13051L1034', ind2='APCL_13051L1598')
totest[[110]]=data.frame(ind1='APCL_13052L1035', ind2='APCL_13052L1599')
totest[[111]]=data.frame(ind1='APCL_13053L1036', ind2='APCL_13053L1600')
totest[[112]]=data.frame(ind1='APCL_13054L1037', ind2='APCL_13054L1601')
totest[[113]]=data.frame(ind1='APCL_13055L1038', ind2='APCL_13055L1602')
totest[[114]]=data.frame(ind1='APCL_13056L1039', ind2='APCL_13056L1603')
totest[[115]]=data.frame(ind1='APCL_13057L1040', ind2='APCL_13057L1718')
totest[[116]]=data.frame(ind1='APCL_13058L1041', ind2='APCL_13058L1605')
totest[[117]]=data.frame(ind1='APCL_13059L1042', ind2='APCL_13059L1606')
totest[[118]]=data.frame(ind1='APCL_13061L1043', ind2='APCL_13061L1607')
totest[[119]]=data.frame(ind1='APCL_13062L1044', ind2='APCL_13062L1641')
totest[[120]]=data.frame(ind1='APCL_13063L1045', ind2='APCL_13063L1608')
totest[[121]]=data.frame(ind1='APCL_13064L1046', ind2='APCL_13064L1642')
totest[[122]]=data.frame(ind1='APCL_13065L1047', ind2='APCL_13065L1609')
totest[[123]]=data.frame(ind1='APCL_13067L1049', ind2='APCL_13067L1644')
totest[[124]]=data.frame(ind1='APCL_13068L1050', ind2='APCL_13068L1610')
totest[[125]]=data.frame(ind1='APCL_13070L1051', ind2='APCL_13070L1611')
totest[[126]]=data.frame(ind1='APCL_13071L1052', ind2='APCL_13071L1645')
totest[[127]]=data.frame(ind1='APCL_13072L1053', ind2='APCL_13072L1612')
totest[[128]]=data.frame(ind1='APCL_13075L1054', ind2='APCL_13075L1646')
totest[[129]]=data.frame(ind1='APCL_13076L1055', ind2='APCL_13076L1647')
totest[[130]]=data.frame(ind1='APCL_13077L1056', ind2='APCL_13077L1648')
totest[[131]]=data.frame(ind1='APCL_13078L1057', ind2='APCL_13078L1649')
totest[[132]]=data.frame(ind1='APCL_13079L1058', ind2='APCL_13079L1650')
totest[[133]]=data.frame(ind1='APCL_13080L1059', ind2='APCL_13080L1613')
totest[[134]]=data.frame(ind1='APCL_13082L1060', ind2='APCL_13082L1688')
totest[[135]]=data.frame(ind1='APCL_13083L1061', ind2='APCL_13083L1614')
totest[[136]]=data.frame(ind1='APCL_13084L1062', ind2='APCL_13084L1652')
totest[[137]]=data.frame(ind1='APCL_13085L1063', ind2='APCL_13085L1653')
totest[[138]]=data.frame(ind1='APCL_13086L1064', ind2='APCL_13086L1654')
totest[[139]]=data.frame(ind1='APCL_13089L0424', ind2='APCL_13089L1615')
totest[[140]]=data.frame(ind1='APCL_13091L1065', ind2='APCL_13091L1655')
totest[[141]]=data.frame(ind1='APCL_13092L1066', ind2='APCL_13092L1656')
totest[[142]]=data.frame(ind1='APCL_13095L1067', ind2='APCL_13095L1616')
totest[[143]]=data.frame(ind1='APCL_13097L1068', ind2='APCL_13097L1657')
totest[[144]]=data.frame(ind1='APCL_13098L1069', ind2='APCL_13098L1658')
totest[[145]]=data.frame(ind1='APCL_13100L1070', ind2='APCL_13100L1617')
totest[[146]]=data.frame(ind1='APCL_13123L1086', ind2='APCL_13123L1689')
totest[[147]]=data.frame(ind1='APCL_13140L1303', ind2='APCL_13140L1643')
totest[[148]]=data.frame(ind1='APCL_13176L1159', ind2='APCL_13176L1618')
totest[[149]]=data.frame(ind1='APCL_13179L1161', ind2='APCL_13179L1619')
totest[[150]]=data.frame(ind1='APCL_13181L1162', ind2='APCL_13181L1620')
totest[[151]]=data.frame(ind1='APCL_13183L1305', ind2='APCL_13183L1660')
totest[[152]]=data.frame(ind1='APCL_13185L1163', ind2='APCL_13185L1661')
totest[[153]]=data.frame(ind1='APCL_13187L1023', ind2='APCL_13187L1621')
totest[[154]]=data.frame(ind1='APCL_13188L1165', ind2='APCL_13188L1622')
totest[[155]]=data.frame(ind1='APCL_13189L1264', ind2='APCL_13189L1662')
totest[[156]]=data.frame(ind1='APCL_13196L1169', ind2='APCL_13196L1623')
totest[[157]]=data.frame(ind1='APCL_13199L1170', ind2='APCL_13199L1624')
totest[[158]]=data.frame(ind1='APCL_13209L1048', ind2='APCL_13209L1626')
totest[[159]]=data.frame(ind1='APCL_13210L1176', ind2='APCL_13210L1627')
totest[[160]]=data.frame(ind1='APCL_14497L1750', ind2='APCL_14497L2201')
totest[[161]]=data.frame(ind1='APCL_14502L1287', ind2='APCL_14502L1679')
totest[[162]]=data.frame(ind1='APCL_13130L0432', ind2='APCL_13130L1720')
totest[[163]]=data.frame(ind1='APCL_13087L0423', ind2='APCL_13087L1719')
totest[[164]]=data.frame(ind1='APCL_14318L1300', ind2='APCL_14318L1744')
totest[[165]]=data.frame(ind1='APCL_14423L1341', ind2='APCL_14423L1746')
totest[[166]]=data.frame(ind1='APCL_13048L0881', ind2='APCL_13048L1596')
totest[[167]]=data.frame(ind1='APCL_13207L0444', ind2='APCL_13207L1625')
totest[[168]]=data.frame(ind1='APCL_13048L1032', ind2='APCL_13048L1596')
totest[[169]]=data.frame(ind1='APCL_13207L1024', ind2='APCL_13207L1625')

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
write.csv(out, file=(paste("regenotyped", Sys.Date(), ".csv", sep="")))



onematch = (alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1) # does one allele match but not the other?
homvhet = ((alone1 == altwo1 & alone2 != altwo2) | (alone1 == altwo2 & alone2 != altwo1) | (alone2 == altwo1 & alone1 != altwo2) | (alone2 == altwo2 & alone1 != altwo1)) & (alone1 == alone2 | altwo1 == altwo2) # a onematch where one of the genotypes is a homozygote (hom vs. het mismatch)
sum(onematch)
sum(homvhet) # the same, if all one allele matches are hom vs het mismatches
sum(!onematch)

rbind(genosone[which(!matches)], genostwo[which(!matches)]) # visually inspect
rbind(genosone[which(!matches)][onematch], genostwo[which(!matches)][onematch]) # visually inspect cases where they match on one allele
rbind(genosone[which(!matches)][!onematch], genostwo[which(!matches)][!onematch]) # visually inspect where no alleles match




