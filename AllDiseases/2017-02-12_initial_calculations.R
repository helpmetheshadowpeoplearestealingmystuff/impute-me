

#2017-02-12 new section to attempt to query GWAS central
rm(list=ls())
library(biomaRt)
listMarts(host="mart.gwascentral.org:8000")


listMarts(host="http://mart.gwascentral.org/biomart/martview/")
#hmm doesn't seem to work






#try bulk download
rm(list=ls())
data<-read.table("AllDiseases/gwas_catalog_v1.0-associations_e87_r2017-02-06.tsv.gz",header=T,sep="\t",comment.char="",quote="",stringsAsFactors=F)




#remove sets that are too small
d<-gsub(",","",gsub("[A-Za-z]","",data[,"INITIAL.SAMPLE.SIZE"]))
data[,"sampleSize"]<-sapply(strsplit(d," +"),function(x){sum(as.numeric(x),na.rm=T)})
data<-data[order(data[,"sampleSize"]),]
hist(data[,"sampleSize"],breaks=1000,xlim=c(0,10000),xlab="sample size",main="Sample sizes")
#decision - remove all studies with sample size < 2000 (because, they may or may not be good)
data<-data[data[,"sampleSize"]> 2000,]



#check that the strongest SNP entry is consistent with the SNPs entry (remove otherwise)
data[data[,"STRONGEST.SNP.RISK.ALLELE"]%in%"rs12449664A","STRONGEST.SNP.RISK.ALLELE"]<-"rs12449664-A" #clear typo
s1<-gsub(" ","",sub("\\?","",sub("NR$","",sub("-.+$","",data[,"STRONGEST.SNP.RISK.ALLELE"]))))
sum(s1 != data[,"SNPS"]) #154
s2<-data[s1 != data[,"SNPS"],c("STRONGEST.SNP.RISK.ALLELE","SNPS")] 
#ok could probably save some of these, but on later testing it is found that all but 6 are removed by other filters anyway... so just get rid of them
data<-data[s1 == data[,"SNPS"],]


#remove SNPs that don't have OR/beta or risk-allele indication
data[,"risk_allele"]<-sub("^.+-","",data[,"STRONGEST.SNP.RISK.ALLELE"])
sum(data[,"risk_allele"]=="?") #3242 - definetly must remove these
data<-data[data[,"risk_allele"]!="?",]





#investigating number of studies per trait
traits<-data.frame(row.names=unique(data[,"DISEASE.TRAIT"]))
for(trait in rownames(traits)){
	d2<-data[data[,"DISEASE.TRAIT"]%in%trait,]
	traits[trait,"SNPs"]<-nrow(d2)
	traits[trait,"studies"]<-length(unique(d2[,"PUBMEDID"]))
}
# traits[order(traits[,"studies"]),]
#conclusion - some have more than 10 studies, probably most transparent to let users select the specific study


#remove some traits because they are better handled in other modules and/or too weird/difficult to explain easily and/or conflict with module title (or perhaps just ideas for new modules?)
omit<-unique(c(
	grep("height",rownames(traits),ignore.case=T,value=T),
	grep("hair",rownames(traits),ignore.case=T,value=T),
	grep("economic",rownames(traits),ignore.case=T,value=T),
	grep("political",rownames(traits),ignore.case=T,value=T),
	grep("word reading",rownames(traits),ignore.case=T,value=T),
	grep("eyes",rownames(traits),ignore.case=T,value=T)
	# social_communication_problems
	# wine_liking
))
data<-data[!data[,"DISEASE.TRAIT"]%in%omit,]


#remove some columns that are not needed
col_to_remove<-c("MAPPED_GENE","UPSTREAM_GENE_ID","DOWNSTREAM_GENE_ID","SNP_GENE_IDS","UPSTREAM_GENE_DISTANCE","DOWNSTREAM_GENE_DISTANCE","PLATFORM..SNPS.PASSING.QC.","CNV","P.VALUE..TEXT.","PVALUE_MLOG","RISK.ALLELE.FREQUENCY","CONTEXT","INTERGENIC","SNP_ID_CURRENT","MERGED","STUDY","JOURNAL","DATE.ADDED.TO.CATALOG","INITIAL.SAMPLE.SIZE","REPLICATION.SAMPLE.SIZE","CHR_POS")
for(col in col_to_remove){data[,col]<-NULL}



#retrieve chr-ID (for double-check), minor allele frequency and assign effect and non-effect allele
library(biomaRt)
snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
attributes<-c("refsnp_id","chr_name","chrom_start","allele","minor_allele_freq","minor_allele")
query<-getBM(attributes, filters = c("snp_filter"), values = data[,"SNPS"], mart = snp_mart)
query<-query[nchar(query[,"chr_name"])%in%1:2,]
rownames(query)<-query[,"refsnp_id"]

data[,"ensembl_alleles"]<-query[data[,"SNPS"],"allele"]
data[,"chr_name"]<-query[data[,"SNPS"],"chr_name"]
data[,"minor_allele_freq"]<-query[data[,"SNPS"],"minor_allele_freq"]
data[,"minor_allele"]<-query[data[,"SNPS"],"minor_allele"]

#remove the ones with no known MAF
sum(is.na(data[,"minor_allele_freq"]))
#610
data<-data[!is.na(data[,"minor_allele_freq"]),]

#check the two chr-names are the same
sum(data[,"CHR_ID" ] != data[,"chr_name"],na.rm=F)
#0
data[is.na(data[,"chr_name" ]),]
#good! can remove the original one then
data[,"CHR_ID"] <- NULL


#perhaps remove the tri-allelics
sum(sapply(strsplit(data[,"ensembl_alleles"],"/"),length) != 2)
#  613 of 16744 -- should be ok to remove - probably the safer option
data<-data[sapply(strsplit(data[,"ensembl_alleles"],"/"),length) == 2,]




#assign major allele from the ensembl alleles
a1<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[1]})
a2<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[2]})
data[,"major_allele"] <- NA
data[data[,"minor_allele"]==a1,"major_allele"]<-a2[data[,"minor_allele"]==a1]
data[data[,"minor_allele"]==a2,"major_allele"]<-a1[data[,"minor_allele"]==a2]


#check some cases were minor allele is not even found in the ensembl alleles. These should probably be removed; they are cases were ensembl_alleles is on the opposite strand. Could be flipped, but better to be safe.
data[is.na(data[,"major_allele"]),"minor_allele"]<-"?"
data[is.na(data[,"major_allele"]),"major_allele"]<-"?"



#check cases were risk allele is not found in minor or major allele
sum(!(data[,"risk_allele"] %in% data[,"minor_allele"] | data[,"risk_allele"] %in% data[,"major_allele"] ))
#23
#probably also best to remove these
data[!(data[,"risk_allele"] %in% data[,"minor_allele"] | data[,"risk_allele"] %in% data[,"major_allele"] ),"risk_allele"]<-"?"

#double check a few with online browsers
set.seed(42)
data[sample(1:nrow(data),3),c("SNPS","minor_allele_freq","major_allele","minor_allele")]
#seems ok


#remove sets that have too few SNPs per study
studies<-data.frame(row.names=unique(data[,"PUBMEDID"]))
for(study in rownames(studies)){
	d1<-data[data[,"PUBMEDID"]%in%study,]
	studies[study,"SNPs"]<-nrow(d1)
}
hist(studies[,"SNPs"],breaks=10000,xlim=c(0,100))
# decision - remove all studies with SNP-count < 5 (because the GRS may be odd)
data<-data[!data[,"PUBMEDID"]%in%rownames(studies)[studies[,"SNPs"]<5],]



#now let's try to assign effect allele (scared-smiley goes here)
sum(data[,"OR.or.BETA"]<0,na.rm=T)
#none are below 0, so at least the betas are not inverted or something... could be problematic with <1 ORs, but -- we'll see


#check places were the risk-allele is not found in ensembl minor or major allele
data[is.na(data[,"risk_allele"] != data[,"minor_allele"] & data[,"risk_allele"] != data[,"major_allele"]),]
#ok - none found that's quite nice

#insert the non-risk allele as being the allele that is not risk, and is the other allele (taking from major/minor info)
data[data[,"minor_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"minor_allele"]==data[,"risk_allele"],"major_allele"]
data[data[,"major_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"major_allele"]==data[,"risk_allele"],"minor_allele"]

#check how often minor allele is risk
sum(data[,"minor_allele"]==data[,"risk_allele"]) / nrow(data)
#0.49 


#check some 7 random ones with their literature entries
set.seed(42)
data[sample(1:nrow(data),7),c("SNPS","minor_allele_freq","major_allele","minor_allele","risk_allele","non_risk_allele","PUBMEDID","DISEASE.TRAIT")]
# SNPS minor_allele_freq major_allele minor_allele risk_allele non_risk_allele PUBMEDID
# 21156  rs13017997       0.291733000            T            A           T               A 26198764
# 21526   rs7610761       0.371406000            T            A           A               T 26198764
# 26326  rs11217863       0.100639000            G            A           A               G 25637523
# 14040   rs2606736       0.481230000            T            C           C               T 24097068
# 13043    rs865686       0.291933000            T            G           T               G 23535729
# 32885 rs188657011       0.000199681            C            A           A               C 26634245
# 27881   rs1859962       0.424920000            T            G           G               T 26034056
#1) rs2045517 is not present in original paper - but likely rs66831316, at almost same location and at least same P-value. Unable to estimate direction. Conclusion: unsure.
#2) rs7610761 is not present in original paper - but likely rs4685495, at almost same location and at least same P-value. Unable to estimate direction. Conclusion: unsure.
#3) rs11217863 checks out, both minor/major and risk. Conclusion: correct.
#4) rs2606736 checks out, both minor/major and risk. Conclusion: correct.
#5) rs865686 is present in original paper, well hidden in supplementary. minor/major is correct. Odds ratio 0.89 for minor allele. So it's correct that T is risk. Conclusion: correct.
#6) rs188657011 well hidden in supplementary, and also have a mistake indication that Coded_Allele_/Beta/P is reported for the number block e.g. A/0.999/0.043/0.4566 or A/0.999/0.367/1.56e-06, but surely that must be allele/frequency/beta/P -- # rs188657011	6	SUPT3H A/0.999/0.043/0.4566	A/0.998/-0.001/0.9865	A/0.999/0.367/1.56e-06	A/0.998/0.189/0.009	NA	A/0.002/0.001/0.986. Anyway - looks correct. Conclusion: correct.
#7) rs1859962 from supplementary: 17q24 69108753 G T 1.19 (1.14, 1.24) 0.48 1.13 (1.09, 1.18) 7.7e-09 1.00 0.55 1.16 (0.99, 1.36) 0.06 1.00 0.4 1.03 (0.85, 1.24) 0.79 1.00 0.32 1.08 (0.93, 1.24) 0.31 0.97 1.13 (1.08, 1.17) 1.6e-09 1.13 (1.08, 1.17) 1.6e-09. Conclusion: correct.
#overall - quite encouraging!



#add a 'safe_name' trait/PMID no-special characters identifier to each
data[,"study_id"]<-tolower(gsub(" ","_",gsub("[?&/\\-\\.\\(\\)\\']", "", paste(data[,"DISEASE.TRAIT"],data[,"PUBMEDID"],sep="_"))))



#ensure only standard values A G C T ? are present
table(data[,"major_allele"])
table(data[,"minor_allele"])
table(data[,"risk_allele"])
table(data[,"non_risk_allele"])
ok_values <- c("A","C","T","G","?")
for(col in c("major_allele","minor_allele","risk_allele","non_risk_allele")){
  data[!data[,col]%in%ok_values,col]<-"?"
}

#ensure match between risk/non-risk and major/minor
g1<-apply(t(apply(data[,c("major_allele","minor_allele")],1,sort,decreasing=F)),1,paste,collapse="")
g2<-apply(t(apply(data[,c("risk_allele","non_risk_allele")],1,sort,decreasing=F)),1,paste,collapse="")
have_unknown <- apply(data[,c("major_allele","minor_allele","risk_allele","non_risk_allele")]=="?",1,sum)>0
# have_unknown
sum(g1!=g2 & !have_unknown)
# 0 #good!


#flip effect size whenever the word 'decrease' is used (note, this seems a bit inconsistenly done in GWAS central, but at least on sample checkups it's rare)
data[grep('decrease',data[,"X95..CI..TEXT."]),"OR.or.BETA"] <-  -data[grep('decrease',data[,"X95..CI..TEXT."]),"OR.or.BETA"]


#re-order colnames so that the essential are first
putFirst<-c("SNPS", "chr_name","risk_allele","non_risk_allele","OR.or.BETA",  "minor_allele_freq","minor_allele","major_allele")
data<-data[,c(putFirst,colnames(data)[!colnames(data)%in%putFirst])]
colnames(data)[1]<-"SNP"
colnames(data)[3]<-"effect_allele"
colnames(data)[4]<-"non_effect_allele"
colnames(data)[5]<-"Beta"
save(data, file="AllDiseases/2017-02-21_semi_curated_version_gwas_central.rdata")




#then save a SNPs_to_analyze.txt (which just contains snp, chr, effect_allele and non_effect_allele and NO duplicate SNPs)
gwas_snps <- data[,c("SNP","chr_name","effect_allele","non_effect_allele")]
gwas_snps <- gwas_snps[!duplicated(gwas_snps[,"SNP"]),]
rownames(gwas_snps) <- gwas_snps[,"SNP"]
save(gwas_snps,file="AllDiseases/2017-02-21_all_gwas_snps.rdata")








#then create an overview trait list
traits<-data.frame(row.names=unique(paste(data[,"DISEASE.TRAIT"], data[,"PUBMEDID"],data[, "FIRST.AUTHOR"],sep=" // ")), study_id=unique(data[,"study_id"]),stringsAsFactors=F)
for(trait in rownames(traits)){
	traits[trait,"trait"] <- strsplit(trait," // ")[[1]][1]
	traits[trait,"PMID"] <- strsplit(trait," // ")[[1]][2]
	traits[trait,"Author"] <- strsplit(trait," // ")[[1]][3]
}
traits<-traits[order(rownames(traits)),]
for(trait in rownames(traits)){
	traitName<-traits[trait,"trait"]	
	PMID<-traits[trait,"PMID"]	
	Author<-traits[trait,"Author"]	
	if(sum(traits[,"trait"]%in%traitName)>1){
		traits[trait ,"niceName"] <- paste0(traitName," [PMID ",PMID,"]")
	}else{
		traits[trait ,"niceName"] <- traitName
	}
}
rownames(traits)<-traits[,"study_id"]
# head(traits)

save(traits, file="AllDiseases/2017-02-21_trait_overoverview.rdata")


