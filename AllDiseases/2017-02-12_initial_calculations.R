

#2017-02-12 new section to attempt to query GWAS central
rm(list=ls())
library(biomaRt)
listMarts(host="mart.gwascentral.org:8000")


listMarts(host="http://mart.gwascentral.org/biomart/martview/")
#hmm doesn't seem to work






#try bulk download
rm(list=ls())
data<-read.table("AllDiseases/gwas_catalog_v1.0-associations_e87_r2017-02-06.tsv.gz",header=T,sep="\t",comment.char="",quote="",stringsAsFactors=F)



#check that the strongest SNP entry is consistent with the SNPs entry (remove otherwise)
data[data[,"STRONGEST.SNP.RISK.ALLELE"]%in%"rs12449664A","STRONGEST.SNP.RISK.ALLELE"]<-"rs12449664-A" #clear typo
s1<-gsub(" ","",sub("\\?","",sub("NR$","",sub("-.+$","",data[,"STRONGEST.SNP.RISK.ALLELE"]))))
data[s1 != data[,"SNPS"],c("STRONGEST.SNP.RISK.ALLELE","SNPS")] #6 entries
data<-data[s1 == data[,"SNPS"],]


#remove SNPs that don't have OR/beta or risk-allele indication
sum(is.na(data[,"OR.or.BETA"]))
#1662
data<-data[!is.na(data[,"OR.or.BETA"]),]

#remove sets that are too small
d<-gsub(",","",gsub("[A-Za-z]","",data[,"INITIAL.SAMPLE.SIZE"]))
data[,"sampleSize"]<-sapply(strsplit(d," +"),function(x){sum(as.numeric(x),na.rm=T)})
data<-data[order(data[,"sampleSize"]),]

hist(data[,"sampleSize"],breaks=1000,xlim=c(0,10000),xlab="sample size",main="Sample sizes")
#decision - remove all studies with sample size < 2000 (because, they may or may not be good)
data<-data[data[,"sampleSize"]> 2000,]

#remove sets that have too few SNPs per study
studies<-data.frame(row.names=unique(data[,"PUBMEDID"]))
for(study in rownames(studies)){
	d1<-data[data[,"PUBMEDID"]%in%study,]
	studies[study,"SNPs"]<-nrow(d1)
}
hist(studies[,"SNPs"],breaks=10000,xlim=c(0,100))
#decision - remove all studies with SNP-count < 10 (because the GRS may be odd)
data<-data[!data[,"PUBMEDID"]%in%rownames(studies)[studies[,"SNPs"]<10],]


#investigating number of studies per trait
traits<-data.frame(row.names=unique(data[,"DISEASE.TRAIT"]))
for(trait in rownames(traits)){
	d2<-data[data[,"DISEASE.TRAIT"]%in%trait,]
	traits[trait,"SNPs"]<-nrow(d2)
	traits[trait,"studies"]<-length(unique(d2[,"PUBMEDID"]))
}
traits[order(traits[,"studies"]),]
#conclusion - some have more than 10 studies, probably most transparent to let users select the specific study


#remove some traits because they are better handled in other modules and/or too weird/difficult to explain easily and/or conflict with module title (or perhaps just ideas for new modules?)
omit<-unique(c(
	grep("height",rownames(traits),ignore.case=T,value=T),
	grep("hair",rownames(traits),ignore.case=T,value=T),
	grep("economic",rownames(traits),ignore.case=T,value=T),
	grep("political",rownames(traits),ignore.case=T,value=T)
))
data<-data[!data[,"DISEASE.TRAIT"]%in%omit,]


#remove some columns that are not needed
col_to_remove<-c("MAPPED_GENE","UPSTREAM_GENE_ID","DOWNSTREAM_GENE_ID","SNP_GENE_IDS","UPSTREAM_GENE_DISTANCE","DOWNSTREAM_GENE_DISTANCE")
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
#760
data<-data[!is.na(data[,"minor_allele_freq"]),]


#check the two chr-names are the same
colnames(data)
sum(data[,"CHR_ID" ] != data[,"chr_name"])
#0
#good! can remove the original one then
data[,"CHR_ID"] <- NULL


#perhaps remove the tri-allelics
sum(sapply(strsplit(data[,"ensembl_alleles"],"/"),length) != 2)
# 581 of 18456 -- should be ok to remove - probably the safer option
data<-data[sapply(strsplit(data[,"ensembl_alleles"],"/"),length) == 2,]

#assign major allele from the ensembl alleles
a1<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[1]})
a2<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[2]})
data[,"major_allele"] <- NA
data[data[,"minor_allele"]==a1,"major_allele"]<-a2[data[,"minor_allele"]==a1]
data[data[,"minor_allele"]==a2,"major_allele"]<-a1[data[,"minor_allele"]==a2]

#check some cases were minor allele is not even found in the ensembl alleles (these should probably be removed)
data<-data[!is.na(data[,"major_allele"]),]

#double check a few with online browsers
set.seed(42)
data[sample(1:nrow(data),5),c("SNPS","minor_allele_freq","major_allele","minor_allele")]
#seems ok



#now let's try to assign effect allele (scared-smiley goes here)
sum(data[,"OR.or.BETA"]<0,na.rm=T)
#none are below 0, so at least the betas are not inverted or something... could be problematic with <1 ORs, but -- we'll see
data[,"risk_allele"]<-sub("^.+-","",data[,"STRONGEST.SNP.RISK.ALLELE"])
sum(is.na(data[,"risk_allele"]))


#check places were the risk-allele is not found in ensembl minor or major allele
data[is.na(data[,"risk_allele"] != data[,"minor_allele"] & data[,"risk_allele"] != data[,"major_allele"]),]
#ok - none found that's quite nice


# data[,"risk_allele"] == data[,"minor_allele"]
data[data[,"minor_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"minor_allele"]==data[,"risk_allele"],"major_allele"]
data[data[,"major_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"major_allele"]==data[,"risk_allele"],"minor_allele"]

#check often minor allele is risk
sum(data[,"minor_allele"]==data[,"risk_allele"]) / nrow(data)
#0.52 -- I would have expected less, but that's probably just for old rare-variant studies that we'd think so.


c(data)
