
#2017-02-23 start from allDiseases bulk download
rm(list=ls())
data<-read.table("AllDiseases/gwas_catalog_v1.0-associations_e87_r2017-02-06.tsv.gz",header=T,sep="\t",comment.char="",quote="",stringsAsFactors=F)


# data<-data[grep("political",data[,"DISEASE.TRAIT"],ignore.case=T),]
data<-data[data[,"PUBMEDID"] %in%"24569950",]

head(data)

#remove SNPs that don't have OR/beta or risk-allele indication
data[,"risk_allele"]<-sub("^.+-","",data[,"STRONGEST.SNP.RISK.ALLELE"])
sum(data[,"risk_allele"]=="?") #3242 - definetly must remove these
data<-data[data[,"risk_allele"]!="?",]

#ok -clearly not ok to take from GWAS central. Instead use the supplementary information.



#
rm(list=ls())
data<-read.table("opinions/2017-02-23_supplementary.txt",header=T,sep="\t",comment.char="",quote="",stringsAsFactors=F)

rownames(data)<-data[,"SNP"]

#retrieve chr-ID (for double-check), minor allele frequency and assign effect and non-effect allele
library(biomaRt)
snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
attributes<-c("refsnp_id","chr_name","chrom_start","allele","minor_allele_freq","minor_allele")
query<-getBM(attributes, filters = c("snp_filter"), values = data[,"SNP"], mart = snp_mart)
query<-query[nchar(query[,"chr_name"])%in%1:2,]
rownames(query)<-query[,"refsnp_id"]

data[,"ensembl_alleles"]<-query[data[,"SNP"],"allele"]
data[,"chr_name"]<-query[data[,"SNP"],"chr_name"]
data[,"minor_allele_freq"]<-query[data[,"SNP"],"minor_allele_freq"]
data[,"minor_allele"]<-query[data[,"SNP"],"minor_allele"]

#manually correct a triallelic
data["rs10036225","ensembl_alleles"] <- "C/G"

head(data)
#check the two chr-names are the same
sum(data[,"CHR" ] != data[,"chr_name"],na.rm=F)
#0
data[,"CHR"] <- NULL


#assign major allele from the ensembl alleles
a1<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[1]})
a2<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[2]})
data[,"major_allele"] <- NA
data[data[,"minor_allele"]==a1,"major_allele"]<-a2[data[,"minor_allele"]==a1]
data[data[,"minor_allele"]==a2,"major_allele"]<-a1[data[,"minor_allele"]==a2]


#check some cases were minor allele is not even found in the ensembl alleles. These should probably be removed; they are cases were ensembl_alleles is on the opposite strand. Could be flipped, but better to be safe.
data[is.na(data[,"major_allele"]),"minor_allele"]<-"?"
data[is.na(data[,"major_allele"]),"major_allele"]<-"?"


#flip alleles so AL1 is always 'risk' (haha, I'm not even sure if 'risk' is right-wing or left-wing yet)
data[data[,"EFFECT"] > 0,"risk_allele"] <- toupper(data[data[,"EFFECT"] > 0,"AL1"])
data[data[,"EFFECT"] < 0,"risk_allele"] <- toupper(data[data[,"EFFECT"] < 0,"AL2"])
data[,"EFFECT"]<-abs(data[,"EFFECT"])

#check cases were risk allele is not found in minor or major allele
sum(!(data[,"risk_allele"] %in% data[,"minor_allele"] | data[,"risk_allele"] %in% data[,"major_allele"] ))
#0




#check places were the risk-allele is not found in ensembl minor or major allele
data[is.na(data[,"risk_allele"] != data[,"minor_allele"] & data[,"risk_allele"] != data[,"major_allele"]),]
#ok - none found that's quite nice

#insert the non-risk allele as being the allele that is not risk, and is the other allele (taking from major/minor info)
data[data[,"minor_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"minor_allele"]==data[,"risk_allele"],"major_allele"]
data[data[,"major_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"major_allele"]==data[,"risk_allele"],"minor_allele"]




#add a 'safe_name' trait/PMID no-special characters identifier to each
data[,"study_id"]<-"political_opinion_24569950"



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
have_unknown
sum(g1!=g2 & !have_unknown)
# 0 #good!

#re-order colnames so that the essential are first
colnames(data)
putFirst<-c("SNP", "chr_name","risk_allele","non_risk_allele","EFFECT",  "minor_allele_freq","minor_allele","major_allele")
data<-data[,c(putFirst,colnames(data)[!colnames(data)%in%putFirst])]
colnames(data)[1]<-"SNP"
colnames(data)[3]<-"effect_allele"
colnames(data)[4]<-"non_effect_allele"
colnames(data)[5]<-"Beta"


write.table(data,file="opinions/SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F)


