

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
head(data)
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
head(data)



#retrieve chr-ID (for double-check), minor allele frequency and assign effect and non-effect allele
library(biomaRt)


marts<-listMarts()
name<-marts[grep("HapMap.+$",marts[,"biomart"]),"biomart"]
hapmap_version<-marts[grep("HapMap.+$",marts[,"biomart"],ignore.case=T),"version"]
hapmap_mart <- useMart("HapMap_rel27")
datasets<-listDatasets(hapmap_mart)
dataset_name<-as.character(datasets[grep(paste(population,"$",sep=""),datasets[,"dataset"]),"dataset"])
hapmap_mart<-useDataset(hapmap_mart,dataset=dataset_name)
attributes<-c("ref_allele_freq","marker1")
query<-getBM("marker1", filters = c("marker_name"), values = snps, mart = hapmap_mart, verbose = FALSE)


snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
attributes<-c("refsnp_id","chr_name","chrom_start","allele","minor_allele_freq")
query<-getBM(attributes, filters = c("snp_filter"), values = data[,"SNPS"], mart = snp_mart)
query<-query[nchar(query[,"chr_name"])%in%1:2,]
rownames(query)<-query[,"refsnp_id"]

data[,"ensembl_alleles"]<-query[data[,"SNPS"],"allele"]
data[,"chr_name"]<-query[data[,"SNPS"],"chr_name"]
data[,"minor_allele_freq"]<-query[data[,"SNPS"],"minor_allele_freq"]

#remove the ones with no known MAF
sum(is.na(data[,"minor_allele_freq"]))
#760
data<-data[!is.na(data[,"minor_allele_freq"]),]


#check the two chr-names are the same
colnames(data)
sum(data[,"CHR_ID" ] != data[,"chr_name"])
#0
#good! can remove the original one
data[,"CHR_ID"] <- NULL





