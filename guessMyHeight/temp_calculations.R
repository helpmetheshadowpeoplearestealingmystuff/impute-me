# uniqueID<-"id_424142906"


#Get all the alleles
giant_sup_path<-"/home/ubuntu/misc_files/GIANT_height_250k_Supplementary_Tables_20131030.txt"

giant_sup_path<-"C:/Users/FOLK/Documents/Work/Analysis/2014-08-11 pedigree project/GIANT_height_250k_Supplementary_Tables_20131030.txt"


giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
# giant_sup<-giant_sup[order(abs(giant_sup[,"Beta"]),decreasing=T),]
# giant_sup[,"chr_name"]<-giant_sup[,"Chr"]

library(biomaRt)
snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
attributes<-c("refsnp_id","chr_name","chrom_start","allele")
listFilters(snp_mart)
query<-getBM(attributes, filters = c("snp_filter"), values = rownames(giant_sup), mart = snp_mart)
query<-query[nchar(query[,"chr_name"])%in%1:2,]
rownames(query)<-query[,"refsnp_id"]
giant_sup[,"ensembl_alleles"]<-query[rownames(giant_sup),"allele"]


#get the non-effect allele
for(snp in rownames(giant_sup)){
	effect_allele<-giant_sup[snp,"Effect..Allele"]
	alleles<-strsplit(giant_sup[snp,"ensembl_alleles"],"/")[[1]]
	if(length(alleles)==2){
		giant_sup[snp,"non_effect_allele"]<-alleles[!alleles%in%effect_allele	]
	}else{
		if(snp == "rs870183"){
			giant_sup[snp,"non_effect_allele"]<-alleles[!alleles%in%effect_allele	][2]
		}else{
		giant_sup[snp,"non_effect_allele"]<-alleles[!alleles%in%effect_allele	][1]
		}
		print(paste("Omitted some allele information for",snp,"- effect allele:",giant_sup[snp,"Effect..Allele"],"non-effect allele:",giant_sup[snp,"non_effect_allele"],"but all alleles were",giant_sup[snp,"ensembl_alleles"]))
		
	}
}
	
write.table(giant_sup,file="GIANT_modified_table.txt",col.names=NA,sep="\t")









#start a test script
rm(list=ls())
giant_sup_path<-"/home/ubuntu/misc_files/GIANT_modified_table.txt"

# giant_sup_path<-"C:/Users/FOLK/Documents/Work/Analysis/2014-08-11 pedigree project/GIANT_height_250k_Supplementary_Tables_20131030.txt"


giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
giant_sup<-giant_sup[order(abs(giant_sup[,"Beta"]),decreasing=T),]
giant_sup[,"chr_name"]<-giant_sup[,"Chr"]

source("/srv/shiny-server/gene-surfer/functions.R")
genotypes<-get_genotypes(uniqueID="id_424142906",request=giant_sup)


gheight_score<-0
for(snp in rownames(giant_sup)){
	if(is.na(genotypes[snp,]))next
	genotype<-strsplit(genotypes[snp,],"/")[[1]]
	effect_allele<-giant_sup[snp,"Effect..Allele"]
	non_effect_allele<-giant_sup[snp,"non_effect_allele"]
	all_alleles<-c(non_effect_allele,effect_allele)
	beta<-giant_sup[snp,"Beta"]	
	gheight_score <- gheight_score + sum(genotype%in%effect_allele) * beta
}











#generating some initial gheight stuff
heights_registered<-"/home/ubuntu/misc_files/height_registrered.txt"

for(i in 1:100){
	real_height<-round(rnorm(1,mean=174,sd=10))
	real_age<-sample(30:60,1)
	gheight<-signif((real_height - 174)/10,4)+rnorm(1,mean=0,sd=0.2)
	uniqueID <- paste("id_",sample(1000:9000,1),sample(10000:90000,1),sep="")
	
	real_entry<-FALSE
	entry <- c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),uniqueID, gheight, real_height, real_age, real_entry)
	write(paste(entry,collapse="\t"),file=heights_registered,append=TRUE)
	
	
	
}