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
write.table(giant_sup,file="GIANT_modified_table.txt",col.names=NA,sep="\t")






#start a test script
rm(list=ls())
giant_sup_path<-"/home/ubuntu/misc_files/GIANT_modified_table.txt"

# giant_sup_path<-"C:/Users/FOLK/Documents/Work/Analysis/2014-08-11 pedigree project/GIANT_height_250k_Supplementary_Tables_20131030.txt"


giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
giant_sup<-giant_sup[order(abs(giant_sup[,"Beta"]),decreasing=T),]
giant_sup[,"chr_name"]<-giant_sup[,"Chr"]

source("/srv/shiny-server/gene-surfer/guessMyHeight/functions.R")
genotypes<-get_genotypes(uniqueID="id_424142906",request=giant_sup)


for(snp in rownames(giant_sup)){
	# giant_sup[
	
	genotype<-strsplit(genotypes[snp,],"/")[[1]]
	effect_allele<-giant_sup[snp,"Effect..Allele"]
	beta<-giant_sup[snp,"Beta"]	
	
	if(length(genotype) == 2 && !is.na(genotype)){
		if(!effect_allele%in%genotype)stop("Effect allele not found")	
		
	}
	
	
	
}
