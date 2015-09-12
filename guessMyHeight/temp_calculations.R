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
	real_gender<-sample(1:2,1)
	real_height<-round(rnorm(1,mean=167,sd=5))
	gender_component<-round((2-real_gender)*7*rnorm(1,1,0.05))
	real_age<-sample(30:60,1)
	gheight<-signif((real_height - (167+3.5))/5+rnorm(1,mean=0,sd=0.2),4)
	real_height<-real_height+ gender_component
	uniqueID <- paste("id_",sample(1000:9000,1),sample(10000:90000,1),sep="")
	
	real_entry<-FALSE
	entry <- c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),uniqueID, gheight, real_height, real_age, real_gender,real_entry)
	write(paste(entry,collapse="\t"),file=heights_registered,append=TRUE)
	
}
















#2015-09-12 getting serious about the background distribution (see scientific_notes.txt)
heights_pre_registered_file<-"/home/ubuntu/misc_files/background_heights.txt"
heights_pre_registered_file<-"background_heights.txt"



setwd("C:/Users/Lasse/Documents/Work/Bioinformatics/gene-surfer/guessMyHeight")

#First get the n, the weighted mean and SD - by gender
library("openxlsx")
studyDescriptions<-read.xlsx("supplementary_table_22.xlsx")
head(studyDescriptions)
heights<-studyDescriptions[studyDescriptions[,"Trait"]%in%"Height (m)",]


for(gender in c("women","men")){
	n<-as.numeric(heights[,paste(gender,"n",sep=".")])
	means<-as.numeric(heights[,paste(gender,"mean",sep=".")])
	SDs<-as.numeric(heights[,paste(gender,"SD",sep=".")])
	
	n<-n[!is.na(means)]
	SDs<-SDs[!is.na(means)]
	means<-means[!is.na(means)]
	
	#so unprofessional of the authors of a nature paper to report both as m and cm in the same table (oh wait, I'm a co-author - well damn me)
	SDs[means>100] <- SDs[means>100]/100
	means[means>100] <- means[means>100]/100
	
	mean<-signif(sum(means*n)/sum(n),3)
	SD<-signif(sum(SDs*n)/sum(n),3)
	print(paste("For",gender,"the mean is",mean,"and the SD is",SD))
}
# [1] "For women the mean is 1.62 and the SD is 0.0796"
# [1] "For men the mean is 1.75 and the SD is 0.0802"


women_mean<-1.62
women_sd<-0.0796
men_mean<-1.75
men_sd<-0.0802




 	
set.seed(42)
n<-5000
men<-data.frame(
	time=format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),
	uniqueID=paste("id_",sample(1000:9000,n),sample(10000:90000,n),sep=""),
	gheight=rnorm(n,mean=0,sd=1),
	real_height=	rnorm(n,mean=men_mean,sd=men_sd)*100,
	real_age = NA,
	real_gender = 1,
	real_entry = F
)


# men[,"gheight"]<-(men[,"real_height"]-mean(men[,"real_height"]))/sd(men[,"real_height"])

women<-data.frame(
	time=format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),
	uniqueID=paste("id_",sample(1000:9000,n),sample(10000:90000,n),sep=""),
	gheight=rnorm(n,mean=0,sd=1),
	real_height=	rnorm(n,mean=women_mean,sd=women_sd)*100,
	real_age = NA,
	real_gender = 2,
	real_entry = F
)
# women[,"gheight"]<-(women[,"real_height"]-women(men[,"real_height"]))/sd(women[,"real_height"])

#they report that the SNPs can explain 20%, so we remove 20% of the random gheight and insert 20% of a number exactly proportionate to the real_heigt
fractionExplained<-0.4 #ok we can change to 20% later
women[,"gheight"]<-women[,"gheight"]*(1-fractionExplained) + (women[,"real_height"]-mean(women[,"real_height"]))/sd(women[,"real_height"])*fractionExplained
men[,"gheight"]<-men[,"gheight"]*(1-fractionExplained) + (men[,"real_height"]-mean(men[,"real_height"]))/sd(men[,"real_height"])*fractionExplained




write.table(rbind(women,men),file=heights_pre_registered_file,col.names=TRUE,row.names=FALSE,quote=FALSE,sep="\t")
