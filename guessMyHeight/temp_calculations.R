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
  effect_allele<-giant_sup[snp,"effect_allele"]
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

source("/home/ubuntu/srv/impute-me/functions.R")
genotypes<-get_genotypes(uniqueID="id_424142906",request=giant_sup)


gheight_score<-0
for(snp in rownames(giant_sup)){
  if(is.na(genotypes[snp,]))next
  genotype<-strsplit(genotypes[snp,],"/")[[1]]
  effect_allele<-giant_sup[snp,"effect_allele"]
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









#2017-05-27 re-doing the SNPs_to_analyze

data<-read.table("hairColour/SNPs_to_analyze.txt",header=T,sep="\t",comment.char="",quote="",stringsAsFactors=F)
data<-data[!(data[,"SNP"]%in%"rs12931267" & data[,"Category"]%in%"red"),]
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
# data[which(nchar(data[,"ensembl_alleles"])==5),"SNP"]


#assign major allele from the ensembl alleles
a1<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[1]})
a2<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[2]})
data[,"major_allele"] <- NA
data[data[,"minor_allele"]==a1,"major_allele"]<-a2[data[,"minor_allele"]==a1]
data[data[,"minor_allele"]==a2,"major_allele"]<-a1[data[,"minor_allele"]==a2]


#check some cases were minor allele is not even found in the ensembl alleles. These should probably be removed; they are cases were ensembl_alleles is on the opposite strand. Could be flipped, but better to be safe.
data[is.na(data[,"major_allele"]),"minor_allele"]<-"?"
data[is.na(data[,"major_allele"]),"major_allele"]<-"?"




#special for here: briefly rename effect_allele to risk_allele (and remove effect and non_effect)
data[,"risk_allele"] <- data[,"effect_allele"]
data[,"effect_allele"]<-NULL
data[,"non_effect_allele"]<-NULL
data[,"non_risk_allele"]<-NULL
data[,"study_id"]<-NULL


#check cases were risk allele is not found in minor or major allele
sum(!(data[,"risk_allele"] %in% data[,"minor_allele"] | data[,"risk_allele"] %in% data[,"major_allele"] ))
#0




#check places were the risk-allele is not found in ensembl minor or major allele
non_matching<-which(data[,"risk_allele"] != data[,"minor_allele"] & data[,"risk_allele"] != data[,"major_allele"])

#insert the non-risk allele as being the allele that is not risk, and is the other allele (taking from major/minor info)
data[data[,"minor_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"minor_allele"]==data[,"risk_allele"],"major_allele"]
data[data[,"major_allele"]==data[,"risk_allele"],"non_risk_allele"]<-data[data[,"major_allele"]==data[,"risk_allele"],"minor_allele"]

data[which(!(data[,"minor_allele"]==data[,"risk_allele"] | data[,"major_allele"]==data[,"risk_allele"])),c("risk_allele","major_allele","minor_allele")]


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





#re-order colnames so that the essential are first
colnames(data)
putFirst<-c("SNP", "chr_name","risk_allele","non_risk_allele","Beta",  "minor_allele_freq","minor_allele","major_allele")
data<-data[,c(putFirst,colnames(data)[!colnames(data)%in%putFirst])]
colnames(data)[1]<-"SNP"
colnames(data)[3]<-"effect_allele"
colnames(data)[4]<-"non_effect_allele"
colnames(data)[5]<-"Beta"




write.table(data,file="hairColour/SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F,sep="\t")










#2017-05-27 re-doing hair colour so it reports using the get_GRS_2 function, relative to normal distribution insted of the previous arbitrary assignment
rm(list=ls())




functions_path <- paste0(srv_path,"functions.R")
source(functions_path)
snps_path<-paste0(srv_path,"hairColour/SNPs_to_analyze.txt")
snps<-read.table(snps_path, stringsAsFactors = F, header=T,sep="\t")




data_files<-data.frame(row.names=list.files(data_path))


for(f in rownames(data_files)){
  for(col in c("red","brown")){
    s1<-snps[snps[,"Category"]%in%col,]
    rownames(s1)<-s1[,"SNP"]
    genotypes<-try(get_genotypes(f,s1,gtools="~/gtool"))
    if(class(genotypes)=="try-error")next
    s1[,"genotype"]<-genotypes
    s1<-get_GRS_2(s1,mean_scale=T,unit_variance=T)
    population_sum_sd<-sqrt(sum(s1[,"population_score_sd"]^2,na.rm=T))
    GRS <-sum(s1[,"score_diff"],na.rm=T) / population_sum_sd  
    data_files[f,col]<-GRS
    
    pdata<-read.table(paste0(data_path,f,"/pData.txt"),header=T,stringsAsFactors = F,sep="\t")
    if("red_hair"%in%colnames(pdata)){
      data_files[f,"red_hair"]<-pdata[,"red_hair"]  
    }
    if("blonde_hair"%in%colnames(pdata)){
      data_files[f,"blonde_hair"]<-pdata[,"blonde_hair"]  
    }
    
  }
}


pdf("2017-05-27_distributions.pdf")
hist(data_files[,"brown"],xlab="blonde GRS",main="blondeness distribution")
caucasian<-data_files["id_828661c63","brown"]
abline(v=caucasian,col="sandybrown",lwd=2,lty=2)
chinese<-data_files["id_13896s006","brown"]
abline(v=chinese,col="black",lwd=2,lty=2)

hist(data_files[,"red"],xlab="red GRS",main="redness distribution")
caucasian<-data_files["id_828661c63","red"]
abline(v=caucasian,col="sandybrown",lwd=2,lty=2)
chinese<-data_files["id_13896s006","red"]
abline(v=chinese,col="black",lwd=2,lty=2)


plot(data_files[,"brown"],data_files[,"blonde_hair"],xlab="genetic blonde",ylab="self-reported blonde")
plot(data_files[,"red"],data_files[,"red_hair"],xlab="genetic redhead",ylab="self-reported redhead")

dev.off()



