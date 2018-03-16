
#2018-03-15 Adding more neuro studies
rm(list=ls())
library(openxlsx)


#IQ
d<-read.xlsx("../Copy of 41380_2017_1_MOESM2_ESM.xlsx")
w<-which(apply(d,1,function(x){x["Independent.significant.SNPs"] == x["Tagged.SNPs"]}))
any(duplicated(d[w,"Tagged.SNPs"]))
d1<-d[w,]
rownames(d1)<-d1[,"Tagged.SNPs"]

# head(d1)

d2<-data.frame(
  SNP=d1[,'Tagged.SNPs'],
  chr_name=d1[,'Chromosome'],
  effect_allele=d1[,'effect.allele'],
  non_effect_allele=d1[,'non-effect.allele'],
  effect_size=d1[,'Beta'],
  minor_allele_freq=d1[,'MAF.of.tagged.SNP'],
  minor_allele=NA,
  major_allele=NA,
  PUBMEDID="29326435" ,
  FIRST.AUTHOR="Hill W.D.",
  DATE="2017-11-03",
  LINK="www.ncbi.nlm.nih.gov/pubmed/29326435",
  DISEASE.TRAIT="Intelligence",
  REGION=NA,
  REPORTED.GENE.S.=d1[,'Nearest.Gene'],
  STRONGEST.SNP.RISK.ALLELE=NA,
  P.VALUE=d1[,'P-value.of.tagged.SNP'],
  X95..CI..TEXT.=NA,
  sampleSize=199242,
  ensembl_alleles=NA,
  study_id="intelligence_29326435",
  EAS_AF=NA,
  AMR_AF=NA,
  AFR_AF=NA,
  EUR_AF=NA,
  SAS_AF=NA,
  stringsAsFactors = F
)

#EQ
a1<-read.xlsx("../41398_2017_82_MOESM2_ESM.xlsx",sheet=2)
rownames(a1) <- a1[,"SNP"]

# head(a1)
a2<-data.frame(
  SNP=a1[,'SNP'],
  chr_name=a1[,'Chr'],
  effect_allele=a1[,'Effect.allele'],
  non_effect_allele=a1[,'Other.allele'],
  effect_size=a1[,'Effect'],
  minor_allele_freq=a1[,'Freq.Effect.allele'],
  minor_allele=a1[,'Effect.allele'],
  major_allele=a1[,'Other.allele'],
  PUBMEDID="29527006" ,
  FIRST.AUTHOR="Warrier V",
  DATE="2018-01-03",
  LINK="www.ncbi.nlm.nih.gov/pubmed/29527006",
  DISEASE.TRAIT="Emotional Intelligence",
  REGION=NA,
  REPORTED.GENE.S.=a1[,'Nearest.Gene.(GENCODE)'],
  STRONGEST.SNP.RISK.ALLELE=NA,
  P.VALUE=a1[,'P'],
  X95..CI..TEXT.=NA,
  sampleSize=46861,
  ensembl_alleles=NA,
  study_id="emotional_Intelligence_29527006",
  EAS_AF=NA,
  AMR_AF=NA,
  AFR_AF=NA,
  EUR_AF=NA,
  SAS_AF=NA,
  stringsAsFactors = F
)


data<-rbind(d2,a2)
save(data,file="2018-03-15_temp_data.rdata")




# load("AllDiseases/2017-02-21_semi_curated_version_gwas_central.rdata")
rm(list=ls())
load("2018-03-15_temp_data.rdata")
path_1kg<-"/home/people/lasfol/downloadBulk/annotation/1000_genomes_20130502/"



output<-data.frame("CHROM"=vector(), "POS"=vector(),   "REF"=vector(),   "ALT"=vector(),   "AF"=vector(),    "EAS_AF"=vector(),"AMR_AF"=vector(),"AFR_AF"=vector(),"EUR_AF"=vector(),"SAS_AF"=vector(),stringsAsFactors = F)

for(chr in sort(unique(data[,"chr_name"]))){
  print(chr)
  g1<-data[data[,"chr_name"]%in%chr,]
  snps<-unique(g1[,"SNP"])
  
  write.table(snps,file="temp_list_of_snps.txt",sep="\t",col.names=F,row.names=F,quote=F)
  
  if(chr =="X"){
    vcf_path<-paste0(path_1kg,"ALL.chrX.phase3_shapeit2_mvncall_integrated_v1b.20130502.genotypes.vcf.gz")
  }else{
    vcf_path<-paste0(path_1kg,"ALL.chr",chr,".phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz")
  }
  cmd1<-paste0("vcftools --indv HG00096 --snps temp_list_of_snps.txt --gzvcf ",vcf_path," --out chr",chr," --recode --recode-INFO-all")
  system(cmd1)
  
  cmd2 <- paste0("cut -f 3 chr",chr,".recode.vcf")
  snps<-grep("^[#I]",system(cmd2,inter=T),value=T,invert=T)
  
  cmd3 <- paste0("vcftools --vcf chr",chr,".recode.vcf --get-INFO AF --get-INFO EAS_AF --get-INFO AMR_AF --get-INFO AFR_AF --get-INFO EUR_AF --get-INFO SAS_AF --out chr",chr)
  system(cmd3)
  
  result<-read.table(paste0("chr",chr,".INFO"),sep="\t",header=T,stringsAsFactors = F)
  rownames(result)<-snps
  output <- rbind(output, result)
  
}

# cmd2<-paste0("cat ",paste(paste0("chr",c(1:22,"X"),".INFO"), collapse=" ")," > 2017-03-24_all_frequencies.txt")
# system(cmd2)

write.table(output,file="2018-03-15_all_frequencies.txt",sep="\t",col.names=NA)


#on local
rm(list=ls())
load("intelligence/2018-03-15_temp_data.rdata")
d<-read.table("intelligence/2018-03-15_all_frequencies.txt",sep="\t",header=T,stringsAsFactors = F,row.names=1)


#fix tri-allelics
trialleic_snps <- rownames(d[c(142, 199, 207, 405, 567),])
for(trialleic_snp in trialleic_snps){
  d[trialleic_snp,]
  w1<-which(data[,"SNP"]%in%trialleic_snp)
  ea <-data[w1,"effect_allele"]
  nea <-data[w1,"non_effect_allele"]
  
  ref<-d[trialleic_snp,"REF"]
  alt<-d[trialleic_snp,"ALT"]
  
  #choose which entry (1 or 2)
  if(nea == ref){
    w2<-which(strsplit(alt,",")[[1]]%in%ea)
  }else{
    w2<-which(strsplit(alt,",")[[1]]%in%nea)
  }
  
  #insert right w2
  d[trialleic_snp,"ALT"] <- strsplit(alt,",")[[1]][w2]
  for(col in grep("AF",colnames(d),value=T)){
    d[trialleic_snp,col] <- strsplit(d[trialleic_snp,col],",")[[1]][w2]
  }
}



for(col in grep("AF",colnames(d),value=T)){
  d[,col]<-as.numeric(d[,col])
}


colnames(d)[colnames(d)%in%"AF"]<-"minor_allele_freq"

for(snp in rownames(d)){
  
  
  w<-which(data[,"SNP"] %in% snp)
  if(length(w)!=1)stop()
  
  
  
  
  
  if(d[snp,"minor_allele_freq"] < 0.5){
    data[w,"minor_allele"] <- d[snp,"ALT"]
    data[w,"major_allele"] <- d[snp,"REF"]
    flip <- F
  }else{
    data[w,"minor_allele"] <- d[snp,"REF"]
    data[w,"major_allele"] <- d[snp,"ALT"]
    flip <- T
  }
  
  cols<-c("minor_allele_freq", "EAS_AF", "AMR_AF", "AFR_AF", "EUR_AF", "SAS_AF")
  for(col in cols){
    
    if(flip){
      f<-1-d[snp,col] 
    }else{
      f<-d[snp,col] 
    }
    data[w,col]<-f
  }
  # data[w,"effect_allele"] <- "?"
  # data[w,"non_effect_allele"] <- "?"
}

write.table(data,file="intelligence/SNPs_to_analyze.txt",row.names=F,col.names=T,quote=F,sep="\t")


# 
# #adding the all_gwas
# rm(list=ls())
# load("intelligence/2018-03-16_semi_curated_version_gwas_central.rdata")
# d1<-data[,c("SNP","chr_name","effect_allele","non_effect_allele")]
# rownames(d1)<-d1[,"SNP"]
# write.table(d1,file="intelligence/SNPs_to_analyze.txt",row.names=F,col.names=T,quote=F,sep="\t")
# 



