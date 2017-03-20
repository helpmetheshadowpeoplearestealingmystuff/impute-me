
rm(list=ls())
source("functions_local.R")


generate_person<-function(n_snps=100,seed=42){
  set.seed(seed)
  snps<-paste0("rs",sample(100000:999999,n_snps))
  if(any(duplicated(snps)))stop("!")
  snp_data <- data.frame(row.names=snps)
  
  
  snp_data[,"effect_allele"] <- sample(c("C","T"),n_snps,replace=T)
  snp_data[,"non_effect_allele"] <- sample(c("A","G"),n_snps,replace=T)
  snp_data[,"effect_size"] <- runif(n_snps,min=0.4,max=3.5)
  snp_data[,"minor_allele_freq"] <- runif(n_snps,min=0.1,max=0.45)
  snp_data[,"minor_allele"] <- snp_data[,"effect_allele"]
  snp_data[,"major_allele"] <- snp_data[,"non_effect_allele"]
  flip<-sample(1:n_snps,n_snps/2)
  r1<-snp_data[flip,"major_allele"]
  r2<-snp_data[flip,"minor_allele"]
  snp_data[flip,"major_allele"]<-r2
  snp_data[flip,"minor_allele"]<-r1
  
  return(snp_data)
}  

generate_genotypes<-function(snp_data,seed=42){
  for(snp in rownames(snp_data)){
    maf<-snp_data[snp,"minor_allele_freq"]
    s<-sample(c(snp_data[snp,"minor_allele"],snp_data[snp,"major_allele"]),2,replace=T,prob=c(maf,1-maf))
    snp_data[snp,"genotype"] <- paste(sort(s),collapse="/")
  }
  return(snp_data)
}



d<-data.frame(GRS2=vector())
snp_data<-generate_person(seed=1+3)

for(i in 1:200){
  
  snp_data[,"genotype"]<-generate_genotypes(snp_data,seed=i+4)[,"genotype"]
  
  snp_data<-get_GRS_2(snp_data,mean_scale=T, unit_variance=T, verbose=T)
  population_sum_sd<-sqrt(sum(snp_data[,"population_score_sd"]^2))
  GRS <-sum(snp_data[,"score_diff"],na.rm=T) / population_sum_sd
  
  d[i,"GRS"]<-sum(snp_data[,"score_diff"],na.rm=T)
  d[i,"population_sum_sd"]<-population_sum_sd
  d[i,"GRS_normalized"]<-GRS
  
}


hist(d[,"GRS"])
d[,"population_sum_sd"]
hist(d[,"GRS_normalized"])
sum(d[,"GRS_normalized"] > -1 & d[,"GRS_normalized"] < 1) / nrow(d)
#ok - looks better. This should preserve the weighting while still being mean-scaled unit-variance


