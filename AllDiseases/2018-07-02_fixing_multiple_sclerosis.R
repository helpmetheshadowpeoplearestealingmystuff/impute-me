

rm(list=ls())
library(openxlsx)
pmid <- "24076602"

a<-read.xlsx("AllDiseases/2018-07-02_multiple_sclerosis.xlsx")
load("AllDiseases/2018-05-28_semi_curated_version_gwas_central.rdata")


w1<-which(data[,"PUBMEDID"] %in% pmid)



data[w1,"SNP"] %in% a[,"rsID"]
#most are found in this data. Good - so we can assign the correct risk/non-risk direction. Only one missing

#all odds-ratios are given as OR>1, so use the column risk allele to fix 

#ready for flip
nuc<-c("A","T","C","G")
names(nuc) <- c("T","A","G","C")
count <-0
for(w2 in w1){
  if(!data[w2,"SNP"]%in% a[,"rsID"])next
  
  
  new_data <-a[a[,"rsID"]%in%data[w2,"SNP"],]
  if(nrow(new_data)!=1)stop("not only one?")

  #the problem is strand-flip (of course), so we need to check RAF
  
  risk_allele_frequency_2 <- new_data[1,"RAF"]
  risk_allele_2 <- new_data[1,"RA"]
  
  minor_allele_frequency_1 <- data[w2,"EUR_AF"] #this seems to work better than minor_allele_freq (=closer to new_data)
  minor_allele_1 <- data[w2,"minor_allele"]
  major_allele_1 <- data[w2,"major_allele"]
  effect_allele_1 <- data[w2,"effect_allele"]
  non_effect_allele_1 <- data[w2,"non_effect_allele"]
  
  #at least be aware of the A/T C/G problems
  if(
    (minor_allele_1 == "T" & major_allele_1 == "A") |
    (minor_allele_1 == "A" & major_allele_1 == "T") |
    (minor_allele_1 == "C" & major_allele_1 == "G") |
    (minor_allele_1 == "G" & major_allele_1 == "C")
  ){at_prob <- TRUE}else{ at_prob<-FALSE}
    
  
  #check RA and risk
  if(risk_allele_2 %in% c(minor_allele_1,major_allele_1)){
    #in this case we assume there is no strandflip
    if(!risk_allele_2%in%c(effect_allele_1,non_effect_allele_1))stop("This is odd")
    
    
    
  }else{
    #in this case we assume there is a strandflip
    effect_allele_1<-nuc[risk_allele_2]
    non_effect_allele_1<-c(minor_allele_1,major_allele_1)[!c(minor_allele_1,major_allele_1)%in%effect_allele_1]
    
    # print(paste("Replacing",data[w2,"effect_allele"],"with",effect_allele_1,"and",data[w2,"non_effect_allele"],"with",non_effect_allele_1))
    
    data[w2,"effect_allele"] <- effect_allele_1
    data[w2,"non_effect_allele"] <- non_effect_allele_1
    
    
    
  }
  
  #double checking
  if(effect_allele_1 == minor_allele_1 & non_effect_allele_1 == major_allele_1){
    
    diff <- risk_allele_frequency_2 - minor_allele_frequency_1
  }else if(effect_allele_1 == major_allele_1 & non_effect_allele_1 == minor_allele_1){
    diff <- risk_allele_frequency_2 - (1-minor_allele_frequency_1)
    
    
  }else{stop("!!!")}
  
  
  
  if(is.na(diff) || diff > 0.2){
    
    #this happens five times. We should just put them as NA
    data[w2,"effect_allele"] <- "?"
    data[w2,"non_effect_allele"] <- "?"
    
    
  }
}


# data[w1,]


save(data,file="AllDiseases/2018-05-28_semi_curated_version_gwas_central.rdata")
