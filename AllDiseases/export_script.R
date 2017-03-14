source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  # dataFolder<-"/home/ubuntu/data/"
  snps_file<-"/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_semi_curated_version_gwas_central.rdata"
  trait_file<-"/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_trait_overoverview.rdata"
  
  #testing
  #preload
  load(snps_file)
  load(trait_file)
  
  
  if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  
  output<-list()
  
  
  for(study_id in rownames(traits)){
    
    SNPs_to_analyze<-data[data[,"study_id"]%in%study_id ,]
    
    
    SNPs_requested<-SNPs_to_analyze[!duplicated(SNPs_to_analyze[,"SNP"]),]
    rownames(SNPs_requested)<-SNPs_requested[,"SNP"]
    genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_requested, namingLabel="cached.all_gwas")
    
    snp_data<-SNPs_requested
    snp_data[,"genotype"] <- genotypes[rownames(snp_data),"genotype"]
    snp_data <-get_GRS_2(snp_data,mean_scale=T, unit_variance=T, verbose=T)
    population_sum_sd<-sqrt(sum(snp_data[,"population_score_sd"]^2))
    GRS_beta <-sum(snp_data[,"score_diff"],na.rm=T) / population_sum_sd
    
    
    output[[study_id]]<- GRS_beta
           
           
  }
  
  
  
  return(output)
  
}




