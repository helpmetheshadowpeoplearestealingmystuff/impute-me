

export_function<-function(uniqueID){
  snps_file<-paste0(get_conf("code_path"),"AllDiseases/2021-01-28_snp_weights.rdata")
  trait_file<-paste0(get_conf("code_path"),"AllDiseases/2021-01-28_trait_overview.xlsx")
  
  #testing
  #preload
  library(openxlsx)
  load(snps_file)
  traits <- read.xlsx(trait_file,rowNames=T)
  traits<-traits[!is.na(traits[,"omit"]) & !traits[,"omit"],]  
  
  if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  
  output<-list()
  output[["documentation"]] <- list()
  output[["documentation"]][["trait_overview"]] <- "https://github.com/lassefolkersen/impute-me/blob/5a31c3d703281a511e5f750d1ba05601b6ac2c36/AllDiseases/2021-01-28_trait_overview.xlsx"
  output[["documentation"]][["snp_file"]] <- "https://github.com/lassefolkersen/impute-me/blob/5a31c3d703281a511e5f750d1ba05601b6ac2c36/AllDiseases/2021-01-28_snp_weights.rdata"
  
  #get ethnicity parameter
  pDataFile<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
  pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
  if(class(pData)!="try-error" && "ethnicity" %in% colnames(pData)){
    ethnicity <-pData[1,"ethnicity"]
  }else{
    ethnicity <-"global"
  }
  
  
  
  for(study_id in rownames(traits)){
    
    SNPs_to_analyze<-data[data[,"study_id"]%in%study_id ,]
    
    #get genotypes
    SNPs_requested<-SNPs_to_analyze[!duplicated(SNPs_to_analyze[,"SNP"]),]
    rownames(SNPs_requested)<-SNPs_requested[,"SNP"]
    genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_requested, namingLabel="cached.all_gwas")
    
    
    #get correct ethnicity minor_allele_frequency
    if(ethnicity %in% c("EAS","AMR","AFR","EUR","SAS")){
      SNPs_requested[,"minor_allele_freq"]<-SNPs_requested[,paste0(ethnicity,"_AF")]
    }
    
    #calculate GRS
    snp_data<-SNPs_requested
    snp_data[,"genotype"] <- genotypes[rownames(snp_data),"genotype"]
    snp_data <-get_GRS_2(snp_data,mean_scale=T, unit_variance=T)
    population_sum_sd<-sqrt(sum(snp_data[,"population_score_sd"]^2,na.rm=T))
    GRS_beta <-signif(sum(snp_data[,"score_diff"],na.rm=T) / population_sum_sd,4)
    
    output[[study_id]] <- list()
    output[[study_id]][["GRS"]]<- GRS_beta
    output[[study_id]][["trait"]]<- NA
           
           
  }
  
  
  
  return(output)
  
}




