

export_function<-function(uniqueID){
  snps_file<-paste0(get_conf("code_path"),"ukbiobank/2017-09-28_semi_curated_version_ukbiobank.rdata")
  trait_file<-paste0(get_conf("code_path"),"ukbiobank/2017-09-28_trait_overoverview.rdata")
  
  #testing
  #preload
  load(snps_file)
  load(trait_file)
  traits<-traits[!traits[,"omit"],]
  
  if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  
  output<-list()
  output[["documentation"]] <- list()
  output[["documentation"]][["trait_overview"]] <- "https://github.com/lassefolkersen/impute-me/blob/14ba4b697185898135755ec69e36fe3cdad5c8e1/ukbiobank/2017-09-28_trait_overoverview.rdata"
  output[["documentation"]][["snp_file"]] <- "https://github.com/lassefolkersen/impute-me/blob/31846520f7bb84c3ab079b5fd0ea202bbf30b844/ukbiobank/2017-09-28_semi_curated_version_ukbiobank.rdata"
  
  
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
    
    
    output[[study_id]]<-list()
    output[[study_id]][["GRS"]] <- GRS_beta
    output[[study_id]][["trait"]] <- traits[study_id,"niceName"]
  }
  
  
  
  return(output)
  
}




