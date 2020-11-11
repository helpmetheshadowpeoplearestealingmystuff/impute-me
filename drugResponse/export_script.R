source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  table_file <-"/home/ubuntu/srv/impute-me/drugResponse/SNPs_to_analyze.txt"
  SNPs_to_analyze<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
  SNPs_to_analyze[,"PMID"] <- as.character(SNPs_to_analyze[,"PMID"])
  
  output<-list()
  output[["documentation"]] <- list()
  output[["documentation"]][["source"]] <- "https://github.com/lassefolkersen/impute-me/blob/31846520f7bb84c3ab079b5fd0ea202bbf30b844/drugResponse/SNPs_to_analyze.txt"

  
  #retrieving SNPs
  SNPs_to_retrieve<-SNPs_to_analyze
  SNPs_to_retrieve<-SNPs_to_retrieve[!duplicated(SNPs_to_retrieve[,"SNP"]),]
  rownames(SNPs_to_retrieve) <- SNPs_to_retrieve[,"SNP"]
  SNPs_to_retrieve<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_retrieve)
  #inserting SNPs and calculating GRS
  SNPs_to_analyze[,"genotype"] <- SNPs_to_retrieve[SNPs_to_analyze[,"SNP"],"genotype"]
  
  
  for(study in unique(SNPs_to_analyze[,"PMID"])){
    
    d2<-SNPs_to_analyze[SNPs_to_analyze[,"PMID"]%in%study,]
    disease <- unique(d2[,"disease"])
    drug <- unique(d2[,"drug"])

    
    #handling duplicated SNPs
    if(any(duplicated(d2[,"SNP"]))){
      d2<-d2[!duplicated(d2[,"SNP"]),]
      #suggestion for future: add in a note in the return text that we have removed the duplicates?
    }
    
    rownames(d2)<-d2[,"SNP"]
    d3<-try(get_GRS_2(d2, mean_scale=T, unit_variance=T, verbose=F))
    
    
    
    
    if(class(d3)=="try-error"){
      z_score <- "Not calculated"
      percentage <- "Not calculated"
      
    }else{
      population_sum_sd<-sqrt(sum(d3[,"population_score_sd"]^2,na.rm=T))
      if(population_sum_sd == 0){
        z_score <- "Not calculated"
        percentage <- "Not calculated"
      }else{
        GRS <-sum(d3[,"score_diff"],na.rm=T) / population_sum_sd
        percentage<-floor(pnorm(GRS,mean=0,sd=1)*100)
        z_score <- signif(GRS,2)
        percentage <- signif(percentage,2)
        
      }
      
    }
    
    
    output[[study]] <-
      list(
        disease=disease,
        drug=drug,
        z_score=z_score,
        percentage=percentage
      )
    
  }
  
  
  
  return(output)
  
}




