source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  #start check ups
  
  
  if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  
  SNPs_to_analyze_file<-paste0("/home/ubuntu/srv/impute-me/","autoimmuneDiseases/2016-05-21_SNPs_to_analyze_SOURCE.txt")
  
  means_file<-paste0("/home/ubuntu/srv/impute-me/","autoimmuneDiseases/2016-05-21_means.txt")
  means<-suppressWarnings(read.table(means_file,sep="\t",header=T,row.names=1,stringsAsFactors=F))
  
  
  diseaseNames<-rbind(
    c("RA","Rheumatoid Arthritis","okada"),
    c("UC","Ulcerative colitis","ellinghaus"),
    c("CD","Crohn’s disease","ellinghaus"),
    c("PS","Psoriasis","ellinghaus"),
    c("PSC","Primary Sclerosing Cholangitis","ellinghaus"),
    c("AS","Ankylosing Spondylitis","ellinghaus")
  )
  colnames(diseaseNames)<-c("Acronym","Disease","Source")
  rownames(diseaseNames)<-diseaseNames[,"Acronym"]

  output<-list()
  
  for(disease in diseaseNames[,"Acronym"]){
    output[[disease]]<-list()
    source<-diseaseNames[disease,"Source"]
    SNPs_to_analyze<-read.table(sub("SOURCE",source,SNPs_to_analyze_file),sep="\t",stringsAsFactors=F,header=T,row.names=1)
    genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
    
    #get risk score
    if(source == "ellinghaus"){
      or_column<-paste0("OR.",disease,".")
    }else if(source=="okada"){
      or_column<-"OR"
    }else{stop("!")}
    
    SNPs_to_analyze[,"Beta"]<-log10(SNPs_to_analyze[,or_column])
    GRS_beta <-get_GRS(genotypes=genotypes,betas=SNPs_to_analyze)
    output[[disease]][["GRS_beta"]] <- GRS_beta
    
    output[[disease]][["case_mean"]]<-means[disease,"case_mean"]
    output[[disease]][["case_sd"]]<-means[disease,"case_sd"]
    output[[disease]][["control_mean"]]<-means[disease,"control_mean"]
    output[[disease]][["control_sd"]]<-means[disease,"control_sd"]
    
    output[[disease]][["control_prob"]]<-signif(100*pnorm(GRS_beta,mean=output[[disease]][["control_mean"]],sd=output[[disease]][["control_sd"]]),4)
    
    output[[disease]][["case_prob"]]<-signif((1-pnorm(GRS_beta,mean=output[[disease]][["case_mean"]],sd=output[[disease]][["case_sd"]]))*100,4)
    
    
    
      
  }
  
  
  
  return(output)
  
}




