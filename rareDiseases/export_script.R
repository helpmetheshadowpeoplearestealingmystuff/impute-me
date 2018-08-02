source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  #start check ups
  
  
  if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  
  table_file <-"/home/ubuntu/srv/impute-me/rareDiseases/SNPs_to_analyze.txt"
  request <- table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
  
  #get data
  request<-request[!duplicated(request[,"SNP"]),]
  rownames(request) <- request[,"SNP"]
  genotypes<-get_genotypes(uniqueID=uniqueID,request=request )
  
  #remove the iXXXX
  table<-table[grep("^i",table[,"SNP"],invert=T),]
  table<-table[order(table[,"disease_name"]),]
  
  #more intelligible comment
  table[grep("^original",table[,"comment"]),"comment"] <-"rs-id from original 23andme"
  
  #adding genotypes in (many will be missing unfortunately)
  table[,"Your genotype"]<-genotypes[table[,"SNP"],]
  
  
  #generating advice
  table[,"First_allele"]<-substr(table[,"Your genotype"],1,1)
  table[,"Second_allele"]<-substr(table[,"Your genotype"],3,3)
  table[,"First_carrier"]<-table[,"First_allele"]==table[,"risk_allele"]
  table[,"Second_carrier"]<-table[,"Second_allele"]==table[,"risk_allele"]
  diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
  diseases_of_interest<-diseases_of_interest[!is.na(diseases_of_interest)]
  if(length(diseases_of_interest)==0){
    m <- "There's no particular inherited conditions that you should pay attention to, according to this analysis"	
  }else if(length(diseases_of_interest)==1){
    m <- paste("According to this analysis, you should pay particular attention to the inherited condition:",diseases_of_interest)
  }else{
    m <- paste("According to this analysis, you should pay particular attention to these",length(diseases_of_interest),"inherited conditions:",paste(diseases_of_interest,collapse=", "))	
  }
  
  
  table<-table[,c("SNP","Your genotype","risk_allele","non_risk_allele","disease_name")]
  colnames(table)<-c("SNP","Your genotype","Risk-allele","Non-Risk-allele","Inherited Condition")
  
  
  
  output<-list(
    advice=m,
    diseases_of_interest=diseases_of_interest,
    all_findings=table)
  
  return(output)
  
}




