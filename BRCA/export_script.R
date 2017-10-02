source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  #start check ups
  
  
  if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  
  
  
  BRCA_table_file <-"/home/ubuntu/srv/impute-me/BRCA/SNPs_to_analyze.txt"
  BRCA_table<-read.table(BRCA_table_file,sep="\t",header=T,stringsAsFactors=F)
  
  rownames(BRCA_table)<-BRCA_table[,"SNP"]
  BRCA_table[BRCA_table[,"chr_name"]%in%13,"gene"]<-"BRCA2"
  BRCA_table[BRCA_table[,"chr_name"]%in%17,"gene"]<-"BRCA1"
  
  BRCA_table["i4000377","gene"]<-"BRCA1"
  BRCA_table["i4000378","gene"]<-"BRCA1"
  BRCA_table["i4000379","gene"]<-"BRCA2"
  
  BRCA_table["i4000377","consequence_type_tv"]<-"Direct from 23andme"
  BRCA_table["i4000378","consequence_type_tv"]<-"Direct from 23andme"
  BRCA_table["i4000379","consequence_type_tv"]<-"Direct from 23andme"
  
  
  #get genotypes 
  genotypes<-get_genotypes(uniqueID=uniqueID,request=BRCA_table)
  BRCA_table[,"Your genotype"]<-genotypes[rownames(BRCA_table),]

  
  BRCA_table[,"chr_name"]<-BRCA_table[,"chrom_start"]<-BRCA_table[,"SNP"]<-NULL
  
  
  output<-list(BRCA_table)
  
  return(output)
  
}




