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

  
  #order so pathogenic is always on top
  order<-c('Pathogenic','Likely pathogenic','Conflicting interpretations of pathogenicity','Uncertain significance','not provided','Likely benign','Benign')
  BRCA_table[,"clinvar"]<-factor(BRCA_table[,"clinvar"],levels=order)
  BRCA_table<-BRCA_table[order(BRCA_table[,"clinvar"]),]
  
  # BRCA_table[,"chr_name"]<-BRCA_table[,"chrom_start"]<-BRCA_table[,"SNP"]<-NULL
  
  
  
  output<-list()
  output[["differing_snps"]]<-vector()
  
  for(snp in rownames(BRCA_table)){
    
    if(is.na(BRCA_table[snp, "Your genotype"]))next
    output[[snp]]<-list()
    output[[snp]][["clinvar"]] <- as.character(BRCA_table[snp, "clinvar"])
    output[[snp]][["polyphen_prediction"]] <- BRCA_table[snp, "polyphen_prediction"]
    output[[snp]][["sift_prediction"]] <- BRCA_table[snp, "sift_prediction"]
    output[[snp]][["consequence_type_tv"]] <- BRCA_table[snp, "consequence_type_tv"]
    output[[snp]][["gene"]] <- BRCA_table[snp, "gene"]
    output[[snp]][["normal"]] <- BRCA_table[snp, "normal"]
    output[[snp]][["Your_genotype"]] <- BRCA_table[snp, "Your genotype"]
    
    
    if(!is.na(BRCA_table[snp, "normal"]) & !is.na(BRCA_table[snp, "Your genotype"])){
      if(BRCA_table[snp, "Your genotype"] != BRCA_table[snp, "normal"]){
        output[["differing_snps"]] <- c(output[["differing_snps"]], snp)
      }
    }
  }
  
  
  return(output)
  
}




