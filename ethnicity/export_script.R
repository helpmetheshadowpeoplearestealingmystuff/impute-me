

export_function<-function(uniqueID){
  
  
  if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  
  output<-list()
  
  
  #get input constants
  load(paste0(get_conf("code_path"),"ethnicity/2017-04-03_ethnicity_snps.rdata"))
  load(paste0(get_conf("code_path"),"ethnicity/2017-04-03_ethnicity_pca.rdata"))
  ethnicity_desc<-read.table(paste0(get_conf("code_path"),"ethnicity/2017-04-03_ethnicity_descriptions.txt"),sep="\t",header=T,stringsAsFactors = F,row.names=1)
  
  
  
  #get genotypes
  genotypes<-get_genotypes(uniqueID=uniqueID,request=ethnicity_snps, namingLabel="cached.ethnicity")
  ethnicity_snps[,"genotype"]<-genotypes[rownames(ethnicity_snps),"genotype"]
  get_alt_count <- function(x){sum(strsplit(x["genotype"],"/")[[1]]%in%x["alt"])}
  ethnicity_snps[,"alt_count"]<-apply(ethnicity_snps,1,get_alt_count)
  
  
  
  #get missing SNP counts
  found <- sum(!is.na(ethnicity_snps[,"genotype"]))
  QC_conclusion<-paste("Based on",found,"of",nrow(ethnicity_snps),"pre-computed ethnicity SNPs.")
  if(found < 1500){
    QC_conclusion<-paste(QC_conclusion,"This is a low number and may be a serious problem for the ancestry call.")
  }
  
  
  
  #quick-calculate the PCA metrics for this person
  you<-data.frame(pop="YOU", super_pop="YOU", gender=NA,stringsAsFactors = F)
  for(pc in 1:5){
    val<-sum(((ethnicity_snps[,"alt_count"] - ethnicity_snps[,"center"])/ethnicity_snps[,"scale"]) * ethnicity_snps[,paste0("rot_PC",pc)])
    you[,paste0("pos_PC",pc)]<-val
  }
  pca<-rbind(pca_data,you)
  
  
  #pick some colours for each super population (first dilute their alpha a little)
  colours <- ethnicity_desc[,"Col"]
  names(colours) <- ethnicity_desc[,"PopulationDescription"]
  
  #also get the long descriptor of each populations
  pca[,"pop_long"]<-ethnicity_desc[pca[,"pop"],"PopulationDescription"]
  
  
  
  
  #calculate closest superpopulation (just use geometric distance. A little low tech but should be ok for sure cases)
  y<-which(pca[,"pop"]%in%"YOU")
  pca[,"distance"]<-sqrt((pca[,"pos_PC1"] - pca[y,"pos_PC1"])^2 + (pca[,"pos_PC2"] - pca[y,"pos_PC2"])^2 + (pca[,"pos_PC3"] - pca[y,"pos_PC3"])^2)
  pca<-pca[order(pca[,"distance"]),]
  guessed_super_pop<-unique(pca[2:6,"super_pop"])
  if(length(guessed_super_pop)!=1)guessed_super_pop<-NA #if there's more than one superpop among closest 5 - then we don't want to guess
  if(found < 1500)guessed_super_pop<-NA #also don't guess if too many SNPs were missing
  output[["guessed_super_pop"]]<- guessed_super_pop

  
  #adding in some coordinates to the JSON
  output[["PCA_coordinates"]] <- list()
  output[["PCA_coordinates"]][["PC1"]] <- pca[pca[,"pop"]%in%"YOU","pos_PC1"]
  output[["PCA_coordinates"]][["PC2"]] <- pca[pca[,"pop"]%in%"YOU","pos_PC2"]
  output[["PCA_coordinates"]][["PC3"]] <- pca[pca[,"pop"]%in%"YOU","pos_PC3"]
  output[["SNP_count"]]<-QC_conclusion
  
  return(output)  
}




















