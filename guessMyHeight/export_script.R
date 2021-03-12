source("/home/ubuntu/srv/impute-me/functions.R")
library("jsonlite")


export_function<-function(uniqueID){

  
  if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  output<-list()
  
  
  #############
  # First get height
  #############
  
  #set main choice and paths
  gheight_choice<-"height_25282103"
  
  
  #getting gender
  pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
  gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
  
  
  #get height SNPs (for Wood et al - height_25282103)
  SNPs_to_analyze_path<-"/home/ubuntu/srv/impute-me/guessMyHeight/SNPs_to_analyze.txt"
  SNPs_to_analyze<-read.table(SNPs_to_analyze_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
  SNPs_to_analyze<-SNPs_to_analyze[SNPs_to_analyze[,"category"]%in%c("height"),]
  SNPs_to_analyze[,"genotype"]<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
  height_25282103<-sum(get_GRS_2(SNPs_to_analyze,mean_scale = F, unit_variance = F)[,"personal_score"],na.rm=T)
  
  
  #decide which to use (left here for later, but always default to the top-SNP option because Chung et al is calculated in a different module.)
  gheight <- get(gheight_choice)
  
  
  #calculate auxilary info
  gheight_snp_count <- paste(sum(!is.na(SNPs_to_analyze[,"genotype"])),"of",nrow(SNPs_to_analyze))
  
  
  #calculating estimated_real height (note this one may need a lot of fine tuning later)
  all_mean<-1.69
  all_sd<-0.0799
  women_mean<-1.62
  women_sd<-0.0796
  men_mean<-1.76
  men_sd<-0.0802
  if(is.na(gender)){
    mean_here <- all_mean
    sd_here <- all_sd
  }else if(gender == 1){
    mean_here <- men_mean
    sd_here <- men_sd
  }else if(gender ==2){
    mean_here <- women_mean
    sd_here <- women_sd
  }else{
    mean_here <- all_mean
    sd_here <- all_sd
  }
  gheight_m_estimate<-mean_here + gheight*sd_here
  
  gheight_m_estimate_cm <- gheight_m_estimate * 100
  
  output[["gheight_Z_score"]]<-gheight
  output[["gheight_snp_count"]]<-gheight_snp_count
  output[["gheight_m_estimate"]]<-gheight_m_estimate_cm
  output[["gheight_choice"]]<-gheight_choice
  output[["gheight_gender_basis"]]<-gender
  
  
  
  
  
  
  
  #############
  # Then get colour
  #############
  
  
  #get the gColour
  GRS_file_name<-"/home/ubuntu/srv/impute-me/guessMyHeight/SNPs_to_analyze.txt"
  GRS_file<-read.table(GRS_file_name,sep="\t",header=T,stringsAsFactors=F)
  for(component in c("blonde","red")){
    s1<-GRS_file[GRS_file[,"category"]%in%component,]
    rownames(s1)<-s1[,"SNP"]
    #get genotypes and calculate gHairColour
    s1[,"genotype"]<-get_genotypes(uniqueID=uniqueID,request=s1)
    
    s1<-get_GRS_2(s1,mean_scale=T,unit_variance=T)
    population_sum_sd<-sqrt(sum(s1[,"population_score_sd"]^2,na.rm=T))
    GRS <-sum(s1[,"score_diff"],na.rm=T) / population_sum_sd  
    assign(paste("gColour",component,sep="_"),GRS)
  }
  
  blond_calibrate<-function(x){max(c(0,min(c(1, (x+1)/6))))}
  red_calibrate<-function(x){max(c(0,min(c(1, (x+1)/5))))}
  blondeness<-blond_calibrate(gColour_blonde)
  redheadness<-red_calibrate(gColour_red)
  
  
  #Calculate colour #with the line from the image map
  colour<-hsv(h=0.1 - (redheadness/10),s=min(c(1,1-blondeness + (redheadness/2))),v=blondeness)
  output[["hair_colour_your_colours"]] <- colour
  

  return(output)
  
}




