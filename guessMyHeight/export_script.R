source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){

  
  if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
    stop("Did not find a user with this id")
  }
  output<-list()
  
  
  
  
  #############
  # First get height
  #############
  
  #getting gender
  pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
  gender<-read.table(pDataFile,header=T,stringsAsFactors=F)[1,"gender"]
  
  
  #getting the current best predictor SNPs
  giant_sup_path<-"/home/ubuntu/srv/impute-me/guessMyHeight/SNPs_to_analyze.txt"
  giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
  
  
  #get genotypes and calculate gheight
  genotypes<-get_genotypes(uniqueID=uniqueID,request=giant_sup)
  gheight<-get_GRS(genotypes=genotypes,betas=giant_sup)
  
  #calculate auxilary info
  gheight_snp_count <- paste(sum(!is.na(genotypes[,"genotype"])),"of",nrow(genotypes))
  
  
  #calculating estimated_real height (note this one may need a lot of fine tuning later)
  women_mean<-1.62
  women_sd<-0.0796
  men_mean<-1.76
  men_sd<-0.0802
  if(gender == 1){
    mean_here <- men_mean
    sd_here <- men_sd
  }else{
    mean_here <- women_mean
    sd_here <- women_sd
  }
  gheight_m_estimate<-mean_here + gheight*sd_here
  
  gheight_m_estimate_cm <- gheight_m_estimate * 100
  
  output[["gheight_Z_score"]]<-gheight
  output[["gheight_snp_count"]]<-gheight_snp_count
  output[["gheight_m_estimate"]]<-gheight_m_estimate_cm
  
  
  
  
  
  
  
  
  
  #############
  # Then get colour
  #############
  
  
  
  
  #image file for hair colour  
  edge<-20
  col<-y<-x<-vector()
  for(red in seq(0,1,1/edge)){	
    for(blonde in seq(0,1,1/edge)){
      col<-c(col,hsv(h=0.1 - (red/10),s=min(c(1,1-blonde + (red/2))),v=blonde))
      x<-c(x,blonde)
      y<-c(y,red)
    }
  }
  d<-data.frame(x=x,y=y,col=col,stringsAsFactors=F)
  d[,"index"]<-d[,"x"]*edge + d[,"y"]*edge*(1+edge)
  # z<-matrix(d[,"index"],ncol=edge+1,nrow=edge+1,byrow=F)
  x<-y<-seq(0,1,1/edge)
  
  
  
  #get the gColour
  GRS_file_name<-"/home/ubuntu/srv/impute-me/hairColour/SNPs_to_analyze.txt"
  GRS_file<-read.table(GRS_file_name,sep="\t",header=T,stringsAsFactors=F)
  for(component in c("brown","red")){
    print(paste("Getting",component,"g-haircolour"))
    GRS_file_here<-GRS_file[GRS_file[,"Category"]%in%component,]
    rownames(GRS_file_here)<-GRS_file_here[,"SNP"]
    #get genotypes and calculate gHairColour
    genotypes<-get_genotypes(uniqueID=uniqueID,request=GRS_file_here)
    gHairColour<-get_GRS(genotypes=genotypes,betas=GRS_file_here)
    assign(paste("gColour",component,sep="_"),gHairColour)
  }
  
  
  #Calibrating and plotting
  brown_calibrate<-function(x){max(c(0,min(c(1, ((x-8)/50)))))}
  red_calibrate<-function(x){max(c(0.1,min(c(1,1-(x/6)))))}
  
  blondeness<-brown_calibrate(gColour_brown)
  redheadness<-red_calibrate(gColour_red)
  
  
  #Calculate colour #with the line from the image map
  colour<-hsv(h=0.1 - (redheadness/10),s=min(c(1,1-blondeness + (redheadness/2))),v=blondeness)
  
  #to double check calculate closest colour using geometric distance
  # d[,"distance"]<-sqrt((d[,"x"] - blondeness)^2 + (d[,"y"] - redheadness)^2)
  # d<-d[order(d[,"distance"]),]
  # d[1,"col"]
  #ok for me that's comparing these two
  # #674824
  # #664824
  #close enough
  
  output[["hair_colour_all_colours"]] <- d
  output[["hair_colour_your_colours"]] <- colour
  
  
  
  
  return(output)
  
}




