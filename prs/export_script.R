source("/home/ubuntu/srv/impute-me/functions.R")

plink="/home/ubuntu/impute_dir/plink"

export_function<-function(uniqueID){

  #prepare output list
  output<-list()
  output[["documentation"]] <- list()
  output[["documentation"]][["data_set_overview"]] <- "not available yet - not implemented, because first version"
  output[["documentation"]][["export_script"]] <- "https://github.com/lassefolkersen/impute-me/blob/1166a12f7fb92cd04b4e5eec1bb4e7ca1a07d8f8/prs/export_script.R"
  
  
  
  #check score files exists and are ok
  prs_dir <- "/home/ubuntu/prs_dir/"
  #the idea here is to prepare for a overview data frame with all good prs-sources we'll have
  score_sets <- data.frame(path=list.files(prs_dir,full.names=T),nicename = list.files(prs_dir,full.names=F),stringsAsFactors = F)
  
  for(i in 1:nrow(score_sets)){
    testRead <-read.table(score_sets[i,"path"],nrows=10,header=T,stringsAsFactors = F)
    if(ncol(testRead)<3)stop(paste("Too few columns in score file",score_file))
    if(!all(colnames(testRead)[1:2] == c("rsid","ea")))stop(paste("first two columns of a ldpred file must have rsid and ea as headers in",score_file))
    for(k in 3:ncol(testRead))if(class(testRead[,k])!="numeric")stop(paste("non-numeric column: col",k,"of file",score_file))
  }
  
  
  
  #check input data exists
  simple_format_zip_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip")
  if(!file.exists(simple_format_zip_path))stop("Did not find simple_format_zip_path for this user")
  
  
  #creating a temp folder to use
  idTempFolder<-paste("/home/ubuntu/data",uniqueID,"temp/",sep="/")
  if(file.exists(idTempFolder)){
    stop(paste("Temp folder exists already, this could indicate that",uniqueID,"is already worked on."))
  }else{
    dir.create(idTempFolder)
  }
  

  #unpack and check contents  
  outZip<-unzip(simple_format_zip_path, overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
  chromosomes <- as.character(1:22)
  merge_files <- paste0(uniqueID,"_chr",chromosomes,".simple_format.txt")
  if(!all(merge_files %in% list.files(idTempFolder)))stop("Not all expected files found")
  
  for(chr in chromosomes){
    #Load data using plink 1.9
    f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
    cmd1<-paste0(plink," --noweb --23file ",idTempFolder,f," ",uniqueID,"  ",uniqueID," --recode --make-bed --out ",idTempFolder,"/",f)
    out1<-system(cmd1,ignore.stdout=T)
    
    
    #this is typically because of the indel bug. Here's a fix, although shouldn't happen again
    if(out1 == 3){
      print("Fixing the commit-05b6df2 indel error - this can be removed once it is well established as ok")
      cmd2 <- paste0("sed -i '/\\o0/d' ",idTempFolder,f)
      out2<-system(cmd2,ignore.stdout=T)
      out1<-system(cmd1,ignore.stdout=T)
      if(out1 == 3)stop(paste("Error in reading",f)) #or else fail
    }
  }    
  
  
  for(i in 1:nrow(score_sets)){
    score_file <- score_sets[i,"path"]
    for(chr in chromosomes){
      f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
      out_file<-paste0(idTempFolder,"score_file",i,"_chr",chr,".txt")
      
      #prepare plink score run. Note for later - the -read-freq should probably be set up such that the calculation is in line with the main AllDisease module. For later.
      cmd3 <- paste0(plink, " --bfile ", idTempFolder,f, " --score ",score_file," 1 2 3 header sum --out ",out_file)
      out3 <- system(cmd3,ignore.stdout=F)
      
      #give up here if there's problems here
      if(out3 == 3){
        cmd4 <- paste0(plink, " --bfile ", idTempFolder,f, " --list-duplicate-vars")
        out4 <- system(cmd4)  

        unlink(idTempFolder,recursive = T)
        system(cmd3,ignore.stdout=F)
        stop(paste("Found an error in the plink score step for chr",chr," - stopping and removing temp folder"))
      }
      
      #read the scores and insert
      per_chr_data<-read.table(paste0(out_file,".profile"),header=T)
      score_sets[i,paste0("PART_CNT1_",chr)] <- per_chr_data[1,"CNT"]
      score_sets[i,paste0("PART_CNT2_",chr)] <- per_chr_data[1,"CNT2"]
      score_sets[i,paste0("PART_SCORESUM_",chr)] <- per_chr_data[1,"SCORESUM"]
      
    }
  }    
  
  #summarize over chromosomes and get rid of part calculations
  score_sets[,"SCORESUM"] <- colSums(t(score_sets[,grep("^PART_SCORESUM_",colnames(score_sets))]))
  score_sets[,"CNT1"] <- colSums(t(score_sets[,grep("^PART_CNT1_",colnames(score_sets))]))
  score_sets[,"CNT2"] <- colSums(t(score_sets[,grep("^PART_CNT2_",colnames(score_sets))]))
  for(k in grep("^PART",colnames(score_sets),value=T))score_sets[,k]<-NULL
  
  
  #reform to export/json style output
  for(i in 1:nrow(score_sets)){
    nicename <- score_sets[i,"nicename"]
    output[[nicename]]<-list()
    output[[nicename]][["SCORESUM"]] <- score_sets[i,"SCORESUM"]
    output[[nicename]][["GRS"]] <- score_sets[i,"SCORESUM"] #I'm thinking GRS for being systematic, but also could be needs some further normalization between SCORESUM and GRS
    output[[nicename]][["alleles_checked"]] <- score_sets[i,"CNT1"]
    output[[nicename]][["alleles_observed"]] <- score_sets[i,"CNT2"]
  }

  #remove temp folder and return output
  unlink(idTempFolder,recursive = T)
  return(output)

    
}




