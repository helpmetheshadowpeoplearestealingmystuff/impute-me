source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  #set correct plink version (that's v1.90b1g being refered here first, and then plink 2.00 alpha)
  plink="/home/ubuntu/impute_dir/plink"
  plink2="/home/ubuntu/impute_dir/plink2/plink2"
  
  
  #prepare output list
  output<-list()
  output[["documentation"]] <- list()
  output[["documentation"]][["data_set_overview"]] <- "not available yet - not implemented, because first version"
  output[["documentation"]][["export_script"]] <- "https://github.com/lassefolkersen/impute-me/blob/1166a12f7fb92cd04b4e5eec1bb4e7ca1a07d8f8/prs/export_script.R"
  
  
  
  #check score files exists and are ok
  prs_dir <- "/home/ubuntu/prs_dir/"
  #the idea here is to prepare for a overview data frame with all good prs-sources we'll have
  score_sets <- data.frame(path=list.files(prs_dir,full.names=T),nicename = list.files(prs_dir,full.names=F),stringsAsFactors = F)
  
  
  score_sets <- score_sets[!file.info(score_sets[,"path"])[,"isdir"],]
  
  for(i in 1:nrow(score_sets)){
    
    testRead <-read.table(score_sets[i,"path"],nrows=10,header=T,stringsAsFactors = F)
    if(ncol(testRead)<3)stop(paste("Too few columns in score file",basename(score_sets[i,"path"])))
    if(!all(colnames(testRead)[1:2] == c("rsid","ea")))stop(paste("first two columns of a ldpred file must have rsid and ea as headers in",basename(score_sets[i,"path"])))
    for(k in 3:ncol(testRead))if(class(testRead[,k])!="numeric")stop(paste("non-numeric column: col",k,"of file",basename(score_sets[i,"path"])))
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
    #Load data using plink
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
    
    #check if score can be run at all
    cmd3 <- paste0(plink, " --score ",score_sets[1,"path"]," --bfile ", idTempFolder,f)
    out3 <- system(cmd3,ignore.stdout=T)
    if(out3 == 3){
      print(paste0("Fixing the duplicate SNP error in chr ",chr," - this can be removed once it is well established as ok in upstream pipelines, also for odd/lacking genome data"))
      cmd4 <- paste0("awk -i inplace '!seen[$1]++' ",idTempFolder,f)
      out4 <- system(cmd4)
      out1<-system(cmd1,ignore.stdout=T)
    }
  }    
  
  
  for(i in 1:nrow(score_sets)){
    score_file <- score_sets[i,"path"]
    for(chr in chromosomes){
      #set file genotyping input file f, out_file name
      f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
      out_file<-paste0(idTempFolder,"score_file",i,"_chr",chr,".txt")


      #prepare plink score run using 1.9. 
      cmd5 <- paste0(plink, " --bfile ", idTempFolder,f," --score ",score_file," 1 2 3 header sum --out ",out_file)
      out5 <- system(cmd5,ignore.stdout=T)

      #give up here if there's problems here
      if(out5 == 3){
        unlink(idTempFolder,recursive = T)
        system(cmd5,ignore.stdout=F)
        stop(paste("Found an error in the plink score step for chr",chr," - stopping and removing temp folder"))
      }
      
      #read the scores and insert
      per_chr_data<-read.table(paste0(out_file,".profile"),header=T)
      score_sets[i,paste0("PART_PLINK1_9_CNT1_",chr)] <- per_chr_data[1,"CNT"]
      score_sets[i,paste0("PART_PLINK1_9_CNT2_",chr)] <- per_chr_data[1,"CNT2"]
      score_sets[i,paste0("PART_PLINK1_9_SCORESUM_",chr)] <- per_chr_data[1,"SCORESUM"]
      
      
      
      
      #then prepare plink score run using 2.0 and read-freq
      
      #first get ethnicity and set relevant freq file
      pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
      pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
      if(class(pData)!="try-error" && "ethnicity" %in% colnames(pData)){
        ethnicity <-pData[1,"ethnicity"]
        if(is.na(ethnicity))ethnicity <-"ALL"
      }else{
        ethnicity <-"ALL"
      }
      freq_file <- paste0("/home/ubuntu/prs_dir/frequencies/2019-03-11_chr",chr,"_",ethnicity,"_freq.txt.gz")
      
      #then run the score calculation in plink with freq correction
      out_file<-paste0(idTempFolder,"score_file",i,"_chr",chr,".plink2.txt")
      cmd6 <- paste0(plink2, " --bfile ", idTempFolder,f," --read-freq ", freq_file," --score ",score_file," 1 2 3 header --out ",out_file)
      out6 <- system(cmd6,ignore.stdout=T, ignore.stderr =T)
      
      if(out6 == 0){
        #read the scores and insert
        per_chr_data_plink2 <-read.table(paste0(out_file,".sscore"),header=F)
        score_sets[i,paste0("PART_PLINK2_0_SCORESUM_",chr)] <- per_chr_data_plink2[1,"V5"]
      }else{
        print("Had zero read out for plink2")
        out6 <- system(cmd6,ignore.stdout=F, ignore.stderr =F)
        print("omitting and continuing")
        score_sets[i,paste0("PART_PLINK2_0_SCORESUM_",chr)] <- NA
      }
      
      
      
      
    }
  }    
  
  #summarize over chromosomes and get rid of part calculations
  score_sets[,"SCORESUM_PLINK_1_9"] <- colSums(t(score_sets[,grep("^PART_PLINK1_9_SCORESUM_",colnames(score_sets))]))
  score_sets[,"PLINK_1_9_CNT1"] <- colSums(t(score_sets[,grep("^PART_PLINK1_9_CNT1_",colnames(score_sets))]))
  score_sets[,"PLINK_1_9_CNT2"] <- colSums(t(score_sets[,grep("^PART_PLINK1_9_CNT2_",colnames(score_sets))]))
  
  score_sets[,"SCORESUM_PLINK_2_0"] <- colSums(t(score_sets[,grep("^PART_PLINK2_0_SCORESUM_",colnames(score_sets))]))

  for(k in grep("^PART",colnames(score_sets),value=T))score_sets[,k]<-NULL
  
  
  #reform to export/json style output
  for(i in 1:nrow(score_sets)){
    nicename <- score_sets[i,"nicename"]
    output[[nicename]]<-list()
    output[[nicename]][["SCORESUM_PLINK_1_9"]] <- score_sets[i,"SCORESUM_PLINK_1_9"]
    output[[nicename]][["SCORESUM_PLINK_2_0"]] <- score_sets[i,"SCORESUM_PLINK_2_0"]
    
    output[[nicename]][["GRS"]] <- score_sets[i,"SCORESUM_PLINK_1_9"] #for now we use this, but could also be others
    output[[nicename]][["alleles_checked"]] <- score_sets[i,"PLINK_1_9_CNT1"]
    output[[nicename]][["alleles_observed"]] <- score_sets[i,"PLINK_1_9_CNT2"]
  }

  #remove temp folder and return output
  unlink(idTempFolder,recursive = T)
  return(output)

    
}




