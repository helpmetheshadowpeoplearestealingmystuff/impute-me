source("/home/ubuntu/srv/impute-me/functions.R")

plink="/home/ubuntu/impute_dir/plink"

export_function<-function(uniqueID){
  stop("not ready yet")
  #check data exists
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
  chromosomes <- 1:22
  merge_files <- paste0(uniqueID,"_chr",chromosomes,".simple_format.txt")
  if(!all(merge_files %in% list.files(idTempFolder)))stop("Not all expected files found")
  
  for(chr in chromosomes){
    #Load data using plink 1.9
    f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
    cmd1<-paste0(plink," --noweb --23file ",idTempFolder,f," John Doe --recode --make-bed --out ",idTempFolder,"/",f)
    out1<-system(cmd1)
    
    #this is typically because of the indel bug. Here's a fix, although shouldn't happen again
    if(out1 == 3){
      print("Fixing the commit-32fa60b indel error")
      cmd2 <- paste0("sed -i '/\\o0/d' ",idTempFolder,f)
      out2<-system(cmd2)
      out1<-system(cmd1)
      if(out1 == 3)stop(paste("Error in reading",f)) #or else fail
    }
  }    
    
  #this should work, but out of memory. Maybe better to stay per-chr
  # merge_df<-data.frame(bed=paste0(idTempFolder,merge_files,".bed"),bim=paste0(idTempFolder,merge_files,".bim"),fam=paste0(idTempFolder,merge_files,".fam"))
  # merge_df<-merge_df[2:nrow(merge_df),]
  # write.table(merge_df,file=paste0(idTempFolder,"merge_list.txt"),sep="\t",quote=F,row.names=F,col.names=F)
  # cmd3 <- paste0(plink," --file ",idTempFolder,expected_files[1]," --merge-list ",idTempFolder,"merge_list.txt --recode --out m")
  # out3<-system(cmd3)
  
  score_file <- "/home/ubuntu/ldpred/DEPSYM_2016_OKBAY.EurUnrel.hapmap3.all.ldpred.effects"

  scores<-list()  
  for(chr in chromosomes){
    f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
    out_file<-paste0(idTempFolder,"score_chr",chr,".txt")
    
    
    cmd3 <- paste0(plink, " --bfile ", idTempFolder,f, " --score ",score_file," --out ",out_file)
    out3 <- system(cmd3)

    
    # #this is typically because of the duplicate-SNPs bug. Here's a fix, although shouldn't happen again because of fix 043f43
    # if(out1 == 3){
    #   print("Fixing the commit-043f43 duplicate error")
    #   
    #   cmd4 <- paste0("awk 'seen[$0]++ == 1' ",idTempFolder,f,".bim > chr",chr,"_duplicates.txt")
    #   system(cmd4)
    # 
    #   cmd5 <- paste0("awk -F '{print $2}' chr",chr,"_duplicates.txt > test.txt")
    #   system(cmd5)
    #   
    #   
    #   
    #   cmd2 <- paste0("sed -i '/\\o0/d' ",idTempFolder,f)
    #   out2<-system(cmd2)
    #   out1<-system(cmd1)
    #   if(out1 == 3)stop(paste("Error in reading",f)) #or else fail
    # }
    
    
    
        scores[[as.character(chr)]]<-read.table(paste0(out_file,".profile"),header=T)
    #MADE IT TO HERE - Fails on duplicate SNPs
    # Specifically rs145588914 in chr 2 - unique id_181X14715

  }
  
  
  
  
  
  output<-list()
  output[["MDD_PRS"]] <- colour
  return(output)
  
}




