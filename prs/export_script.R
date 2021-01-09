library("openxlsx")
source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  
  
  #set correct plink version (that's v1.90b1g being refered here first, and then plink 2.00 alpha)
  plink="/home/ubuntu/programs/plink"
  plink2="/home/ubuntu/programs/plink2/plink2"
  
  #set paths to supporting information
  prs_dir <- "/home/ubuntu/prs_dir/"
  prs_overview_path<-"/home/ubuntu/srv/impute-me/prs/2019-03-05_study_list.xlsx"
  
  
  #Set verbosity consequences
  if(verbose > 3){
    ignore.stdout <- FALSE 
  }else{
    ignore.stdout<- TRUE
  }
  if(verbose > 2){
    ignore.stderr <- FALSE 
  }else{
    ignore.stderr<- TRUE
  }
  
  
  #set chromosomes to iterate over (good place for testing limitation)
  chromosomes <- as.character(1:22)
  
  
  #prepare output list
  output<-list()
  output[["documentation"]] <- list()
  output[["documentation"]][["data_set_overview"]] <- "https://github.com/lassefolkersen/impute-me/blob/b693ce0136809d690f702cc01c2e4bfb60833061/prs/2019-03-05_study_list.xlsx"
  output[["documentation"]][["export_script"]] <- "https://github.com/lassefolkersen/impute-me/blob/b693ce0136809d690f702cc01c2e4bfb60833061/prs/export_script.R"
  
  
  
  #check score files exists and are ok
  score_sets<-read.xlsx(prs_overview_path,rowNames=T)
  
  #remove score sets with no data
  score_sets<-score_sets[!is.na(score_sets[,"file_to_read"]),]
  if(nrow(score_sets)==0)stop("Stopping because of lack of PRS weight data")
  score_sets[,"path"] <- paste0(prs_dir,score_sets[,"file_to_read"])
  score_sets[,"nicename"] <- score_sets[,"file_to_read"] #mostly for backward compatability
  
  #run through score sets to check consistency (just takes few seconds)
  for(i in 1:nrow(score_sets)){
    if(!file.exists(score_sets[i,"path"]))stop(paste0("PRS weights files were missing, e.g. this one: ",score_sets[i,"path"]))
    if(file.info(score_sets[i,"path"])[["isdir"]])stop(paste("This PRS weights file was actually a directory:",score_sets[i,"path"]))
    testRead <-read.table(score_sets[i,"path"],nrows=10,header=T,stringsAsFactors = F)
    if(ncol(testRead)<3)stop(paste("Too few columns in score file",basename(score_sets[i,"path"])))
    if(!all(colnames(testRead)[1:2] == c("rsid","ea")))stop(paste("first two columns of a ldpred file must have rsid and ea as headers in",basename(score_sets[i,"path"])))
    for(k in 3:ncol(testRead))if(class(testRead[,k])!="numeric")stop(paste("non-numeric column: col",k,"of file",basename(score_sets[i,"path"])))
  }
  
  
  #creating a temp folder to use
  idTempFolder<-paste("/home/ubuntu/data",uniqueID,"temp/",sep="/")
  if(file.exists(idTempFolder)){
    stop(paste("Temp folder exists already, this could indicate that",uniqueID,"is already worked on."))
  }else{
    dir.create(idTempFolder)
  }
  
  # Select which of the different approaches to actually run.
  # A more complete write-up of testing is available at this DOI:
  # https://doi.org/10.13140/RG.2.2.10081.53602
  
  algorithms_to_run <- c(
    "SCORESUM_PLINK_1_9",
    "SCORESUM_PLINK_2_0",
    "SCORESUM_PLINK_2_0_DOSAGE",
    "SCORESUM_PLINK_2_0_DOSAGE_MATRIX"
  )

  algorithms_to_run <- c(
    "SCORESUM_PLINK_1_9",
    "SCORESUM_PLINK_2_0_DOSAGE_MATRIX"
  )
  
  
  
  ###################
  #prepare plink score run using 1.9. on hard-calls
  #
  # This is the most basic setup of all-SNP calculations
  # Probably the only advantage is that it is the one with the most
  # user-distribution data available because it is the oldest
  
  #Timing case
  t1 <- Sys.time()
  
  #only run if requested
  if("SCORESUM_PLINK_1_9"%in%algorithms_to_run){
    
    #check simple_format data exists (would be good to move to dosages when plink 2.0 is ready for that)
    simple_format_zip_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip")
    if(!file.exists(simple_format_zip_path))stop("Did not find simple_format_zip_path for this user")
    
    #unpack and check contents
    outZip<-unzip(simple_format_zip_path, overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
    
    merge_files <- paste0(uniqueID,"_chr",chromosomes,".simple_format.txt")
    if(!all(merge_files %in% list.files(idTempFolder)))stop("Not all expected files found")
    
    
    #then iterate over chromosomes in check-up mode and reformat to plink bed-format
    for(chr in chromosomes){
      #Load data using plink
      f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
      cmd1<-paste0(plink," --noweb --23file ",idTempFolder,f," ",uniqueID,"  ",uniqueID," --recode --make-bed --out ",idTempFolder,"/",f)
      out1<-system(cmd1,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
      
      #check if score can be run at all 
      #we would like to get rid of this check-up but it still comes up in <1% of the runs. Also the computing price is small since it doesn't scale with PRS count
      cmd3 <- paste0(plink, " --score ",score_sets[1,"path"]," --bfile ", idTempFolder,f," --out ",idTempFolder,"/",f,".temp")
      out3 <- system(cmd3,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
      if(out3 == 3){
        if(verbose>0)print(paste0(Sys.time(),": Fixing the duplicate SNP error in chr ",chr," - this can be removed once it is well established as ok in upstream pipelines, also for odd/lacking genome data"))
        cmd4 <- paste0("awk -i inplace '!seen[$1]++' ",idTempFolder,f)
        out4 <- system(cmd4)
        out1<-system(cmd1,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
      }
      
      #clean up non-used temp files while going to not spike the disk-requirements
      unlink(
        c(paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt"),
          paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt.map"),
          paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt.ped"),
          paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt.temp.nopred")))
    }    
    
    for(i in 1:nrow(score_sets)){
      score_file <- score_sets[i,"path"]
      for(chr in chromosomes){
        #set file genotyping input file f, out_file name
        f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
        out_file<-paste0(idTempFolder,"score_file",i,"_chr",chr,".txt")
        
        
        cmd5 <- paste0(plink, " --bfile ", idTempFolder,f," --score ",score_file," 1 2 3 header sum --out ",out_file)
        out5 <- system(cmd5,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
        
        #give up here if there's problems here
        if(out5 == 3){
          unlink(idTempFolder,recursive = T)
          system(cmd5,ignore.stdout=F)
          stop(paste0(Sys.time(),": Found an error in the plink score step for chr ",chr," - stopping and removing temp folder"))
        }
        
        #read the scores and insert
        per_chr_data<-read.table(paste0(out_file,".profile"),header=T)
        score_sets[i,paste0("PART_PLINK_1_9_SCORESUM_",chr)] <- per_chr_data[1,"SCORESUM"]
        score_sets[i,paste0("PART_PLINK_1_9_CNT1_",chr)] <- per_chr_data[1,"CNT"]
        score_sets[i,paste0("PART_PLINK_1_9_CNT2_",chr)] <- per_chr_data[1,"CNT2"]

        
        
        
        #clean-up as we go because even though these are small it scales with number of PRS 
        #so we really want to keep it low
        unlink(c(paste0(idTempFolder,"score_file",i,"_chr",chr,".plink2.txt.log"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".plink2.txt.sscore"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".txt.log"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".txt.nopred"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".txt.profile")))
      }
    }    
    unlink(list.files(idTempFolder,full.names=T))
    
    
    #summarize over chromosomes and get rid of part calculations
    score_sets[,"SCORESUM_PLINK_1_9"] <- colSums(t(score_sets[,paste0("PART_PLINK_1_9_SCORESUM_",chromosomes)]))
    score_sets[,"PLINK_1_9_CNT1"] <- colSums(t(score_sets[,paste0("PART_PLINK_1_9_CNT1_",chromosomes)]))
    score_sets[,"PLINK_1_9_CNT2"] <- colSums(t(score_sets[,paste0("PART_PLINK_1_9_CNT2_",chromosomes)]))
  }
  
  
  
  
  
  
  
  
  
  ###################
  #prepare plink score run using 2.0 on hard-calls along with read-freq
  #
  # Advantage over previous - the read-freq provides the population-matched
  # allele frequencies. In the case where all SNPs are well-imputed and available
  # in input, this shouldn't matter - but if they are missing, it should be better to
  # fill in population average than just 0 (which by extension is what happens with the CENTER
  # switch on and allele frequencies available)
  
  #Timing case
  t2 <- Sys.time()
  
  #only run if requested
  if("SCORESUM_PLINK_2_0"%in%algorithms_to_run){
    
    
    #check simple_format data exists (would be good to move to dosages when plink 2.0 is ready for that)
    simple_format_zip_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip")
    if(!file.exists(simple_format_zip_path))stop("Did not find simple_format_zip_path for this user")
    
    #unpack and check contents
    outZip<-unzip(simple_format_zip_path, overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
    merge_files <- paste0(uniqueID,"_chr",chromosomes,".simple_format.txt")
    if(!all(merge_files %in% list.files(idTempFolder)))stop("Not all expected files found")
    
    
    #then iterate over chromosomes in check-up mode and reformat to plink bed-format
    for(chr in chromosomes){
      #Load data using plink
      f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
      cmd1<-paste0(plink," --noweb --23file ",idTempFolder,f," ",uniqueID,"  ",uniqueID," --recode --make-bed --out ",idTempFolder,"/",f)
      out1<-system(cmd1,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
      
      #check if score can be run at all 
      #we would like to get rid of this check-up but it still comes up in <1% of the runs. Also the computing price is small since it doesn't scale with PRS count
      cmd3 <- paste0(plink, " --score ",score_sets[1,"path"]," --bfile ", idTempFolder,f," --out ",idTempFolder,"/",f,".temp")
      out3 <- system(cmd3,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
      if(out3 == 3){
        if(verbose>0)print(paste0(Sys.time(),": Fixing the duplicate SNP error in chr ",chr," - this can be removed once it is well established as ok in upstream pipelines, also for odd/lacking genome data"))
        cmd4 <- paste0("awk -i inplace '!seen[$1]++' ",idTempFolder,f)
        out4 <- system(cmd4)
        out1<-system(cmd1,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
      }
      
      #clean up non-used temp files while going to not spike the disk-requirements
      unlink(
        c(paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt"),
          paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt.map"),
          paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt.ped"),
          paste0(idTempFolder,uniqueID,"_chr",chr,".simple_format.txt.temp.nopred")))
    }    
    
    
    
    for(i in 1:nrow(score_sets)){
      score_file <- score_sets[i,"path"]
      for(chr in chromosomes){
        #set file genotyping input file f, out_file name
        f <- paste0(uniqueID,"_chr",chr,".simple_format.txt")
        out_file<-paste0(idTempFolder,"score_file",i,"_chr",chr,".txt")
        
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
        out6 <- system(cmd6,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
        
        
        if(out6 == 0){
          #read the scores and insert
          per_chr_data_plink2 <-read.table(paste0(out_file,".sscore"),header=F)
          score_sets[i,paste0("PART_PLINK_2_0_SCORESUM_",chr)] <- per_chr_data_plink2[1,"V5"]
        }else{
          if(verbose>0)print(paste0(Sys.time(),": Had zero read out for plink2. Printing error."))
          if(verbose>0)out6 <- system(cmd6,ignore.stdout=F, ignore.stderr =F)
          if(verbose>=0)print(paste0(Sys.time(),": WARNING omitting and continuing at chr ",chr))
          score_sets[i,paste0("PART_PLINK_2_0_SCORESUM_",chr)] <- NA
        }
        
        
        #clean-up as we go because even though these are small it scales with number of PRS 
        #so we really want to keep it low
        unlink(c(paste0(idTempFolder,"score_file",i,"_chr",chr,".plink2.txt.log"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".plink2.txt.sscore"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".txt.log"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".txt.nopred"),
                 paste0(idTempFolder,"score_file",i,"_chr",chr,".txt.profile")))
      }
    }    
    unlink(list.files(idTempFolder,full.names=T))
    
    #summarize over chromosomes and get rid of part calculations
    score_sets[,"SCORESUM_PLINK_2_0"] <- colSums(t(score_sets[,paste0("PART_PLINK_2_0_SCORESUM_",chromosomes)]))
    
  }
  
  
  
  
  
  ###################
  #prepare plink score run using plink 2.0 on dosage
  #
  # This functionality was not available when all-SNP scores were first implemented, 
  # but the obvious advantage over previous is that skipping a re-format check is good for speed
  # More subtly there may also be problems with indels in hard-calls, meaning this score captures more
  #
  
  #Timing case
  t3 <- Sys.time()
  
  #only run if requested
  if("SCORESUM_PLINK_2_0_DOSAGE"%in%algorithms_to_run){
    
    
    #check existence of dosage files  
    gen_format_zip_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip")
    if(!file.exists(gen_format_zip_path))stop("Did not find gen_format_zip_path for this user")
    
    #get ethnicity and gender
    pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
    if(class(pData)!="try-error" && "ethnicity" %in% colnames(pData)){
      ethnicity <-pData[1,"ethnicity"]
      if(is.na(ethnicity))ethnicity <-"ALL"
    }else{
      ethnicity <-"ALL"
    }
    gender <- pData[1,"gender"]
    
    #iterate over all chromosomes
    for(chr in chromosomes){
      #Set frequency file
      freq_file <- paste0("/home/ubuntu/prs_dir/frequencies/2019-03-11_chr",chr,"_",ethnicity,"_freq.txt.gz")
      
      #unpack and check contents
      outZip<-unzip(gen_format_zip_path, files = paste0(uniqueID,"_chr",chr,".gen"), overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
      if(outZip %in% list.files(idTempFolder))stop("Not all expected files found")
      
      #iterate over scores
      for(i in 1:nrow(score_sets)){
        score_file <- score_sets[i,"path"]
        
        #set file genotyping input file f, out_file name and freq_file
        f <- paste0(uniqueID,"_chr",chr,".gen")
        out_file<-paste0(idTempFolder,"dosage_score_file",i,"_chr",chr,".txt")
        
        #write temp sample file (contents doesn't matter much but needs to be there)
        temp_sample_path <- paste0(idTempFolder,uniqueID,"_chr",chr,".sample")
        temp_sample_file<-file(temp_sample_path,"w")
        writeLines(c("ID_1 ID_2 missing father mother sex plink_pheno","0 0 0 D D D B",paste0(uniqueID," ",uniqueID," 0 1 1 ",gender," 1")),con=temp_sample_file)
        close(temp_sample_file)
        
        #execute plink2 score/dosage command
        #Remember there is a possibility for a large computational saving by using --score-col-nums here - but we need to first get PRS in a one-file format then
        cmd7 <- paste0(plink2, " --gen ", idTempFolder,f," ref-first --sample ", temp_sample_path," --oxford-single-chr ",chr," --read-freq ", freq_file," --score ",score_file," 1 2 3 header center --out ",out_file)
        out7 <- system(cmd7,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
        
        #The double entry filter (try to get this out of the way later)
        if(out7 == 6){
          if(verbose>0)print(paste("The double entry filter was activated for chr",chr,"score",i,"in plink 2.0 dosage"))
          cmd8 <- paste0("awk -i inplace '!seen[$2]++' ",idTempFolder,f)
          out8 <- system(cmd8)
          out7 <- system(cmd7,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
        }
        
        
        
        #read the scores and insert
        if(out7 == 0){
          per_chr_data_plink2_dosage <-read.table(paste0(out_file,".sscore"),header=T,comment.char="")
          score_sets[i,paste0("PART_SCORESUM_PLINK_2_0_DOSAGE_",chr)] <- per_chr_data_plink2_dosage[1,"SCORE1_AVG"]
          score_sets[i,paste0("PART_PLINK_2_0_DOSAGE_NMISS_ALLELE_CT_",chr)] <- per_chr_data_plink2_dosage[1,"NMISS_ALLELE_CT"]
          score_sets[i,paste0("PART_PLINK_2_0_DOSAGE_NAMED_ALLELE_DOSAGE_SUM_",chr)] <- per_chr_data_plink2_dosage[1,"NAMED_ALLELE_DOSAGE_SUM"]
        }else{
          if(verbose>0)print(paste0(Sys.time(),": Had zero read out for plink2 - re-printing last command"))
          if(verbose>0)out7 <- system(cmd7,ignore.stdout=F, ignore.stderr =F)
          if(verbose>=0)print(paste0(Sys.time(),": WARNING omitting and continuing at ",chr))
          score_sets[i,paste0("PART_SCORESUM_PLINK_2_0_DOSAGE_",chr)] <- NA
          score_sets[i,paste0("PART_PLINK_2_0_DOSAGE_NMISS_ALLELE_CT_",chr)] <- NA
          score_sets[i,paste0("PART_PLINK_2_0_DOSAGE_NAMED_ALLELE_DOSAGE_SUM_",chr)] <- NA
        }
        
        
        
      }
      #clean up
      unlink(list.files(idTempFolder,full.names=T))
    }
    
    #summarize over chromosomes and get rid of part calculations
    score_sets[,"SCORESUM_PLINK_2_0_DOSAGE"] <- colSums(t(score_sets[,paste0("PART_SCORESUM_PLINK_2_0_DOSAGE_",chromosomes)]))
    score_sets[,"PLINK_2_0_DOSAGE_NMISS_ALLELE_CT"] <- colSums(t(score_sets[,paste0("PART_PLINK_2_0_DOSAGE_NMISS_ALLELE_CT_",chromosomes)]))
    score_sets[,"PLINK_2_0_DOSAGE_NAMED_ALLELE_DOSAGE_SUM"] <- colSums(t(score_sets[,paste0("PART_PLINK_2_0_DOSAGE_NAMED_ALLELE_DOSAGE_SUM_",chromosomes)]))
  }
  
  
  
  
  ###################
  #prepare plink score run using plink 2.0 on dosage in pre-merged matrix with all PRS
  #
  # Advantage here is solely speed. At the cost of a more robust/no-frills input
  # system for the PRS weights. But let's include it and see what the speed gains are
  #
  
  #Timing case
  t4 <- Sys.time()
  
  
  #only run if requested
  if("SCORESUM_PLINK_2_0_DOSAGE_MATRIX"%in%algorithms_to_run){
    
    #check existence of dosage files  
    gen_format_zip_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip")
    if(!file.exists(gen_format_zip_path))stop("Did not find gen_format_zip_path for this user")
    
    #get ethnicity and gender
    pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
    if(class(pData)!="try-error" && "ethnicity" %in% colnames(pData)){
      ethnicity <-pData[1,"ethnicity"]
      if(is.na(ethnicity))ethnicity <-"ALL"
    }else{
      ethnicity <-"ALL"
    }
    gender <- pData[1,"gender"]
    
    #No need to iterate over all scores here - just set the one file
    score_file <- "/home/ubuntu/prs_dir/2020-12-30_prs_weights.txt"
    
    #figure out how many columns in score file and check it matches with score_sets
    score_column_names<-colnames(read.table(score_file,nrows=10,sep=" ",header=T))
    score_column_names<-score_column_names[!score_column_names%in%c("rsid","ea")]
    if(!all(rownames(score_sets)%in%score_column_names)){
      missing<-rownames(score_sets)[!rownames(score_sets)%in%score_column_names]
      stop(paste("These requested score_sets rows were not found in the dosage matrix weight file:",paste(missing,collapse=",")))
    }
    indices_to_run <- 3:(length(score_column_names)+2)
    indices_to_run<-indices_to_run[score_column_names%in%rownames(score_sets)]
    names(indices_to_run)<-rownames(score_sets)

    #iterate over all chromosomes
    for(chr in chromosomes){
      
      #Set frequency file
      freq_file <- paste0("/home/ubuntu/prs_dir/frequencies/2019-03-11_chr",chr,"_",ethnicity,"_freq.txt.gz")
      
      #unpack and check contents
      outZip<-unzip(gen_format_zip_path, files = paste0(uniqueID,"_chr",chr,".gen"), overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
      if(outZip %in% list.files(idTempFolder))stop("Not all expected files found")
      
      
      #set file genotyping input file f, out_file name and freq_file
      f <- paste0(uniqueID,"_chr",chr,".gen")
      out_file<-paste0(idTempFolder,"dosage_score_chr",chr,".txt")
      
      #write temp sample file (contents doesn't matter much but needs to be there)
      temp_sample_path <- paste0(idTempFolder,uniqueID,"_chr",chr,".sample")
      temp_sample_file<-file(temp_sample_path,"w")
      writeLines(c("ID_1 ID_2 missing father mother sex plink_pheno","0 0 0 D D D B",paste0(uniqueID," ",uniqueID," 0 1 1 ",gender," 1")),con=temp_sample_file)
      close(temp_sample_file)
      
      #The double entry filter (try to get this out of the way later)
      double_entry_filter<-TRUE
      if(double_entry_filter){
        cmd_count_before <- paste0("wc -l ",idTempFolder,f)
        count_before <- as.numeric(sub(" .+$","",system(cmd_count_before,intern=T)))
        cmd_subset <- paste0("awk -i inplace '!seen[$2]++' ",idTempFolder,f)
        out_subset <- system(cmd_subset)
        cmd_count_after <- paste0("wc -l ",idTempFolder,f)
        count_after <- as.numeric(sub(" .+$","",system(cmd_count_after,intern=T)))
        if(verbose>1 & count_before-count_after>0)print(paste0(Sys.time(),": The prs-double entry filter on chr ",chr," removed ",count_before-count_after," lines from the .gen file for ",uniqueID))
      }
      
      #execute plink2 score/dosage-matrix command
      cmd7 <- paste0(plink2, " --gen ", idTempFolder,f," ref-first --sample ", temp_sample_path," --oxford-single-chr ",chr," --read-freq ", freq_file," --score ",score_file," 1 2 header center --score-col-nums ",paste(indices_to_run,collapse=",")," --out ",out_file)
      out7 <- system(cmd7,ignore.stdout=ignore.stdout, ignore.stderr = ignore.stderr)
      
      
      #read the scores and insert
      if(out7 == 0){
        sscore <-read.table(paste0(out_file,".sscore"),header=T,comment.char="")
        for(study_id in names(indices_to_run)){
          sscore_index <- which(names(indices_to_run)%in%study_id) + 7
          score_here <- sscore[1,sscore_index]
          score_sets[study_id,paste0("PART_SCORESUM_PLINK_2_0_DOSAGE_MATRIX_",chr)]<-score_here
        }
        score_sets[names(indices_to_run),paste0("PART_PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT_",chr)] <- sscore[1,"NMISS_ALLELE_CT"]
        score_sets[names(indices_to_run),paste0("PART_PLINK_2_0_DOSAGE_MATRIX_NAMED_ALLELE_DOSAGE_SUM_",chr)] <- sscore[1,"NAMED_ALLELE_DOSAGE_SUM"]
      }else{
        if(verbose>0)print(paste0(Sys.time(),": Had zero read out for plink2 - re-printing last command"))
        if(verbose>0)out7 <- system(cmd7,ignore.stdout=F, ignore.stderr =F)
        if(verbose>=0)print(paste0(Sys.time(),": WARNING omitting and continuing at chr ", chr))
        score_sets[names(indices_to_run),paste0("PART_SCORESUM_PLINK_2_0_DOSAGE_MATRIX_",chr)] <- NA
        score_sets[names(indices_to_run),paste0("PART_PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT_",chr)] <- NA
        score_sets[names(indices_to_run),paste0("PART_PLINK_2_0_DOSAGE_MATRIX_NAMED_ALLELE_DOSAGE_SUM_",chr)] <- NA
      }
      
    }
    #clean up
    unlink(list.files(idTempFolder,full.names=T))


    #summarize over chromosomes and get rid of part calculations
    score_sets[,"SCORESUM_PLINK_2_0_DOSAGE_MATRIX"] <- colSums(t(score_sets[,paste0("PART_SCORESUM_PLINK_2_0_DOSAGE_MATRIX_",chromosomes)]))
    score_sets[,"PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT"] <- colSums(t(score_sets[,paste0("PART_PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT_",chromosomes)]))
    score_sets[,"PLINK_2_0_DOSAGE_MATRIX_NAMED_ALLELE_DOSAGE_SUM"] <- colSums(t(score_sets[,paste0("PART_PLINK_2_0_DOSAGE_MATRIX_NAMED_ALLELE_DOSAGE_SUM_",chromosomes)]))
    
  }
  
  
  
  #Timing case
  t5 <- Sys.time()
  #Timing case
  output[["documentation"]][["timing"]]<-list()
  output[["documentation"]][["timing"]][["step_1"]] <- signif(as.numeric(difftime(t2,t1,units="mins")),2)
  output[["documentation"]][["timing"]][["step_2"]] <- signif(as.numeric(difftime(t3,t2,units="mins")),2)
  output[["documentation"]][["timing"]][["step_3"]] <- signif(as.numeric(difftime(t4,t3,units="mins")),2)
  output[["documentation"]][["timing"]][["step_4"]] <- signif(as.numeric(difftime(t5,t4,units="mins")),2)
  
  
  #remove the PART columns (they've been summed up already)
  for(k in grep("^PART",colnames(score_sets),value=T))score_sets[,k]<-NULL

  
  #reform to export/json style output
  for(i in 1:nrow(score_sets)){
    nicename <- score_sets[i,"nicename"]
    output[[nicename]]<-list()
    
    
    if("SCORESUM_PLINK_1_9"%in%algorithms_to_run){
      output[[nicename]][["SCORESUM_PLINK_1_9"]] <- score_sets[i,"SCORESUM_PLINK_1_9"]
      output[[nicename]][["PLINK_1_9_CNT1"]] <- score_sets[i,"PLINK_1_9_CNT1"]
      output[[nicename]][["PLINK_1_9_CNT2"]] <- score_sets[i,"PLINK_1_9_CNT2"]
      
    }
    if("SCORESUM_PLINK_2_0"%in%algorithms_to_run){
      output[[nicename]][["SCORESUM_PLINK_2_0"]] <- score_sets[i,"SCORESUM_PLINK_2_0"]
    }
    
    if("SCORESUM_PLINK_2_0_DOSAGE"%in%algorithms_to_run){
      output[[nicename]][["SCORESUM_PLINK_2_0_DOSAGE"]] <- score_sets[i,"SCORESUM_PLINK_2_0_DOSAGE"]
      output[[nicename]][["PLINK_2_0_DOSAGE_NMISS_ALLELE_CT"]] <- score_sets[i,"PLINK_2_0_DOSAGE_NMISS_ALLELE_CT"]
      output[[nicename]][["PLINK_2_0_DOSAGE_NAMED_ALLELE_DOSAGE_SUM"]] <- score_sets[i,"PLINK_2_0_DOSAGE_NAMED_ALLELE_DOSAGE_SUM"]
      
    }
    
    if("SCORESUM_PLINK_2_0_DOSAGE_MATRIX"%in%algorithms_to_run){
      output[[nicename]][["SCORESUM_PLINK_2_0_DOSAGE_MATRIX"]] <- score_sets[i,"SCORESUM_PLINK_2_0_DOSAGE_MATRIX"]
      output[[nicename]][["PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT"]] <- score_sets[i,"PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT"]
      output[[nicename]][["PLINK_2_0_DOSAGE_MATRIX_NAMED_ALLELE_DOSAGE_SUM"]] <- score_sets[i,"PLINK_2_0_DOSAGE_MATRIX_NAMED_ALLELE_DOSAGE_SUM"]
    }

    #ready-to-use scale to zero-mean and unit-variance (just not implemented yet)
    #When implementing, then make a new density overview,and consider also going for 2_0 scores at the same time
    # if(ethnicity == "ALL"){
    #   densityCurvePath<-"/home/ubuntu/srv/impute-me/prs/2019-09-17_densities_ALL.rdata"
    # }else{
    #   densityCurvePath<-paste0("/home/ubuntu/srv/impute-me/prs/2019-09-17_densities_",ethnicity,".rdata")
    # }
    # load(densityCurvePath)    
    # GRS <- output[[nicename]][["SCORESUM_PLINK_1_9"]] #we really should start using the 2_0 version
    # x <-densities[paste0(nicename,"_x"),]
    # y <- densities[paste0(nicename,"_y"),]
    # mean <- x[order(y,decreasing=T)[1]]
    # proportion <- cumsum(y[order(y,decreasing=T)]) / sum(y)
    # one_sd_range_index<-range(as.numeric(names(proportion[proportion < 0.6827])))
    # sd <- mean(abs(x[one_sd_range_index] - mean))
    # output[[nicename]][["GRS"]] <- as.numeric((GRS - mean) / sd)
    
    #The idea here is that the [[nicename]][["GRS]] will contain best-choice GRS as a Z-score (so including the above)
    #However, it's still testing with more scores, so for now we just insert the old stable one
    output[[nicename]][["GRS"]]<-output[[nicename]][["SCORESUM_PLINK_1_9"]]
    
    output[[nicename]][["alleles_checked"]] <- output[[nicename]][["PLINK_1_9_CNT1"]]
    output[[nicename]][["alleles_observed"]] <- output[[nicename]][["PLINK_1_9_CNT2"]]
  }

  #remove temp folder and return output
  unlink(idTempFolder,recursive = T)
  return(output)
  
  
}




