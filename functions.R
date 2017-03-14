

# read configuration file
source("/home/ubuntu/misc_files/configuration.R")
if(!exists("maxImputations"))stop("Didn't find maxImputations")
if(!is.numeric(maxImputations))stop("maxImputations not numeric")
if(length(maxImputations)!=1)stop("maxImputations not length 1")
if(!exists("maxImputationsInQueue"))stop("Didn't find maxImputationsInQueue")
if(!is.numeric(maxImputationsInQueue))stop("maxImputationsInQueue not numeric")
if(length(maxImputationsInQueue)!=1)stop("maxImputationsInQueue not length 1")
if(!exists("serverRole"))stop("Didn't find serverRole")
if(!is.character(serverRole))stop("serverRole not character")
if(length(serverRole)!=1)stop("serverRole not length 1")
if(!serverRole%in%c("Hub","Node"))stop("serverRole not Hub or Node")
if(!exists("hubAddress"))stop("Didn't find hubAddress")
if(!is.character(hubAddress))stop("hubAddress not character")
if(length(hubAddress)!=1)stop("hubAddress not length 1")
if(!exists("email_password"))stop("Didn't find email_password ")
if(!is.character(email_password ))stop("email_password  not character")
if(length(email_password )!=1)stop("email_password  not length 1")
if(!exists("email_address"))stop("Didn't find email_address")
if(!is.character(email_address))stop("email_address not character")
if(length(email_address)!=1)stop("email_address not length 1")
if(!exists("routinely_delete_this"))stop("Didn't find routinely_delete_this")
if(!is.character(routinely_delete_this))stop("routinely_delete_this not character")



prepare_23andme_genome<-function(path, email, filename, protect_from_deletion){
  library(tools)
  
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be lengh 1, not",length(filename)))
  
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be lengh 1, not",length(protect_from_deletion)))
  
  if(class(email)!="character")stop(paste("email must be character, not",class(email)))
  if(length(email)!=1)stop(paste("email must be lengh 1, not",length(email)))
  if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
    stop(paste("a real email adress is needed:",email))
  }
  
  acceptedMails<-read.table("/home/ubuntu/misc_files/accepted_emails.txt",stringsAsFactors=F)[,1]
  if(!email%in%acceptedMails & FALSE){ #changed to always accept submission for now
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"not_accepted_email",email,path)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
    stop("At the current stage, the project is only open to backers. Please visit our kickstarter page at: http://kck.st/1VlrTlf - sorry for the inconvenience. Going forward the plan is to run on a more voluntary pricing basis, always as non-profit (see terms-of-use). No data was saved.")
  }
  
  
  
  #check for too many ongoing imputations
  print("check for too many ongoing imputations")
  s<-list.files("/home/ubuntu/imputations/")
  if(length(grep("^imputation_folder",s)) >= maxImputationsInQueue){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_many_jobs",email,length(grep("^imputation_folder",s)))
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
    
    stop(paste("More than",maxImputationsInQueue,"imputations are already in progress. Cannot start a new one. Limited server capacity was the reason for our kickstarter campaign. Supporters were first in line: kck.st/1VlrTlf"))
  }
  
  
  
  
  
  
  #Create uniqueID 
  print("create uniqueID")
  setwd("/home/ubuntu/imputations/")
  uniqueID <- paste("id_",sample(1000:9000,1),sample(10000:90000,1),sep="")
  numberOfLetters<-sample(c(1,1,2,3),1)
  if(numberOfLetters>0){
    positionsToInsertLetter<-sample(5:(nchar(uniqueID)-1),numberOfLetters)
    
    l<-c(LETTERS,letters)
    l<-l[!l%in%c("o","O")] #I hate it when O is in
    for(x in positionsToInsertLetter){
      substr(uniqueID,x,x)<-sample(l,1)
    }
  }
  
  
  #create imputation folder and output data folder
  print("create imputation folder and output data folder")
  if(uniqueID%in%list.files("/home/ubuntu/data/")){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
    stop("Problem with unique ID generation. Please re-load and try again.")
  }
  
  homeFolderShort<-paste("imputation_folder",uniqueID,sep="_")
  dir.create(homeFolderShort)
  setwd(homeFolderShort)
  homeFolder<-paste("/home/ubuntu/imputations/",homeFolderShort,"/",sep="")
  write.table("Job is not ready yet",file="job_status.txt",col.names=F,row.names=F,quote=F)
  # 	
  
  #unzipping (or not) and moving to new place
  print("#unzipping (or not) and moving to new place")
  newTempPath <- paste(homeFolder,paste(uniqueID,"_raw_data",sep=""),sep="/")
  newUnzippedPath <- paste(homeFolder,paste(uniqueID,"_raw_data.txt",sep=""),sep="/")
  file.copy(path, newTempPath)	
  gunzipResults<-unzip(newTempPath,exdir=homeFolder)
  if(length(gunzipResults)==1){ #then its a zip file
    file.rename(gunzipResults, newUnzippedPath)		
  }else{ #then it's probably not
    #check if it is a gz file
    filetype<-system(paste("file ", newTempPath),intern=T)
    if(length(grep("gzip compressed",filetype))==1){
      unlink(homeFolder,recursive=T)
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"gzip_file",email,uniqueID)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
      stop("Don't submit gz-files. Only uncompressed text or zip-files. If you already know what a gz file is, this should be easy for you. Please format as tab separated text files.")
    }else{
      #otherwise just rename
      file.rename(newTempPath, newUnzippedPath)		
    }
  }
  
  
  path <- newUnzippedPath
  
  #checking if it is a consistent file
  print("checking if it is a consistent file")
  testRead<-try(read.table(path,nrow=10,stringsAsFactors=F))
  if(class(testRead)=="try-error"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"general_data_file_problem",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)	
    unlink(homeFolder,recursive=T)
    stop("Your file didn't seem like genomic data at all. It must contain many rows, one per SNP, with information about your genotype. Please write an email if you think this is a mistake and that this file format should be supported.")
    
    
  }
  
  if(ncol(testRead)==5){
    #This could be an ancestry.com file. Check that first
    testRead2<-read.table(path,nrow=10,stringsAsFactors=F,header=T)
    if(unique(sub("[0-9]+$","",testRead2[,1]))!="rs"){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"ancestry_problem",email,uniqueID)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)	
      unlink(homeFolder,recursive=T)
      stop("Your file seemed like ancestry.com data, but didn't have rs IDs in column 1")
    }
    #ok, this is probably an ancestry.com file. Let's reformat.
    format_ancestry_com_as_23andme(path)
    
  }
  if(ncol(testRead)==1){
    #this could be myheritage. Let's try with that
    format_myheritage_as_23andme(path)
  }
  
  
  testRead2<-read.table(path,nrow=10,stringsAsFactors=F)
  if(ncol(testRead2)!=4){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_4_columns",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
    unlink(homeFolder,recursive=T)
    stop("Your file didn't have 4 columns (or 5 for ancestry.com data). If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection.")
  }
  if(unique(sub("[0-9]+$","",testRead2[,1]))!="rs"){
    unlink(paste("/home/ubuntu/data/",uniqueID,sep=""),recursive=T)
    unlink(homeFolder,recursive=T)
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_no_rs_id",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
    unlink(homeFolder,recursive=T)
    stop("Your file didn't have rs IDs in column 1. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection.")
  }
  
  
  
  #checking if this job has not actually been run before
  print("checking if this job has not actually been run before")
  this_person_md5sum <- md5sum(path)
  otherPersons<-list.files("/home/ubuntu/data",full.names=T)
  for(otherPerson in otherPersons){
    if(!file.info(otherPerson)[["isdir"]])next
    if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
    other_person_md5sum<-try(read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F)[1,"md5sum"],silent=T)
    if(class(other_person_md5sum)=="try-error")next
    if(is.null(other_person_md5sum))next
    if(this_person_md5sum == other_person_md5sum){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"md5sum_match",email,this_person_md5sum)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
      unlink(homeFolder,recursive=T)
      stop("A person with this genome was already analyzed by the system. Write an email if you wish to clear this flag.")
    }
  }
  
  
  print("Finalize")
  save(uniqueID,email,filename,protect_from_deletion,file=paste(homeFolder,"variables.rdata",sep=""))
  unlink("job_status.txt")
  write.table("Job is ready",file="job_status.txt",col.names=F,row.names=F,quote=F)
  
  
  return(paste("Genome files succesfully uploaded. During the next week you will receive an email to",email,"with download-instructions (look in your spam filter if not). You can close this browser window."))
  
  
}




run_imputation<-function(
  rawdata, 
  runDir, 
  shapeit="/home/ubuntu/impute_dir/bin/shapeit",
  plink="/home/ubuntu/impute_dir/plink",
  impute2="/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2",
  sample_ref="/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
){
  
  
  
  if(class(rawdata)!="character")stop(paste("rawdata must be character, not",class(rawdata)))
  if(length(rawdata)!=1)stop(paste("rawdata must be lengh 1, not",length(rawdata)))
  if(!file.exists(rawdata))stop(paste("Did not find rawdata at path:",rawdata))
  
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  
  if(class(shapeit)!="character")stop(paste("shapeit must be character, not",class(shapeit)))
  if(length(shapeit)!=1)stop(paste("shapeit must be lengh 1, not",length(shapeit)))
  if(!file.exists(shapeit))stop(paste("Did not find shapeit at path:",shapeit))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be lengh 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(class(impute2)!="character")stop(paste("impute2 must be character, not",class(impute2)))
  if(length(impute2)!=1)stop(paste("impute2 must be lengh 1, not",length(impute2)))
  if(!file.exists(impute2))stop(paste("Did not find impute2 at path:",impute2))
  
  if(class(sample_ref)!="character")stop(paste("sample_ref must be character, not",class(sample_ref)))
  if(length(sample_ref)!=1)stop(paste("sample_ref must be lengh 1, not",length(sample_ref)))
  if(!file.exists(sample_ref))stop(paste("Did not find sample_ref at path:",sample_ref))
  
  
  #Load data using plink 1.9
  cmd1<-paste(plink,"--noweb --23file",rawdata,"John Doe --recode --out step_1")
  out1<-system(cmd1)
  
  
  #check for MT presence and other non-sorted behaviour (note this is an exception. Best to submit sorted data)
  if(out1 == 3){
    print("First trying to just remove the mitochondrial SNPS (they are not needed)")
    cmd1_2<-paste(plink,"--noweb --23file",rawdata,"John Doe --recode --out step_1")
    out1_2<-system(cmd1_2,intern=T)
    if(length(grep("are out of order",out1_2))>0){
      cmd1_3<-paste("sed -i.bak '/\\tMT\\t/d'",rawdata)
      system(cmd1_3)
      out1<-system(cmd1)
      if(out1 == 3){
        print("Still problems with sort. Try a bash-based sort.")
        cmd1_4<-paste("sort -k2 -k3 -g -o",rawdata,rawdata)
        system(cmd1_4)
        cmd1_5<-paste("sed -i.bak '/\\tY\\t/d'",rawdata)
        system(cmd1_5)
        # cmd1_6<-paste("sed -i.bak '/\\tX\\t/d'",rawdata)
        # system(cmd1_6)
        out1<-system(cmd1)
        if(out1 == 3){
          stop("Something odd with the MT presence reverter I")
        }
      }
    }else{
      stop("Something odd with the MT presence reverter II")
    }
  }
  
  
  #Rscript to omit duplicates
  map<-read.table('step_1.map',sep='\t',stringsAsFactors=F)
  exclude<-map[duplicated(map[,4]),2]
  print(paste('Removed',length(exclude),'SNPs that were duplicated'))
  write.table(exclude,file='step_2_exclusions',sep='\t',row.names=FALSE,col.names=F,quote=F)
  
  
  
  #loop over chromosomes
  for(chr in c("X",as.character(1:22))){
    
    #First in loop - extract only one specific chromosome
    cmd2<-paste(plink," --file step_1 --chr ",chr," --recode --out step_2_chr",chr," --exclude step_2_exclusions",sep="")
    out2<-system(cmd2)
    
    #if X chromosome is missing it is allowed to skip forward
    if(out2 == 13 & chr == "X"){
      print("Didn't find X-chr data, so skipping that")
      next
    }
    
    #Then check for strand flips etc. 
    cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
    system(cmd3)
    
    
    #Many homozygote SNPs will fail the check, because, well - of course, they don't have the ref-allele. So we make more detailed R script for sorting them
    logFile<-read.table(paste("step_2_chr",chr,"_shapeit_log.snp.strand",sep=""),sep='\t',stringsAsFactors=FALSE,header=F,skip=1)
    omitMissing<-logFile[logFile[,1] %in% 'Missing',3]
    logStrand<-logFile[logFile[,1] %in% 'Strand',]
    omitNonIdentical<-logStrand[logStrand[,5] != logStrand[,6],3]
    omitBlank<-logStrand[logStrand[,5]%in%'',3]
    
    #These are super-annoying. We have to create another (fake) person with the alternative allele just for their sake. This next command takes all the homozygotes, minus the indels (which are too complicated to lift out from 23andme)
    forceHomozygoteTable<-logStrand[
      logStrand[,5] == logStrand[,6] & 
        nchar(logStrand[,9])==1 & 
        nchar(logStrand[,10])==1 &
        !logStrand[,5] %in% c("D","I") &
        !logStrand[,6] %in% c("D","I") 
      ,]
    
    #This removes any cases where there are more than to alleles involved
    forceHomozygoteTable<-forceHomozygoteTable[sapply(apply(forceHomozygoteTable[,c(5,6,9,10)],1,unique),length)==2,]
    
    #This removes any duplicates there might be
    forceHomozygoteTable<-forceHomozygoteTable[!duplicated(forceHomozygoteTable[,4]),]
    map<-read.table(paste("step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F)
    #This loads the ped file, and doubles it
    ped2<-ped1<-strsplit(readLines(paste("step_2_chr",chr,".ped",sep=""))," ")[[1]]
    ped2[1]<-"Temporary"
    ped2[2]<-"Non_person"
    if((length(ped1)-6) / 2 !=nrow(map))stop("mismatch between map and ped")
    replacementPos<-which(map[,2]%in%forceHomozygoteTable[,4])
    A1_pos<-7+2*(replacementPos-1)
    A2_pos<-8+2*(replacementPos-1)
    ped2[A1_pos]<-forceHomozygoteTable[,9]
    ped2[A2_pos]<-forceHomozygoteTable[,10]
    ped<-rbind(ped1,ped2)
    write.table(ped,paste("step_3_chr",chr,".ped",sep=""),sep=" ",col.names=F,row.names=F,quote=F)
    omitRemaining<-logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
    print(paste('Omitting',length(omitMissing),'because of missing',length(omitBlank),'because they are blank, and',length(omitNonIdentical),'true strand flips'))
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    
    #running the shapeit command (with two people, the right one and a placeholder heterozygote
    cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
    system(cmd4)
    
    
    #checking for errors and stopping if there are any. No point to continue otherwise
    log<-readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
    if(substr(log[length(log)],1,5)=="ERROR"){
      m<-paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep="")
      write.table(m,file="master_imputation_log.txt",col.names=F,row.names=F,quote=F)
      stop(m)
    }
    
    #removing the placeholder person again
    cmd5_1<-paste("cut --delimiter=' ' -f 1-7 step_4_chr",chr,".haps > step_5_chr",chr,".haps",sep="")
    system(cmd5_1)
    cmd5_2<-paste("head -n 3 step_4_chr",chr,".sample > step_5_chr",chr,".sample",sep="")
    system(cmd5_2)
    
    
    
    #detect max length of each chromosome
    cmd6<-paste("zcat /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2",sep="")
    maxPos<-as.numeric(system(cmd6,intern=T))
    
    
    #iterate over 5e6 chunks
    starts<-seq(0,maxPos,5e6)
    for(i in 1:length(starts)){
      start <- starts[i]
      end <- start+5e6
      
      
      cmd7<-paste(impute2," -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")
      step_7_log<-system(cmd7)
      
      
      #test for memory-lack bug (step_7_log will be 137 if killed, otherwise 0)
      if(step_7_log == 137){
        #we divide the job in smaller bits 
        divisions<-3
        for(j in 1:divisions){
          start_2 <- floor(starts[i] + (j-1)*(5e6/ divisions))
          end_2 <- floor(starts[i]+ (j)*(5e6/ divisions))
          print(paste("restart imputation with new subset to avoid memory-lack bug:",start_2,"to",end_2)   )
          
          cmd7<-paste(impute2," -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o step_7_chr",chr,"_",i,"-",j,sep="")
          step_7_log_2<-system(cmd7)
          if(step_7_log_2 == 137){print("the memory problem was still active after second round")
          }
        }
      }
    }
  }
}



summarize_imputation<-function(
  runDir,
  uniqueID,
  destinationDir,
  gtools="/home/ubuntu/impute_dir/gtool",
  plink="/home/ubuntu/impute_dir/plink-1.07-x86_64/plink" #note, as of 2015-08-31 this must be plink 1.07, otherwise we get a bug
){
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  setwd(runDir)
  
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
  
  if(class(destinationDir)!="character")stop(paste("destinationDir must be character, not",class(destinationDir)))
  if(length(destinationDir)!=1)stop(paste("destinationDir must be lengh 1, not",length(destinationDir)))
  if(!file.exists(destinationDir))stop(paste("Did not find destinationDir at path:",destinationDir))
  if(length(grep("/$",destinationDir))!=0)stop("Please don't use a trailing slash in the destinationDir")
  
  if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
  if(length(gtools)!=1)stop(paste("gtools must be lengh 1, not",length(gtools)))
  if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be lengh 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(file.exists(paste(destinationDir,"/",uniqueID)))stop("The destinationDir already exists. This is a major unforeseen error")
  
  allFiles1<-list.files(runDir)
  step7Files<-grep("^step_7_chr",allFiles1,value=T)
  step7ResultsFiles<-grep("[0-9]$",step7Files,value=T)
  chromosomes<-unique(sub("_[0-9-]+$","",sub("^step_7_chr","",step7ResultsFiles)))
  
  for(chr in chromosomes){
    print(paste("Merging chunks in chromosome",chr))
    s <-grep(paste("^step_7_chr",chr,"_",sep=""), step7ResultsFiles,value=T)
    s<-s[order(as.numeric(sub("-[0-9]","",sub("^.+_","",s))),as.numeric(substr(sub("^.+_","",s),3,3)))]
    print(paste("For chr",chr,"these were the files to merge:",paste(s,collapse=", ")))
    cmd1<-paste("cat ",paste(s,collapse=" ")," > ",uniqueID,"_chr",chr,".gen",sep="")
    system(cmd1)
  }	
  
  
  
  genFiles<-paste(uniqueID,"_chr",chromosomes,".gen",sep="")
  
  
  #running a conversion first to plink then to 23andme	
  for(genFile in genFiles){
    
    chr <- sub("\\.gen$","",sub("^.+_chr","",genFile))
    print(paste("Simplifying in chromosome",chr))
    sampleFile<-paste("step_4_chr",chr,".sample",sep="")
    
    #make list of non-indels
    cmd2<-paste("awk -F' ' '{ if ((length($4) > 1 ) || (length($5) > 1 )) print $2 }'",genFile,">",paste("step_8_chr",chr,"_snps_to_exclude",sep=""))
    system(cmd2)
    
    #exclude indels
    cmd3 <- paste(gtools," -S --g ",genFile," --s ",sampleFile," --exclusion step_8_chr",chr,"_snps_to_exclude --og step_8_chr",chr,".gen",sep="")
    system(cmd3)
    
    #Convert to ped format
    cmd4 <- paste(gtools," -G --g step_8_chr",chr,".gen --s ",sampleFile," --chr ",chr," --snp",sep="")
    system(cmd4)
    
    
    #reform to plink fam/bim/bed file			
    cmd5 <- paste(plink," --file step_8_chr",chr,".gen --recode --transpose --noweb --out step_9_chr",chr,sep="")
    system(cmd5)
    
    #re-order to 23andme format
    cmd6<-paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' step_9_chr",chr,".tped  > step_10_chr",chr,".txt",sep="")
    system(cmd6)
    
    
    #The step 8 and also 9 sometime fails for no apparent reason. Probably memory. We therefore make a checkup, where
    #it is checked if the file actually exists and if not - a more complicated step splits it up in chunks.
    #It's not looking nice, but at least the split-up only needs to run in very low memory settings
    fileExists<-file.exists(paste("step_10_chr",chr,".txt",sep=""))
    if(fileExists){
      size<-file.info(paste("step_10_chr",chr,".txt",sep=""))["size"]
    }else{
      size<-0	
    }
    
    #	arbitraly re-run if it's less than 100 bytes (fair to assume something was wrong then)
    if(size<100 ){
      print(paste("retrying step 8-9 command for chr",chr,". Trying to split it in pieces (non-normal low memory running)"))
      cmd7 <- paste("split --verbose --lines 5000000 step_8_chr",chr,".gen step_8_extra_chr",chr,".gen",sep="")
      system(cmd7)
      chunks<-grep(paste("step_8_extra_chr",chr,"\\.gena[a-z]$",sep=""),list.files(runDir),value=T)
      for(chunk in chunks){
        ch<-sub("^.+\\.","",chunk)
        cmd8 <- paste(gtools," -G --g ",chunk," --s ",sampleFile," --chr ",chr," --snp",sep="")
        system(cmd8)
        #reform to plink fam/bim/bed file			
        cmd9 <- paste(plink," --file ",chunk," --recode --transpose --noweb --out step_9_chr",chr,"_",ch,sep="")
        system(cmd9)
        #re-order to 23andme format
        cmd10<-paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' step_9_chr",chr,"_",ch,".tped  > step_9_chr",chr,"_split_",ch,".txt",sep="")
        system(cmd10)
      }
      cmd11<-paste("cat ",paste(paste("step_9_chr",chr,"_split_",sub("^.+\\.","",chunks),".txt",sep=""),collapse=" ")," > step_10_chr",chr,".txt",sep="")
      system(cmd11)			
    }
    
    #remove NN
    cmd12 <- paste("awk '{ if($4 != \"NN\") print}' step_10_chr",chr,".txt  >", sub("\\.gen$","",genFile),".23andme.txt",sep="")
    system(cmd12)
    
  }
  
  
  
  #preparing destinationDir
  prepDestinationDir<-paste(destinationDir,"/",uniqueID,sep="")
  if(!file.exists(prepDestinationDir))dir.create(prepDestinationDir)
  
  #zipping and moving 23andme files
  zipFile23andme<-paste(runDir,paste(uniqueID,".23andme.zip",sep=""),sep="/")
  twentythreeandmeFiles<-paste(uniqueID,"_chr",chromosomes,".23andme.txt",sep="")
  zip(zipFile23andme, twentythreeandmeFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFile23andme, paste(prepDestinationDir,basename(zipFile23andme),sep="/"))
  
  
  #zipping gen files
  zipFileGen<-paste(runDir,paste(uniqueID,".gen.zip",sep=""),sep="/")
  zip(zipFileGen, genFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFileGen, paste(prepDestinationDir,basename(zipFileGen),sep="/"))
  
  
  #move the original file as well
  zipFileOriginal<-paste(runDir,paste(uniqueID,".input_data.zip",sep=""),sep="/")
  zip(zipFileOriginal, paste(uniqueID,"_raw_data.txt",sep=""), flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFileOriginal, paste(prepDestinationDir,basename(zipFileOriginal),sep="/"))
  
  
  #return paths
  returnPaths<-c(
    paste(prepDestinationDir,basename(zipFile23andme),sep="/"),
    paste(prepDestinationDir,basename(zipFileGen),sep="/")
  )
  names(returnPaths)<-c("23andme","gen")
  
  return(returnPaths)
}









get_genotypes<-function(
  uniqueID,
  request,
  gtools="/home/ubuntu/impute_dir/gtool",
  namingLabel="cached" #should default to cached, but it's a way of separately saving larger cached sets in a different file
){
  
  
  #checking
  if(class(namingLabel)!="character")stop(paste("namingLabel must be character, not",class(namingLabel)))
  if(length(namingLabel)!=1)stop(paste("namingLabel must be lengh 1, not",length(namingLabel)))
  
  
  #checking data in uniqueID's home folder
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
  idFolder<-paste("/home/ubuntu/data",uniqueID,sep="/")
  if(!file.exists(idFolder))stop(paste("Did not find an idFolder at",idFolder))
  genZipFile<-paste(idFolder,"/",uniqueID,".gen.zip",sep="")
  inputZipFile<-paste(idFolder,"/",uniqueID,".input_data.zip",sep="")
  cachedGenotypeFile<-paste(idFolder,"/",uniqueID,".",namingLabel,".gz",sep="")
  if(!file.exists(cachedGenotypeFile))print(paste("Did not find a",namingLabel, "chachedGenotypeFile file in idFolder at",idFolder,"but that's no problem"))
  
  #creating a temp folder to use
  idTempFolder<-paste("/home/ubuntu/data",uniqueID,"temp",sep="/")
  if(file.exists(idTempFolder))stop(paste("Temp folder exists, this could indicate that",uniqueID,"is already worked on. Wait a little, or write administrators if you think this is a mistake"))
  
  
  #checking other variables
  if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
  if(length(gtools)!=1)stop(paste("gtools must be lengh 1, not",length(gtools)))
  if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
  
  if(class(request)!="data.frame")stop(paste("request must be data.frame, not",class(request)))
  if(!"chr_name"%in%colnames(request))stop("request object must have a column 'chr_name'")
  if("already_exists"%in%colnames(request))print("request object had a column 'already_exists', this will be overwritten")
  if(!any(substr(rownames(request),1,2)%in%"rs")){
    if(!any(substr(rownames(request),1,1)%in%"i")){
      stop("Not a single rs id was found among the rownames of the request. Really?")
    }
  }
  
  
  #checking existence of already cached genotypes
  if(file.exists(cachedGenotypeFile)){
    cachedGenotypes<-read.table(cachedGenotypeFile,header=T,stringsAsFactors=F,row.names=1)
    snpsAlreadyCached<-rownames(cachedGenotypes)
    requestDeNovo<-request[!rownames(request)%in%snpsAlreadyCached,,drop=F]
  }else{
    requestDeNovo<-request
  }
  
  
  #If there are anything novel, extract it from zip (takes a long time)
  if(nrow(requestDeNovo)>0){
    
    #only here need to check that raw data is there
    if(!file.exists(genZipFile) | !file.exists(inputZipFile)){
      stop(paste("Did not find and .input_data or a .gen file in idFolder. This probably means that raw data has been deleted and no furter SNP-data can be retrieved."))
    }
    
    
    dir.create(idTempFolder)
    chromosomes<-unique(requestDeNovo[,"chr_name"])
    contents<-unzip(genZipFile,list=T)
    
    #if input is in as a chromosome, use the 23andmefile as input
    if("input"%in%chromosomes){
      snpsFromInput<-requestDeNovo[requestDeNovo[,"chr_name"]%in%"input","SNP"]
      outZip<-unzip(inputZipFile, overwrite = TRUE,exdir = idTempFolder, unzip = "internal",)
      cmd0 <- paste("grep -E '",paste(paste(snpsFromInput,"\t",sep=""),collapse="|"),"' ",outZip,sep="")
      input_genotypes<-system(cmd0,intern=T)
      input_genotypes<-do.call(rbind,strsplit(input_genotypes,"\t"))
      input_genotypes[,4]<-sub("\r$","",input_genotypes[,4])
      
      
      male_x_chr <- which(input_genotypes[,2]%in%"X" & nchar(input_genotypes[,4])==1)
      if(length(male_x_chr) > 0){
        input_genotypes[male_x_chr,4] <- paste(input_genotypes[male_x_chr,4]," ",sep="")	
      }
      
      
      
      if(any(nchar(input_genotypes[,4])!=2))stop("input data must have length 2 genotypes")
      
      input_genotypes[,4]<-paste(substr(input_genotypes[,4],1,1),substr(input_genotypes[,4],2,2),sep="/")
      genotypes<-data.frame(row.names=input_genotypes[,1],genotype=input_genotypes[,4],stringsAsFactors=F)
    }else{
      genotypes<-data.frame(genotype=vector(),stringsAsFactors=F)
    }
    
    #if any normal style chromosome names are in use the gen files
    if(any(c(as.character(1:22),"X")%in%chromosomes)){
      chromosomes<-chromosomes[chromosomes%in%c(as.character(1:22),"X")]
      gensToExtract<-paste(uniqueID,"_chr",chromosomes,".gen",sep="")
      if(!all(gensToExtract%in%contents[,"Name"])){
        missing<-gensToExtract[!gensToExtract%in%contents[,"Name"]]
        gensToExtract<-gensToExtract[!gensToExtract%in%missing]
        chromosomes<-chromosomes[!chromosomes%in%sub("\\.gen$","",sub("^.+_chr","",missing))]
        warning(paste("These were missing in the zip-gen file:",paste(missing,collapse=", ")))
      }
      outZip<-unzip(genZipFile, files = gensToExtract, overwrite = TRUE,exdir = idTempFolder, unzip = "internal",)
      
      f<-file(paste(idTempFolder,"/samples.txt",sep=""),"w")
      writeLines("ID_1 ID_2 missing sex",f)
      writeLines("0 0 0 D",f)
      writeLines("John Doe 0.0 2 ",f)#gender probably doesn't matter here
      close(f)
      
      
      #looping over all chromosomes and extracting the relevant genotypes in each using gtools
      for(chr in chromosomes){
        
        #This is wrapped in a try block, because it has previously failed from unpredictble memory issues, so it's better to give a few tries
        for(tryCount in 1:5){
          print(paste("Getting ped and map file at chr",chr," - try",tryCount))
          gen<-paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen",sep="")
          snpsHere<-rownames(requestDeNovo)[requestDeNovo[,"chr_name"]%in%chr]
          write.table(snpsHere,file=paste(idTempFolder,"/snps_in_chr",chr,".txt",sep=""),quote=F,row.names=F,col.names=F)
          cmd1<-paste(gtools," -S --g " , gen, " --s ",idTempFolder,"/samples.txt --inclusion ",idTempFolder,"/snps_in_chr",chr,".txt",sep="")
          system(cmd1)
          subsetFile<-paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset",sep="")
          if(!file.exists(subsetFile)){
            print(paste("Did not find any of the SNPs on chr",chr))	
            next
          }
          cmd2<-paste(gtools," -G --g " ,subsetFile," --s ",idTempFolder,"/samples.txt --snp --threshold 0.7",sep="")
          system(cmd2)
          
          
          ped<-try(strsplit(readLines(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.ped",sep="")),"\t")[[1]],silent=T)
          map<-try(read.table(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.map",sep=""),stringsAsFactors=FALSE),silent=T)
          
          if(class(ped)!="try-error" & class(map)!="try-error"){
            ped<-ped[7:length(ped)]
            genotypes_here<-try(data.frame(row.names=map[,2],genotype=sub(" ","/",ped),stringsAsFactors=F))
            #error where there's duplicate row names
            if(class(genotypes_here)=="try-error"){
              print("Found a duplicate row names error")
              include_these<-which(!duplicated(map[,2]))
              genotypes_here<-try(data.frame(row.names=map[include_these,2],genotype=sub(" ","/",ped[include_these]),stringsAsFactors=F))
            }						
            break
          }else{
            genotypes_here<-data.frame(row.names=vector(),genotype=vector(),stringsAsFactors=F)
          }
        }
        genotypes<-rbind(genotypes,genotypes_here)
      }
    }
    
    
    genotypes[genotypes[,"genotype"]%in%"N/N","genotype"]<-NA
    stillMissing<-rownames(requestDeNovo)[!rownames(requestDeNovo)%in%rownames(genotypes)]
    genotypes<-rbind(genotypes,data.frame(row.names=stillMissing,genotype=rep(NA,length(stillMissing),stringsAsFactors=F)))
    
    #removing temporary folder
    unlink(idTempFolder,recursive=T)
  }
  
  #merge with cachedGenotypes
  if(nrow(requestDeNovo)>0){
    if(file.exists(cachedGenotypeFile)){
      genotypes<-rbind(cachedGenotypes,genotypes)
      unlink(cachedGenotypeFile)
    }
    f<-gzfile(cachedGenotypeFile,"w")
    write.table(genotypes,file=f,sep="\t",col.names=NA)
    close(f)
  }else{
    genotypes<-cachedGenotypes
  }
  
  
  
  
  return(genotypes[rownames(request),,drop=FALSE])
  
}
















get_GRS<-function(genotypes, betas){
  
  if(class(genotypes)!="data.frame")stop(paste("genotypes must be data.frame, not",class(genotypes)))
  if(!"genotype"%in%colnames(genotypes))stop(paste("genotypes must have a column genotype"))
  if(!all(unique(sub("[0-9].+$","",rownames(genotypes)))%in%c("i","rs"))){
    
    stop(paste("genotypes must have rownames starting with rs. You had these:",paste(unique(sub("[0-9].+$","",rownames(genotypes))),collapse=", ")))
    
  }
  
  if(class(betas)!="data.frame")stop(paste("genotypes must be data.frame, not",class(betas)))
  necessary_columns<-c("effect_allele","non_effect_allele","Beta")
  if(!all(necessary_columns%in%colnames(betas)))stop(paste("betas must have a column",paste(necessary_columns,collapse=", ")))
  if(!all(unique(sub("[0-9].+$","",rownames(betas)))%in%c("i","rs")))stop("betas must have rownames starting with rs")
  
  
  # if(!all(rownames(genotypes)%in%rownames(betas)))stop("all SNPs in genotypes must be present in betas")
  if(!all(rownames(betas)%in%rownames(genotypes)))stop("all SNPs in betas must be present in genotypes")
  
  
  
  geneticRiskScore<-0
  for(snp in rownames(betas)){
    if(is.na(genotypes[snp,"genotype"])){
      warning(paste("Note, for",snp,"we found missing genotypes. This can cause errors particularly if the data is not mean centered."))
      next
    }
    
    genotype<-strsplit(genotypes[snp,],"/")[[1]]
    effect_allele<-betas[snp,"effect_allele"]
    non_effect_allele<-betas[snp,"non_effect_allele"]
    
    if(!all(genotype%in%c(effect_allele,non_effect_allele))){
      print(paste("Note, for",snp,"we found wrong alleles:",paste(genotype,collapse=""),"and should find",effect_allele,"or",non_effect_allele))
      next
    }
    
    beta<-betas[snp,"Beta"]	
    geneticRiskScore <- geneticRiskScore + sum(genotype%in%effect_allele) * beta
  }
  return(geneticRiskScore)
  
}








crawl_for_snps_to_analyze<-function(uniqueIDs=NULL){
  #A function that will crawl all data directories to extract all currently worked on SNPs
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files("/home/ubuntu/data/")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  #getting a list of SNPs to analyze
  all_SNPs<-data.frame(SNP=vector(),chr_name=vector(),stringsAsFactors = F)		
  for(module in list.files("/home/ubuntu/srv/impute-me",full.names=T)){
    if(!file.info(module)["isdir"])next
    if("SNPs_to_analyze.txt" %in% list.files(module)){
      SNPs_to_analyze<-read.table(paste(module,"/SNPs_to_analyze.txt",sep=""),sep="\t",stringsAsFactors=F,header=T,quote="",comment="")
      if(!all(c("SNP","chr_name")%in%colnames(SNPs_to_analyze)))stop(paste("In",module,"a SNPs_to_analyze file was found that lacked the SNP and chr_name column"))
      SNPs_to_analyze[,"chr_name"]<-as.character(SNPs_to_analyze[,"chr_name"])
      if(!all(SNPs_to_analyze[,"chr_name"]%in%c(1:22,"X","input")))stop(paste("In",module,"a SNPs_to_analyze had a chr_name column that contained something else than 1:22 and X"))
      all_SNPs<-rbind(all_SNPs,SNPs_to_analyze[,c("SNP","chr_name")])
      
    }
  }
  
  
  
  
  
  
  #an extra check for non-discrepant chr info
  if(any(duplicated(all_SNPs[,"SNP"]))){
    duplicates<-all_SNPs[duplicated(all_SNPs[,"SNP"]),"SNP"]
    for(duplicate in duplicates){
      if(length(unique(all_SNPs[all_SNPs[,"SNP"]%in%duplicate,"chr_name"]))!=1)stop(paste("Found a multi-entry SNP",duplicate,"with discrepant chr info"))
    }
    all_SNPs<-all_SNPs[!duplicated(all_SNPs[,"SNP"]),]
  }
  rownames(all_SNPs)<-all_SNPs[,"SNP"]
  
  for(uniqueID in uniqueIDs){
    print(paste("Checking all requested SNPs from",uniqueID))	
    
    genotypes<-try(get_genotypes(uniqueID=uniqueID,request=all_SNPs))
    if(class(genotypes)=="try-error"){
      if(file.exists(paste("/home/ubuntu/data/",uniqueID,"/temp",sep=""))){
        next
      }else{
        print("Some other error happened in the extraction crawler, but probably no cause for alarm:")
        print(genotypes)
      }
    }
    
    genotypes<-try(get_genotypes(uniqueID=uniqueID,request=all_SNPs))
    
  }
  
  
  
  
  #getting the nonsenser SNPs if possible
  e<-try(load("/home/ubuntu/srv/impute-me/nonsenser/2015-12-16_all_coding_SNPs.rdata"))
  if(class(e)!="try-error"){
    for(uniqueID in uniqueIDs){
      genotypes<-try(get_genotypes(uniqueID,coding_snps,namingLabel="cached.nonsenser"))
    }
  }
  
  #getting the AllDiseases SNPs if possible
  e<-try(load("/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_all_gwas_snps.rdata"))
  if(class(e)!="try-error"){
    for(uniqueID in uniqueIDs){
      genotypes<-try(get_genotypes(uniqueID,gwas_snps,namingLabel="cached.all_gwas"))
    }
  }
}














make_overview_of_samples<-function(verbose=T){
  uniqueIDs<-list.files("/home/ubuntu/data/")
  all_pData<-list()
  for(uniqueID in uniqueIDs){
    pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    if(file.exists(pDataFile)){
      all_pData[[uniqueID]]<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
    }else{
      if(verbose)print(paste("Didn't find a pData file for",uniqueID))	
    }
  }
  all_columns<-unique(unlist(lapply(all_pData,colnames)))
  pData<-as.data.frame(matrix(nrow=0,ncol=length(all_columns),dimnames=list(NULL,all_columns)))
  for(uniqueID in names(all_pData)){
    p<-all_pData[[uniqueID]]
    for(missing_col in all_columns[!all_columns%in%colnames(p)]){
      p[1,missing_col]<-NA
    }
    pData<-rbind(pData,p[,all_columns])
  }
  rownames(pData)<-pData[,"uniqueID"]
  return(pData)
  
  
}




format_ancestry_com_as_23andme<-function(path){
  #this is a function to be called whenever a text file with 5 columns and header row needs to be reformatted to a text file with 4 columns and no header rows (and 20 commented out lines at the top). I.e. when reforming from ancestry.com to 23andme format.
  
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  #logging
  m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"invoking format_ancestry_com_as_23andme",path)
  m<-paste(m,collapse="\t")
  write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
  
  
  testRead<-read.table(path,nrow=10,stringsAsFactors=F,header=T)
  if(ncol(testRead)!=5){stop("testRead of file didn't have 5 columns (as it should have when invoking ancestry.com conversion)")}
  if(unique(sub("[0-9]+$","",testRead[,1]))!="rs")stop("testRead seemed like ancestry.com data, but didn't have rs IDs in column 1")
  
  
  
  #inserting # at first rsid palce
  cmd1<-paste("sed 's/^rsid/#rsid/' ",path," > tempOut0.txt",sep="")
  system(cmd1)
  
  
  #retain only non-commented lines
  cmd2<-paste("awk 'NF && $1!~/^#/' tempOut0.txt > tempOut1.txt",sep="")
  system(cmd2)
  
  #merge column 4 and 5
  cmd3<-paste("awk '{ print $1 \"\t\" $2 \"\t\"$3\"\t\" $4 $5}' tempOut1.txt  > tempOut2.txt",sep="")
  system(cmd3)
  
  #Saving header and insert the right number of commented out lines (20)
  linestoinsert<-20
  cmd4<-paste("sed  -n '/^\\s*#/!{=;q}' tempOut0.txt",sep="")
  commentLineCount<-as.numeric(system(cmd4,intern=T))-1
  header<-readLines("tempOut0.txt",n=commentLineCount)
  
  f<-file("spacer","w")
  headerFull<-c(rep("#",linestoinsert-length(header)),header)
  writeLines(paste(headerFull,collapse="\n"),f)
  close(f)
  cmd5<-paste("cat spacer tempOut2.txt > tempOut3.txt")
  system(cmd5)
  
  
  
  
  file.rename("tempOut3.txt",path)
  
  unlink("spacer")
  unlink("tempOut2.txt")
  unlink("tempOut1.txt")
  unlink("tempOut0.txt")
  
  
}













format_myheritage_as_23andme<-function(path){
  #this is a function to be called whenever a text file with , separation needs to be reformatted to a text file with 4 columns and no header rows. I.e. when reforming from myheritage.com to 23andme format.
  
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  #logging
  m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"invoking format_myheritage_as_23andme",path)
  m<-paste(m,collapse="\t")
  write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
  
  testRead<-read.table(path,nrow=10,stringsAsFactors=F,header=T,sep=",")
  if(ncol(testRead)!=4){stop("testRead of file didn't have 5 columns (as it should have when invoking myheritage conversion)")}
  if(unique(sub("[0-9]+$","",testRead[,1]))!="rs")stop("testRead seemed like myheritage data, but didn't have rs IDs in column 1")
  
  #inserting # at first rsid palce
  cmd1<-paste("sed 's/^RSID/#RSID/' ",path," > tempOut0.txt",sep="")
  system(cmd1)
  
  
  #inserting # at first rsid palce
  cmd1<-paste("sed -e 's/\\,/\\t/g' -e 's/\"//g' tempOut0.txt > tempOut1.txt",sep="")
  system(cmd1)
  
  #retain only non-commented lines
  cmd2<-paste("awk 'NF && $1!~/^#/' tempOut1.txt > tempOut2.txt",sep="")
  system(cmd2)
  
  #Saving header and insert the right number of commented out lines (20)
  linestoinsert<-20
  cmd4<-paste("sed  -n '/^\\s*#/!{=;q}' tempOut0.txt",sep="")
  commentLineCount<-as.numeric(system(cmd4,intern=T))-1
  header<-readLines("tempOut0.txt",n=commentLineCount)
  
  f<-file("spacer","w")
  headerFull<-c(rep("#",linestoinsert-length(header)),header)
  writeLines(paste(headerFull,collapse="\n"),f)
  close(f)
  cmd5<-paste("cat spacer tempOut2.txt > tempOut3.txt")
  system(cmd5)
  
  file.rename("tempOut3.txt",path)
  
  unlink("spacer")
  unlink("tempOut1.txt")
  unlink("tempOut0.txt")
  unlink("tempOut2.txt")
}











snps<-c("rs1050829","rs1050828","Rs28929474")


remove_snps_from_cache<-function(snps,verbose=T){
  if(class(snps)!="character")stop("snps must be class character")
  if(any(duplicated(snps)))stop("snps must not have duplications")
  uniqueIDs<-list.files("/home/ubuntu/data/")
  
  for(uniqueID in uniqueIDs){
    cacheFile<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.gz",sep="")
    if(file.exists(cacheFile)){
      cache<-read.table(cacheFile,header=T,stringsAsFactors=F,row.names=1)
    }else{
      if(verbose)print(paste("Didn't find a cache file for",uniqueID))	
      next
    }
    if(any(snps%in%rownames(cache))){
      snpsFound<-snps[snps%in%rownames(cache)]
      if(verbose){
        print(paste("removing",	length(snpsFound),"snps from",uniqueID,"- they are:",paste(snpsFound,collapse=","),"and have values",paste(cache[snpsFound,"genotype"],collapse=", ")))
      }
      
      cache<-cache[!rownames(cache)%in%snps,,drop=F]
      
      f<-gzfile(cacheFile,"w")
      write.table(cache,file=f,sep="\t",col.names=NA)
      close(f)
      
    }
    
  }
}



















remove_all_temp_folders<-function(uniqueIDs=NULL){
  #A function that will crawl all data directories and remove any lingering temp folders - only use with manual execution
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files("/home/ubuntu/data/")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  for(uniqueID in uniqueIDs){
    tempFolder<-paste("/home/ubuntu/data/",uniqueID,"/temp",sep="")
    if(file.exists(tempFolder)){
      print(paste("Deleting",tempFolder))
      unlink(tempFolder,recursive=T)
    }
  }
}



remove_all_empty_data_folders<-function(uniqueIDs=NULL){
  #A function that will crawl all data directories and remove any that are empty. These can happen on submission errors. Best to just execute manually
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files("/home/ubuntu/data/")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  for(uniqueID in uniqueIDs){
    dataFolder<-paste("/home/ubuntu/data/",uniqueID,sep="")
    filesInside<-list.files(dataFolder)
    if(length(filesInside) == 0){
      print(paste("Deleting",dataFolder,"because it was empty"))
      unlink(dataFolder,recursive=T)
    }
  }
}









get_GRS_2<-function(snp_data, mean_scale=T, unit_variance=T, verbose=T){
  #snp_data       a data frame with genotype, effect sizes and information on effect/non-effect allele. Optionally also information about minor allele frequency and minor/major allele (for use with mean scaling etc)
  #mean_scale     logical. If TRUE the GRS output is scaled so that the average person, by MAF-information, will have a score of 0
  #unit_variance  logical. If TRUE the GRS output is scaled so that 68% of everyone, by MAF/HWE-information, are within 1 unit of 0 (=1 SD)
  
  
  
  if(class(snp_data)!="data.frame")stop(paste("snp_data must be data.frame, not",class(snp_data)))
  if("Beta"%in%colnames(snp_data) & !"effect_size"%in%colnames(snp_data)){
    warning("No 'effect_size' column was found, as is necessary per 2017-03-14 - but a 'Beta' column was renamed to 'effect_size'. Do fix in the future")
    colnames(snp_data)[colnames(snp_data)%in%"Beta"]<-"effect_size"
  }
  necessary_columns<-c("genotype","effect_allele","non_effect_allele","effect_size")
  if(!all(necessary_columns%in%colnames(snp_data))){
    stop(paste("snp_data was lacking necessary columns",paste(necessary_columns[!necessary_columns%in%colnames(snp_data)],collapse=", ")))
  }
  if(!all(unique(sub("[0-9].+$","",rownames(snp_data)))%in%c("i","rs")))stop("snp_data must have rownames starting with rs")
  if(class(snp_data[,"effect_size"])!="numeric")stop("Class of the effect_size column in the snp_data object must be numeric")
  
  
  if(class(mean_scale)!="logical")stop(paste("mean_scale must be logical, not",class(mean_scale)))
  if(length(mean_scale)!=1)stop(paste("mean_scale must be length 1"))
  if(mean_scale){
    necessary_columns_2<-c("minor_allele","major_allele","minor_allele_freq")
    if(!all(necessary_columns_2%in%colnames(snp_data)))stop(paste("in mean-scaling, snp_data must have columns",paste(necessary_columns_2,collapse=", ")))
  }
  
  if(class(unit_variance)!="logical")stop(paste("unit_variance must be logical, not",class(unit_variance)))
  if(length(unit_variance)!=1)stop(paste("unit_variance must be length 1"))
  if(!mean_scale & unit_variance)stop("Cannot use unit_variance if not also using mean_scale")
  # if(unit_variance)stop("Unit variance is not implemented yet")
  
  if(class(verbose)!="logical")stop(paste("verbose must be logical, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be length 1"))
  
  
  
  
  snp_data[,"personal_score"] <- NA
  snp_data[,"population_score_average"] <- NA
  snp_data[,"population_score_sd"] <- NA
  snp_data[,"score_diff"] <- NA
  missing_snps<-vector()
  missing_major_minor_snps<-vector()
  missing_effect_info_snps <- vector()
  
  for(snp in rownames(snp_data)){
    #check for missing genotype
    if(is.na(snp_data[snp,"genotype"])){
      missing_snps <- c(snp, missing_snps)  
      next
    }
    
    #get effect/non-effect-alleles and genotypes
    genotype<-strsplit(snp_data[snp,"genotype"],"/")[[1]]
    effect_allele<-snp_data[snp,"effect_allele"]
    non_effect_allele<-snp_data[snp,"non_effect_allele"]
    
    #check if the effect allele info is missing
    if(any(is.na(c(effect_allele,non_effect_allele))) | any(c(effect_allele,non_effect_allele)%in%"?")){
      missing_effect_info_snps <- c(missing_effect_info_snps, snp)
      next
    }
    
    #check if the genotype is part of these
    if(!all(genotype%in%c(effect_allele,non_effect_allele))){
      missing_effect_info_snps <- c(missing_effect_info_snps, snp)
      next
    }
    
    #get effect_size      
    effect_size<-snp_data[snp,"effect_size"]	
    if(is.na(effect_size)){
      missing_effect_info_snps <- c(missing_effect_info_snps, snp)
      next
    }
    personal_effect_allele_count <- sum(genotype%in%effect_allele)
    snp_data[snp,"personal_score"] <- personal_effect_allele_count *  effect_size
    
    #if mean scale, then also calculate the average score in this population (based on MAF)    
    if(mean_scale){
      #get major/minor/maf info
      major_allele<-snp_data[snp,"major_allele"]
      minor_allele<-snp_data[snp,"minor_allele"]
      minor_allele_freq<-snp_data[snp,"minor_allele_freq"]
      
      #check if they are missing
      if(is.na(major_allele) | is.na(minor_allele) | is.na(minor_allele_freq) | minor_allele=="?" | major_allele=="?"){
        missing_major_minor_snps <- c(snp, missing_major_minor_snps)  
        next
      }
      
      #check if major-minor and effect-non-effect are consistent, and get effect_allele_freq
      if(minor_allele == effect_allele & major_allele == non_effect_allele){
        effect_allele_freq <- minor_allele_freq
      }else if(minor_allele == non_effect_allele & major_allele == effect_allele){
        effect_allele_freq <-  1 - minor_allele_freq
      }else{
        stop(paste("discrepancy between effect/non-effect allele and major/minor allele for SNP",snp))
      }
      
      #Calculate what the average score is for this population and also the difference with personal score
      average_effect_allele_count <- effect_allele_freq * 2
      snp_data[snp,"population_score_average"] <- average_effect_allele_count * effect_size
      snp_data[snp,"score_diff"]<-snp_data[snp,"personal_score"] - snp_data[snp,"population_score_average"]
      
            
      #calculate the extent of possible variance of the score
      #in other words -- Z-scores. The population mean will always be zero... but now we can ensure that 68% (1 SD) is within "1" and 95% (2 SD) is within "2"...
      if(unit_variance){
        frac_0 <- (1-effect_allele_freq)^2
        frac_1 <- (1-effect_allele_freq)*(effect_allele_freq)*2
        frac_2 <- (effect_allele_freq)^2
        mean <- (frac_1 * 1 * effect_size + frac_2 * 2 * effect_size)
        sigma<-(0*effect_size - mean)^2 * frac_0  + (1*effect_size - mean)^2 * frac_1  + (2*effect_size - mean)^2 * frac_2 
        population_sd<-( sigma)^0.5
        snp_data[snp,"population_score_sd"] <- population_sd
      }
    }      
  }      
  

  #round values
  for(col in c("personal_score","population_score_average","population_score_sd","score_diff")){
    SNPs_to_analyze[,col]<-signif(SNPs_to_analyze[,col],2)
  }
  
  
  #follow up on the warning message  
  if(length(missing_snps)>0 & verbose){
    warning(paste("Note, for",length(missing_snps),"SNPs, we found missing genotypes. This can cause errors particularly if the data is not mean centered. These were skipped:",paste(missing_snps,collapse=", ")))
  }
  
  if(length(missing_major_minor_snps)>0 & verbose){
    warning(paste("Note, for",length(missing_major_minor_snps),"SNPs, we found missing major/minor/freq-allele information. These SNPs were skipped:",paste(missing_major_minor_snps,collapse=", ")))      
  }
  
  if(length(missing_effect_info_snps)>0  & verbose){
    warning(paste("Note, for",length(missing_effect_info_snps),"SNPs, we found wrong or missing information on what was effect-allele and what was non-effect-allele. They were skipped:",paste(missing_effect_info_snps,collapse=", ")))
    
  }
  
  
  
  #the output can be summarized in several ways summarize results
  #in the no-mean scale no-unit variance case, the choice is simply between sum and mean of personal score.
  # GRS <- sum(snp_data[,"personal_score"],na.rm=T)

  #In the mean-scale but no-unit variance, one should use the score_diff, either as sum or mean.
  #Using the mean is compatible with default settings for plink 1.9
  # GRS <- mean(snp_data[,"score_diff"],na.rm=T)
    
  #In the mean-scale and unit-variance case - one should (could) use the population_score_sd to
  #normalize the overall GRS to unit-variance. This is done first by summing all the per-SNP GRS's
  #like this (see http://mathworld.wolfram.com/NormalSumDistribution.html for details)
  # population_sum_sd<-sqrt(sum(snp_data[,"population_score_sd"]^2))
  # GRS <-sum(snp_data[,"score_diff"],na.rm=T) / population_sum_sd
  
  
  return(snp_data)
}












generate_report<-function(uniqueIDs=NULL, filename=NULL){
  #A function that will crawl all data directories and generate report with various 
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files("/home/ubuntu/data/")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  if(is.null(filename)){
    filename <- paste0(sample(1000:9999,1),sample(1000:9999,1),"_report.pdf")	
  }else{
    if(class(filename)!="character")stop("filename must be of class character")
    if(length(filename)!=1)stop("filename must be of length 1")
    
  }
  filepath <- paste0("/home/ubuntu/srv/impute-me/www/",filename)
  relative_webpath <- paste0("www/",filename)
  pdf(filepath,width=5,height=8)
  layout(matrix(1:6,nrow=3,byrow=T))
  
  
  first_timeStamps<-vector()
  user_log<-data.frame(uniqueIDs=vector(),modules=vector(),dates=vector(),stringsAsFactors=F)
  for(uniqueID in uniqueIDs){
    pData_file<-paste("/home/ubuntu/data",uniqueID,"pData.txt",sep="/")
    if(!file.exists(pData_file))next
    pData<-read.table(pData_file,sep="\t",header=T,stringsAsFactors=F)
    first_timeStamps<-c(first_timeStamps,pData[1,"first_timeStamp"])
    
    user_log_file<-paste("/home/ubuntu/data",uniqueID,"user_log_file.txt",sep="/")
    if(file.exists(user_log_file)){
      user_log_here<-readLines(user_log_file)
      s<-strsplit(user_log_here,"\t")
      dates<-sapply(s,function(x){x[1]})
      modules<-sapply(s,function(x){x[2]})
      o<-data.frame(uniqueIDs=rep(uniqueID,length(user_log_here)),modules=modules,dates=dates,stringsAsFactors=F)
      user_log<-rbind(user_log,o)
    }else{
      # user_log<-c()
    }
    
  }
  
  sampleSize<-data.frame(dates=sort(as.Date(first_timeStamps)),count=1:length(first_timeStamps))
  plot(type='l',x=sampleSize[,"dates"],sampleSize[,"count"],xlab="Date",ylab="Sample Count",lwd=2,main="Sample size")
  
  
  user_log<-user_log[order(strptime(user_log[,"dates"],format="%Y-%m-%d-%H-%S")),]
  
  for(module in sort(unique(user_log[,"modules"]))){
    u1<-user_log[user_log[,"modules"]%in%module,]
    u1[,"count"]<-1:nrow(u1)
    # strptime(user_log[,"dates"],format="%Y-%m-%d-%H-%S"))
    plot(type='s',x=strptime(u1[,"dates"],format="%Y-%m-%d-%H-%S"),u1[,"count"],xlab="Date",ylab="",lwd=2,main=module,col=rgb(1,0,0,0.7))
    
    par(new = T)
    u2<-u1[!duplicated(u1[,"uniqueIDs"]),,drop=FALSE]
    u2[,"count"]<-1:nrow(u2)
    plot(type='s',x=strptime(u2[,"dates"],format="%Y-%m-%d-%H-%S"),u2[,"count"],lwd=2,col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    
    # plot(type='s',x=1:100,y=1:100*sample(100:200,100),lwd=2,col=rgb(0,0,1,0.7),xaxt="n",yaxt="n",xlab="",ylab="")
    legend("topleft",col=c(rgb(1,0,0,0.7),rgb(0,0,1,0.7)),lty=1,legend=c("Unique Requests (left)","Unique Users (right)"),cex=0.7,lwd=2)
    
    
  }
  dev.off()
  return(relative_webpath)
}











run_export_script<-function(uniqueIDs=NULL,modules=NULL){
  require(jsonlite)
  #A function that will crawl all module directories and execute the export script if present
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files("/home/ubuntu/data/")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  if(is.null(modules)){
    modules<-list.files("/home/ubuntu/srv/impute-me/")
    
  }else{
    if(class(modules)!="character")stop("modules must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/srv/impute-me/",modules,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  
  for(uniqueID in uniqueIDs){
    outputList <- list()
    outputList[["current_date_stamp"]] <- as.character(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"))
    #importing standard pData stuff
    pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F),silent=T)
    if(class(pData)=="try-error"){
      print(paste("uniqueID",uniqueID,"was skipped due to inavailability of pData file"))
      next
    }
    if(nrow(pData)!=1)stop("pData file must have 1 row")
    
    #check existence of cached file
    cachedFile<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.gz",sep="")
    cachedData<-try(read.table(cachedFile,header=T,stringsAsFactors=F),silent=T)
    if(class(cachedData)=="try-error"){
      print(paste("uniqueID",uniqueID,"was skipped due to inavailability of cachedData file"))
      next
    }
    
    for(imp in c("uniqueID","filename","email","first_timeStamp")){
      if(!imp %in%colnames(pData))stop(paste("pData lacked this column:",imp))  
      outputList[[imp]] <-pData[1,imp]
    }
    names(outputList)[names(outputList)%in%"filename"] <- "original_filename"
    names(outputList)[names(outputList)%in%"email"] <- "original_submission_email"
    
    
    for(module in modules){
      if(!file.info(paste0("/home/ubuntu/srv/impute-me/",module))["isdir"])next
      if("export_script.R" %in% list.files(paste0("/home/ubuntu/srv/impute-me/",module))){
        print(paste("Running",module,"for",uniqueID))
        if(exists("export_function"))suppressWarnings(rm("export_function"))
        source(paste(paste0("/home/ubuntu/srv/impute-me/",module,"/export_script.R")))
        if(!exists("export_function"))stop(paste("In module",module,"there was an export_script.R without an export_function"))
        exp <- export_function(uniqueID)
        outputList[[module]] <-exp
        
      }
    }
    
    JSON<-toJSON(outputList)
    
    filename <- paste0("/home/ubuntu/data/",uniqueID,"/",paste(uniqueID,"data.json",sep="_"))
    
    
    f<-file(filename,"w")
    writeLines(JSON,f)
    close(f)
    
  }
  
  m<-paste("The module was successfully run for",length(uniqueIDs),"samples on",length(modules),"modules")
  
  
  return(m)
}


