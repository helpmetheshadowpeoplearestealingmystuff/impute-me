

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
if(!exists("paypal"))stop("Didn't find paypal")
if(!is.character(paypal))stop("paypal not character")
if(length(paypal)!=1)stop("paypal not length 1")


prepare_23andme_genome<-function(path, email, filename, protect_from_deletion){
  library(tools)
  
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be lengh 1, not",length(filename)))
  
  
  #instead of pulling errors, just fix it yourself (observed some users got confused already)
  filename<-gsub("\\ ","_",filename)
  filename<-gsub("[\\$\\&\\+\\,\\:\\;\\=\\?\\@\\#\\\"\\\']","",filename)
  
  
  # if(length(grep(" ",filename))>0){
  #   m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"blank_space",email,filename)
  #   m<-paste(m,collapse="\t")
  #   write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
  #   stop(safeError("Please don't have any blank spaces in the filename of the uploaded files."))
  #   
  # }
  # if(length(grep("[\\$\\&\\+\\,\\:\\;\\=\\?\\@\\#\\\"\\\']",filename))>0){
  #   m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"special_character",email,filename)
  #   m<-paste(m,collapse="\t")
  #   write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
  #   stop(safeError("Please don't have any special characters in the filename of the uploaded files."))
  # }
  
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be lengh 1, not",length(protect_from_deletion)))
  
  if(class(email)!="character")stop(paste("email must be character, not",class(email)))
  if(length(email)!=1)stop(paste("email must be lengh 1, not",length(email)))
  if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
    stop(safeError(paste("a real email adress is needed:",email)))
  }
  
  acceptedMails<-read.table("/home/ubuntu/misc_files/accepted_emails.txt",stringsAsFactors=F)[,1]
  if(!email%in%acceptedMails & FALSE){ #changed to always accept submission for now
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"not_accepted_email",email,path)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    stop(safeError("At the current stage, the project is only open to backers. Please visit our kickstarter page at: http://kck.st/1VlrTlf - sorry for the inconvenience. Going forward the plan is to run on a more voluntary pricing basis, always as non-profit (see terms-of-use). No data was saved."))
  }
  
  
  
  #check for too many ongoing imputations
  print("check for too many ongoing imputations")
  s<-list.files("/home/ubuntu/imputations/")
  if(length(grep("^imputation_folder",s)) >= maxImputationsInQueue){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_many_jobs",email,length(grep("^imputation_folder",s)))
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    
    stop(safeError(paste("More than",maxImputationsInQueue,"imputations are already in progress. Cannot start a new one. Limited server capacity was the reason for our kickstarter campaign. Supporters were first in line: kck.st/1VlrTlf")))
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
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    stop(safeError("Problem with unique ID generation. Please re-load and try again."))
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
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
      stop(safeError("Don't submit gz-files. Only uncompressed text or zip-files. If you already know what a gz file is, this should be easy for you. Please format as tab separated text files."))
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
    #Getting a slightly more informative error message for the submission log
    # testRead2<-try(readLines(path,n=5))
    # if(class(testRead2)=="try-error"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"general_data_file_problem",email,uniqueID)
    # }else{
    # first_five_lines<-paste(testRead2,collapse=" // ")
    # if(nchar(first_five_lines)> 200){
    # first_five_lines <- substr(first_five_lines,1,199)
    # }
    # m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"general_data_file_problem",email,uniqueID,first_five_lines)
    # }
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)	
    unlink(homeFolder,recursive=T)
    stop(safeError("Your file didn't seem like genomic data at all. It must contain many rows, one per SNP, with information about your genotype. Please write an email if you think this is a mistake and that this file format should be supported."))
  }
  
  
  #Checking if it as a Genes for Good file (have to reject those, since it's different genome built)
  # if(length(grep("genes for good",tolower(readLines(path,n=2))))>0){
  #   m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"genes_for_good_error",email,uniqueID)
  #   m<-paste(m,collapse="\t")
  #   write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
  #   unlink(homeFolder,recursive=T)
  #   stop(safeError(paste0("Your file seemed to be from Genes for Good. At the moment we can't accept data from Genes for Good because it is made in a different genomic version than other direct-to-consumer data. If you know how to translate to GRCH37-built yourself, you may remove the 'Genes for Good' line in the header and try to resubmit. Otherwise - we are working on a solution.")))
  #   
  # }
  
  
  #checking if there is at least 10k lines (otherwise imputation wouldn't be possible anyway)
  cmd1 <- paste0("wc -l ",path)
  lines<- as.numeric(sub(" .+$","",system(cmd1,intern=T)))
  if(lines < 10000){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_few_lines_error",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    unlink(homeFolder,recursive=T)
    stop(safeError(paste0("Your file only had ",lines," lines. That doesn't look like a genome-wide microarray input file. Genome-wide microarray files have many formats and come from many places (23andme, myheritage, ancestry, geneplaza, etc), but they always have hundreds of thousands of measurements")))
  }
  
  #running the alternative format converters
  if(ncol(testRead)==5){
    #This could be an ancestry.com file. Check that first
    testRead2<-read.table(path,nrow=10,stringsAsFactors=F,header=T)
    if(unique(sub("[0-9]+$","",testRead2[,1]))!="rs"){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"ancestry_problem",email,uniqueID)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)	
      unlink(homeFolder,recursive=T)
      stop(safeError("Your file seemed like ancestry.com data, but didn't have rs IDs in column 1"))
    }
    #ok, this is probably an ancestry.com file. Let's reformat.
    reformat_outcome<-try(format_ancestry_com_as_23andme(path))
    
  }else if(ncol(testRead)==1){
    #this could be myheritage. Let's try with that
    reformat_outcome<-try(format_myheritage_as_23andme(path))
  }else{
    reformat_outcome<-"didn't try"
  }
  
  if(class(reformat_outcome)=="try-error"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"reformat_error",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    unlink(homeFolder,recursive=T)
    stop(safeError("Your file didn't seem to match any of our import algorithms. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
  }
  
  
  #after reformat attempts, perform one more test read and consider
  testRead2<-read.table(path,nrow=10,stringsAsFactors=F)
  if(ncol(testRead2)!=4){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_4_columns",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    unlink(homeFolder,recursive=T)
    stop(safeError("Your file didn't have 4 columns (or 5 for ancestry.com data). If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
  }
  if(unique(sub("[0-9]+$","",testRead2[,1]))!="rs"){
    unlink(paste("/home/ubuntu/data/",uniqueID,sep=""),recursive=T)
    unlink(homeFolder,recursive=T)
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_no_rs_id",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    unlink(homeFolder,recursive=T)
    stop(safeError("Your file didn't have rs IDs in column 1. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
  }
  
  
  
  
  ##checking if this job has not actually been run before
  print("checking if this job has not actually been run before")
  this_person_md5sum <- md5sum(path)
  all_md5sums<-read.table("/home/ubuntu/misc_files/md5sums.txt",sep="\t",stringsAsFactors = F)[,1]
  if(this_person_md5sum %in% all_md5sums){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"md5sum_match",email,this_person_md5sum)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    unlink(homeFolder,recursive=T)
    stop(safeError("A person with this genome was already analyzed by the system. Write an email if you wish to clear this flag."))
  }
  write(this_person_md5sum,file="/home/ubuntu/misc_files/md5sums.txt",append=TRUE)			
  
  
  
  print("Finalize")
  save(uniqueID,email,filename,protect_from_deletion,file=paste(homeFolder,"variables.rdata",sep=""))
  unlink("job_status.txt")
  write.table("Job is ready",file="job_status.txt",col.names=F,row.names=F,quote=F)
  
  
  
  #New 2017-04-11 send off a mail as a receipt of data (many people asked for this)
  library("mailR")
  library("rJava")
  queue_length <- length(list.files("/home/ubuntu/imputations/"))
  message_start <-paste0("<HTML>We received your data from file <i>", filename,"</i> at www.impute.me. It will now be processed, first through an imputation algorithm and then trough several types of genetic-risk score calculators. This takes approximately 19 hours per genome.")
  if(queue_length > 30){
    
    run_time <- 19 #hours
    servers_running <- 8  #default for summer 2017 (don't want to tinker too much with it)
    genomes_per_day <- servers_running * (run_time / 24)
    days_left <- round(queue_length / genomes_per_day)
    half_days_left <- round(days_left/2)
    
    queue_message<-paste0(" Currently ",queue_length," other genomes are waiting in queue, so expect approximately ",half_days_left,"-",days_left," days of waiting.")
  }else if(queue_length > 5){
    queue_message<-paste0(" Currently ",queue_length," other genomes are waiting in queue, so expect several days of waiting.")
  }else{
    queue_message<-""
  }
  message_end <-paste0(" The service is non-profit, but because of heavy computing-requirements for the imputation analysis it is not free to run. We therefore strongly encourage you to pay a contribution to keep the servers running (<u><a href='",paypal,"'>paypal</a></u>, suggested 5 USD). Also, if you do this and put your unique ID, (<i>",uniqueID,"</i>) as payment-message, you'll be moved to priority queue. Either way, once the analysis is finished you'll receive a mail containing download links for the imputed data. You will also be able to browse the analytics-interface using this uniqueID.<br></HTML> ")
  message <- paste0(message_start,queue_message,message_end)
  mailingResult<-try(send.mail(from = email_address,
                               to = email,
                               bcc="lassefolkersen@gmail.com",
                               subject = "Imputation is queued",
                               body = message,
                               html=T,
                               smtp = list(
                                 host.name = "smtp.gmail.com", 
                                 port = 465, 
                                 user.name = email_address, 
                                 passwd = email_password, 
                                 ssl = TRUE),
                               authenticate = TRUE,
                               send = TRUE))
  if(class(mailingResult)=="try-error"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"mailing_error",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
  }
  
  
  return(paste("Genome files succesfully submitted. <b>The processing of your genome will take several days to run</b>. Typically between 1 and 5 days, depending on server-queue. When the processing is finished you will receive an email to",email,"with uniqueID and download-instructions. Look in your spam filter if not. You can close this browser window."))
  
  
}




run_imputation<-function(
  rawdata, 
  runDir, 
  shapeit="/home/ubuntu/impute_dir/bin/shapeit",
  plink="/home/ubuntu/impute_dir/plink",
  impute2="/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2",
  sample_ref="/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
){
  library(tools)
  
  
  if(class(rawdata)!="character")stop(paste("rawdata must be character, not",class(rawdata)))
  if(length(rawdata)!=1)stop(paste("rawdata must be lengh 1, not",length(rawdata)))
  if(!file.exists(rawdata))stop(paste("Did not find rawdata at path:",rawdata))
  
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  
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
  
  #need to always check if the genes_for_good_cleaner should be run
  if(length(grep("genes for good",tolower(readLines(rawdata,n=5)))>0)){
    genes_for_good_cleaner(uniqueID,runDir)
  }
  
  
  #Load data using plink 1.9
  cmd1<-paste(plink,"--noweb --23file",rawdata,"John Doe --recode --out step_1")
  out1<-system(cmd1)
  
  
  #If the standard command fails, we run an extensive error rescue. Hopefully shouldn't be used too often, but is nice for when people submit weird custom-setup data
  if(out1 == 3){
    special_error_check(uniqueID,runDir)
  }  
  
  #Rscript to omit duplicates
  map<-read.table('step_1.map',sep='\t',stringsAsFactors=F,comment.char="")
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
    map<-read.table(paste("step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F,comment.char = "")
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
      stop(paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep=""))
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
  
  if(file.exists(paste0(destinationDir,"/",uniqueID))){
    if(length(list.files(paste0(destinationDir,"/",uniqueID)))>0){
      stop(paste0("The destinationDir '",paste0(destinationDir,"/",uniqueID),"' already exists and has files in it. This is a major unforeseen error")  )
    }
    
  }
  
  
  allFiles1<-list.files(runDir)
  step7Files<-grep("^step_7_chr",allFiles1,value=T)
  step7ResultsFiles<-grep("[0-9]$",step7Files,value=T)
  chromosomes<-unique(sub("_[0-9-]+$","",sub("^step_7_chr","",step7ResultsFiles)))
  chromosomes<-chromosomes[order(suppressWarnings(as.numeric(chromosomes)))]
  
  for(chr in chromosomes){
    print(paste("Merging chunks in chromosome",chr))
    s <-grep(paste("^step_7_chr",chr,"_",sep=""), step7ResultsFiles,value=T)
    s<-s[order(as.numeric(sub("-[0-9]","",sub("^.+_","",s))),as.numeric(substr(sub("^.+_","",s),3,3)))]
    print(paste("For chr",chr,"these were the files to merge:",paste(s,collapse=", ")))
    cmd1<-paste("cat ",paste(s,collapse=" ")," > ",uniqueID,"_chr",chr,".gen",sep="")
    system(cmd1)
    unlink(s)
    
  }	
  
  
  
  genFiles<-paste(uniqueID,"_chr",chromosomes,".gen",sep="")
  if(length(genFiles)==0)stop("Didn't find a single gen-file")
  
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
    cmd12 <- paste("awk '{ if($4 != \"NN\") print}' step_10_chr",chr,".txt  >", sub("\\.gen$","",genFile),".simple_format.txt",sep="")
    system(cmd12)
    
    
    #removing some temporary files
    unlink(list.files(runDir,pattern=paste0("^step_8_chr",chr),full.names=T))
    unlink(list.files(runDir,pattern=paste0("^step_9_chr",chr),full.names=T))
    unlink(list.files(runDir,pattern=paste0("^step_10_chr",chr),full.names=T))
    
  }
  
  
  
  
  #preparing destinationDir
  prepDestinationDir<-paste(destinationDir,"/",uniqueID,sep="")
  if(!file.exists(prepDestinationDir))dir.create(prepDestinationDir)
  
  #zipping and moving simple_format files
  zipFile_simpleformat<-paste(runDir,paste(uniqueID,".simple_format.zip",sep=""),sep="/")
  twentythreeandmeFiles<-paste(uniqueID,"_chr",chromosomes,".simple_format.txt",sep="")
  zip(zipFile_simpleformat, twentythreeandmeFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFile_simpleformat, paste(prepDestinationDir,basename(zipFile_simpleformat),sep="/"))
  unlink(list.files(runDir,pattern="23andme",full.names=T))
  
  #zipping gen files
  zipFileGen<-paste(runDir,paste(uniqueID,".gen.zip",sep=""),sep="/")
  zip(zipFileGen, genFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFileGen, paste(prepDestinationDir,basename(zipFileGen),sep="/"))
  unlink(genFiles)
  
  #move the original file as well
  zipFileOriginal<-paste(runDir,paste(uniqueID,".input_data.zip",sep=""),sep="/")
  zip(zipFileOriginal, paste(uniqueID,"_raw_data.txt",sep=""), flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  file.rename(zipFileOriginal, paste(prepDestinationDir,basename(zipFileOriginal),sep="/"))
  
  
  
  
  #creating the pData file
  load(paste0(runDir,"/variables.rdata"))
  timeStamp<-format(Sys.time(),"%Y-%m-%d-%H-%M")
  md5sum <- md5sum(paste(uniqueID,"_raw_data.txt",sep=""))
  gender<-system(paste("cut --delimiter=' ' -f 6 ",runDir,"/step_4_chr22.sample",sep=""),intern=T)[3]
  f<-file(paste0(prepDestinationDir,"/pData.txt"),"w")
  writeLines(paste(c("uniqueID","filename","email","first_timeStamp","md5sum","gender","protect_from_deletion"),collapse="\t"),f)
  writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,gender,protect_from_deletion),collapse="\t"),f)
  close(f)
  #determine if it is a bulk or single imputation
  crontabs<-grep("^#",system("crontab -l",intern=T),invert = T,value=T)
  crontabs<-sub(" .+$","",sub("^.+Rscript /home/ubuntu/srv/impute-me/imputeme/","",crontabs))
  if(any(c("bulk_imputation_cron_job.R","imputation_cron_job.R")%in%crontabs)){
    pData<-read.table(paste0(prepDestinationDir,"/pData.txt"),header=T,sep="\t",stringsAsFactors = F)
    if("imputation_cron_job.R"%in%crontabs){
      pData[1,"imputation_type"]<-"single"  
    }else{
      pData[1,"imputation_type"]<-"bulk"  
    }
    write.table(pData,file=paste0(prepDestinationDir,"/pData.txt"),sep="\t",col.names=T,row.names=F,quote=F)
  }
  
  
  
  #return paths
  returnPaths<-c(
    paste(prepDestinationDir,basename(zipFile_simpleformat),sep="/"),
    paste(prepDestinationDir,basename(zipFileGen),sep="/")
  )
  names(returnPaths)<-c("23andme","gen")
  
  return(returnPaths)
}









get_genotypes<-function(
  uniqueID,
  request,
  gtools="/home/ubuntu/impute_dir/gtool",
  namingLabel="cached", #should default to cached, but it's a way of separately saving larger cached sets in a different file
  call_threshold = 0.8 #threshold for calling SNP. Ok with 0.8 for multi-SNP signatures, but should definetly be increased in special high-importance SNPs. Default from gtool is suggested at 0.9.
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
  if(!file.exists(cachedGenotypeFile))print(paste0("Did not find a '",namingLabel, "' chachedGenotypeFile file in idFolder at '",idFolder,"' but that's no problem"))
  
  #creating a temp folder to use
  idTempFolder<-paste("/home/ubuntu/data",uniqueID,"temp",sep="/")
  if(file.exists(idTempFolder))stop(safeError(paste("Temp folder exists, this could indicate that",uniqueID,"is already worked on. Wait a little, or write administrators if you think this is a mistake")))
  
  
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
      stop(safeError(paste("Did not find and .input_data or a .gen file in idFolder. This probably means that raw data has been deleted and no furter SNP-data can be retrieved.")))
    }
    
    
    dir.create(idTempFolder)
    chromosomes<-unique(requestDeNovo[,"chr_name"])
    contents<-unzip(genZipFile,list=T)
    
    #create a blank genotypes object
    genotypes<-data.frame(genotype=vector(),stringsAsFactors=F)
    
    #if input is in as a chromosome, use the 23andmefile as input
    if("input"%in%chromosomes){
      snpsFromInput<-requestDeNovo[requestDeNovo[,"chr_name"]%in%"input","SNP"]
      outZip<-unzip(inputZipFile, overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
      cmd0 <- paste("grep -E '",paste(paste(snpsFromInput,"\t",sep=""),collapse="|"),"' ",outZip,sep="")
      input_genotypes<-system(cmd0,intern=T)
      if(length(input_genotypes)>0){
        input_genotypes<-do.call(rbind,strsplit(input_genotypes,"\t"))
        input_genotypes[,4]<-sub("\r$","",input_genotypes[,4])
        if(any(nchar(input_genotypes[,4])!=2))stop("input data must have length 2 genotypes")
        
        input_genotypes[,4]<-paste(substr(input_genotypes[,4],1,1),substr(input_genotypes[,4],2,2),sep="/")
        genotypes<-data.frame(rsids=input_genotypes[,1],genotype=input_genotypes[,4],stringsAsFactors=F)
        genotypes<-genotypes[!duplicated(genotypes[,"rsids"]),]
        rownames(genotypes)<-genotypes[,"rsids"]
        genotypes[,"rsids"]<-NULL
      }
    }
    
    #if any normal style chromosome names are in use the gen files
    if(any(c(as.character(1:22),"X")%in%chromosomes)){
      chromosomes<-chromosomes[chromosomes%in%c(as.character(1:22),"X")]
      chromosomes<-chromosomes[order(suppressWarnings(as.numeric(chromosomes)))]
      
      gensToExtract<-paste(uniqueID,"_chr",chromosomes,".gen",sep="")
      if(!all(gensToExtract%in%contents[,"Name"])){
        missing<-gensToExtract[!gensToExtract%in%contents[,"Name"]]
        gensToExtract<-gensToExtract[!gensToExtract%in%missing]
        chromosomes<-chromosomes[!chromosomes%in%sub("\\.gen$","",sub("^.+_chr","",missing))]
        warning(paste("These were missing in the zip-gen file:",paste(missing,collapse=", ")))
      }
      outZip<-unzip(genZipFile, files = gensToExtract, overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
      
      f<-file(paste(idTempFolder,"/samples.txt",sep=""),"w")
      writeLines("ID_1 ID_2 missing sex",f)
      writeLines("0 0 0 D",f)
      writeLines(paste(uniqueID,uniqueID,"0.0 2 "),f)#gender probably doesn't matter here
      close(f)
      
      
      #looping over all chromosomes and extracting the relevant genotypes in each using gtools
      for(chr in chromosomes){
        genotypes_here<-data.frame(row.names=vector(),genotype=vector(),stringsAsFactors=F)
        
        #This is wrapped in a try block, because it has previously failed from unpredictable memory issues, so it's better to give a few tries
        for(tryCount in 1:3){
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
          cmd2<-paste(gtools," -G --g " ,subsetFile," --s ",idTempFolder,"/samples.txt --snp --threshold ",call_threshold,sep="")
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
    
    if("N/N"%in%genotypes[,"genotype"]){
      genotypes[genotypes[,"genotype"]%in%"N/N","genotype"]<-NA  
    }
    
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
  
  
  
  #getting the AllDiseases + ukbiobank SNPs if possible
  load("/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_all_gwas_snps.rdata")
  e1<-gwas_snps
  load("/home/ubuntu/srv/impute-me/ukbiobank/2017-09-28_all_ukbiobank_snps.rdata")
  e2<-gwas_snps
  e2<-e2[!rownames(e2)%in%rownames(e1),]
  e<-rbind(e1,e2)
  if(class(e)!="try-error"){
    for(uniqueID in uniqueIDs){
      genotypes<-try(get_genotypes(uniqueID,e,namingLabel="cached.all_gwas"))
    }
  }
  
  
  #getting the ethnicity SNPs if possible
  e<-try(load("/home/ubuntu/srv/impute-me/ethnicity/2017-04-03_ethnicity_snps.rdata"))
  if(class(e)!="try-error"){
    for(uniqueID in uniqueIDs){
      genotypes<-try(get_genotypes(uniqueID,ethnicity_snps,namingLabel="cached.ethnicity"))
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
  
  testRead<-read.table(path,nrow=10,stringsAsFactors=F,header=T)
  if(ncol(testRead)!=5){stop("testRead of file didn't have 5 columns (as it should have when invoking ancestry.com conversion)")}
  if(unique(sub("[0-9]+$","",testRead[,1]))!="rs")stop(safeError("testRead seemed like ancestry.com data, but didn't have rs IDs in column 1"))
  
  
  
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
  # m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"invoking format_myheritage_as_23andme",path)
  # m<-paste(m,collapse="\t")
  # write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
  # 
  testRead<-read.table(path,nrow=10,stringsAsFactors=F,header=T,sep=",")
  if(ncol(testRead)!=4){stop("testRead of file didn't have 5 columns (as it should have when invoking myheritage conversion)")}
  if(unique(sub("[0-9]+$","",testRead[,1]))!="rs")stop(safeError("testRead seemed like myheritage data, but didn't have rs IDs in column 1"))
  
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
    snp_data[,col]<-signif(snp_data[,col],2)
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
  
  special_ids<-c("id_613z86871","id_4K806Dh21","id_8E523a1t7")
  
  for(module in sort(unique(user_log[,"modules"]))){
    #only grab for this module
    u1<-user_log[user_log[,"modules"]%in%module,]
    #omit the special-users (e.g. myself)
    u1<-u1[!u1[,"uniqueIDs"]%in%special_ids,]
    
    #get cum-count
    u1[,"count"]<-1:nrow(u1)
    plot(type='s',x=strptime(u1[,"dates"],format="%Y-%m-%d-%H-%S"),u1[,"count"],xlab="Date",ylab="",lwd=2,main=module,col=rgb(1,0,0,0.7))
    
    par(new = T)
    u2<-u1[!duplicated(u1[,"uniqueIDs"]),,drop=FALSE]
    u2[,"count"]<-1:nrow(u2)
    plot(type='s',x=strptime(u2[,"dates"],format="%Y-%m-%d-%H-%S"),u2[,"count"],lwd=2,col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
    axis(4)
    
    # plot(type='s',x=1:100,y=1:100*sample(100:200,100),lwd=2,col=rgb(0,0,1,0.7),xaxt="n",yaxt="n",xlab="",ylab="")
    legend("topleft",col=c(rgb(1,0,0,0.7),rgb(0,0,1,0.7)),lty=1,legend=c("Unique Requests (left)","Unique Users (right)"),cex=0.7,lwd=2)
    
    
  }
  
  
  
  #plot only special ids (myself)
  u1<-user_log
  u1<-u1[u1[,"uniqueIDs"]%in%special_ids,]
  #get cum-count
  u1[,"count"]<-1:nrow(u1)
  plot(type='s',x=strptime(u1[,"dates"],format="%Y-%m-%d-%H-%S"),u1[,"count"],xlab="Date",ylab="",lwd=2,main="Special users",col=rgb(1,0,0,0.7))
  par(new = T)
  u2<-u1[!duplicated(u1[,"uniqueIDs"]),,drop=FALSE]
  u2[,"count"]<-1:nrow(u2)
  plot(type='s',x=strptime(u2[,"dates"],format="%Y-%m-%d-%H-%S"),u2[,"count"],lwd=2,col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4)
  legend("topleft",col=c(rgb(1,0,0,0.7),rgb(0,0,1,0.7)),lty=1,legend=c("Unique Requests (left)","Unique Users (right)"),cex=0.7,lwd=2)
  
  
  
  
  # #generate list of waiting genomes
  # waiting_files<-vector()
  # for(w1 in list.files("/home/ubuntu/imputations",full.names=T)){
  #   if(!file.exists(paste0(w1,"/variables.rdata")))next
  #   load(paste0(w1,"/variables.rdata"))
  #   status<-sub("Job is ","",read.table(paste0(w1,"/job_status.txt"),sep="\t",stringsAsFactors = F)[1,1])
  #   waiting_files<-c(waiting_files,paste(uniqueID,email,status,sep=" - "))
  # }
  # plot(NULL,ylim=c(0,length(waiting_files)+1),xlim=c(0,1),frame=F,xaxt="n",yaxt="n",xlab="",ylab="")
  # for(w2 in 1:length(waiting_files)){
  #   text(x=0.02,y=length(waiting_files)-w2,label=waiting_files[w2],adj=0,cex=0.6)
  # }
  # 
  
  
  dev.off()
  
  
  
  
  
  return(relative_webpath)
}











run_export_script<-function(uniqueIDs=NULL,modules=NULL, delay=0){
  #A function that will crawl all module directories and execute the export script if present
  #uniqueID:    Indicates if specific sets should be processed
  #modules:     Indicates if specific modules should be procssed
  #delay:       an integer, in seconds, giving an optional delay to insert after each uniqueID
  
  require(jsonlite)
  require(shiny) #call safeError, but often will be run from cmd-line
  
  if(class(delay)!="numeric")stop(paste("delay must be numeric, not",class(delay)))
  if(length(delay)!=1)stop(paste("delay must be lengh 1, not",length(delay)))
  
  
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
    #importing standard pData stuff
    pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"),silent=T)
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
    
    
    #get basic stuff
    for(imp in c("uniqueID","filename","email","first_timeStamp")){
      if(!imp %in%colnames(pData))stop(paste("pData lacked this column:",imp))  
      outputList[[imp]] <-pData[1,imp]
    }
    outputList[["current_timeStamp"]] <- as.character(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"))
    outputList[["documentation_url"]] <- "https://github.com/lassefolkersen/impute-me"
    
    names(outputList)[names(outputList)%in%"filename"] <- "original_filename"
    names(outputList)[names(outputList)%in%"email"] <- "original_submission_email"
    
    
    #save ethnicity in pData (because it is needed elsewhere)
    source(paste(paste0("/home/ubuntu/srv/impute-me/","ethnicity"  ,"/export_script.R")))
    ethnicity <- try(export_function(uniqueID))
    if(class(ethnicity)=="try-error"){
      ethnicity<-NA
    }else{
      ethnicity<-ethnicity[["guessed_super_pop"]]
    }
    pDataFile <- paste0("/home/ubuntu/data/",uniqueID,"/pData.txt")
    pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"),silent=T)
    if(class(pData)!="try-error"){
      pData[1,"ethnicity"] <- ethnicity
      write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
      print(paste("Determined and saved ethnicity as:",ethnicity))
    }else{
      print(paste("Couldn't save ethnicity in pData:",ethnicity))
    }
    
    
    
    #get remaining non-ethnicity modules
    for(module in modules){
      if(!file.info(paste0("/home/ubuntu/srv/impute-me/",module))["isdir"])next
      if("export_script.R" %in% list.files(paste0("/home/ubuntu/srv/impute-me/",module))){
        print(paste("Running",module,"for",uniqueID))
        if(exists("export_function"))suppressWarnings(rm("export_function"))
        source(paste(paste0("/home/ubuntu/srv/impute-me/",module,"/export_script.R")))
        if(!exists("export_function"))stop(paste("In module",module,"there was an export_script.R without an export_function"))
        exp <- try(export_function(uniqueID))
        if(class(exp)=="try-error"){next}
        outputList[[module]] <-exp
        
      }
    }
    
    
    filename <- paste0("/home/ubuntu/data/",uniqueID,"/",paste(uniqueID,"data.json",sep="_"))
    
    
    #check if there exists previous json file, with module data that is not re-run.
    #if so, include this.
    if(file.exists(filename)){
      outputList_previous<-fromJSON(filename)
      previous_unique <- outputList_previous[!names(outputList_previous) %in% names(outputList)]
      outputList<-c(outputList,previous_unique)
    }
    
    #save new JSON
    JSON<-toJSON(outputList)
    f<-file(filename,"w")
    writeLines(JSON,f)
    close(f)
    
    
    if(delay > 0){
      Sys.sleep(delay)
      
    }
  }
  
  m<-paste("The module was successfully run for",length(uniqueIDs),"samples on",length(modules),"modules")
  
  
  return(m)
}












re_check_md5sums<-function(){
  library(tools)
  all_md5sums<-read.table("/home/ubuntu/misc_files/md5sums.txt",sep="\t",stringsAsFactors = F)[,1]
  
  
  otherPersons<-list.files("/home/ubuntu/data",full.names=T)
  for(otherPerson in otherPersons){
    if(!file.info(otherPerson)[["isdir"]])next
    if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
    other_person_md5sum<-try(read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")[1,"md5sum"],silent=T)
    if(class(other_person_md5sum)=="try-error")next
    if(is.null(other_person_md5sum))next
    
    all_md5sums<-c(all_md5sums,other_person_md5sum)
  }
  #checking if this job is not already in queue
  for(otherPerson in paste0(list.files("/home/ubuntu/imputations",full.names=T),"/")){
    if(!file.info(otherPerson)[["isdir"]])next
    
    raw_data_file<-grep("raw_data\\.txt",list.files(otherPerson,full.names=T),value=T)
    if(length(raw_data_file)!=1)stop("odd")
    other_person_md5sum<-md5sum(raw_data_file)
    all_md5sums<-c(all_md5sums,other_person_md5sum)
    
  }
  print(paste(sum(duplicated(all_md5sums)),"of",length(all_md5sums),"were duplicated"))
  all_md5sums <- unique(all_md5sums)
  writeLines(all_md5sums,"/home/ubuntu/misc_files/md5sums.txt")
}































# runDir<-"/home/ubuntu/bulk_imputations/2017-09-21-15-59-21_bulk"
# set.seed(42)
# rawdata_files<-sample(list.files("~/imputations",full.names=T),10)
# rawdata_files<-paste0(rawdata_files,"/",sub("^.+folder_","",rawdata_files),"_raw_data.txt")
# shapeit="/home/ubuntu/impute_dir/bin/shapeit"
# plink="/home/ubuntu/impute_dir/plink"
# impute2="/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2"
# sample_ref="/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"



run_bulk_imputation<-function(
  uniqueIDs, 
  runDir, 
  shapeit="/home/ubuntu/impute_dir/bin/shapeit",
  plink="/home/ubuntu/impute_dir/plink",
  impute2="/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2",
  sample_ref="/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
){
  library(tools)
  
  if(class(uniqueIDs)!="character")stop(paste("uniqueIDs must be character, not",class(uniqueIDs)))
  if(length(uniqueIDs)!=10)stop(paste("rawdata must be lengh 10, not",length(uniqueIDs)))
  
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  
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
  
  
  rawdata_files<-paste("/home/ubuntu/imputations/imputation_folder_",uniqueIDs,"/",uniqueIDs,"_raw_data.txt",sep="")
  names(rawdata_files)<-uniqueIDs
  if(!all(file.exists(rawdata_files))){
    missing<-rawdata_files[!file.exists(rawdata_files)]
    stop(paste("Did not find these rawdata files:",paste(missing,collapse=", ")))
  }
  
  
  
  cat(paste0("Starting imputation running on these files:\nc('",paste(uniqueIDs,collapse="','"),"')\nGood luck!\n"))
  setwd(runDir)
  chromosomes <- c("X",as.character(1:22))
  # chromosomes <- c("22")
  
  for(uniqueID in uniqueIDs){
    print("")
    print(paste("Preparing files for",uniqueID))
    
    rawdata_file<-rawdata_files[uniqueID]
    
    #need to always check if the genes_for_good_cleaner should be run
    if(length(grep("genes for good",tolower(readLines(rawdata_file,n=5)))>0)){
      genes_for_good_cleaner(uniqueID,runDir)
    }
    
    #Load data using plink 1.9
    cmd1<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1_",uniqueID)
    out1<-system(cmd1)
    
    
    
    #If the standard command fails, we run an extensive error rescue. Hopefully shouldn't be used too often, but is nice for when people submit weird custom-setup data
    if(out1 == 3 ){
      special_error_check(uniqueID,runDir)
    }  
    
    #Rscript to omit duplicates
    map<-read.table(paste0("step_1_",uniqueID,".map"),sep='\t',stringsAsFactors=F,comment.char = "")
    exclude<-map[duplicated(map[,4]),2]
    print(paste('Removed',length(exclude),'SNPs that were duplicated'))
    write.table(exclude,file=paste0('step_2_',uniqueID,'_exclusions'),sep='\t',row.names=FALSE,col.names=F,quote=F)
    
    #loop over chromosomes
    for(chr in chromosomes){
      #First in loop - extract only one specific chromosome
      cmd2<-paste(plink," --file step_1_",uniqueID," --chr ",chr," --make-bed --out step_2_",uniqueID,"_chr",chr," --exclude step_2_",uniqueID,"_exclusions",sep="")
      system(cmd2)
      
    }  
  }
  
  
  
  #loop over chromosomes
  for(chr in chromosomes){
    merge_files<-paste0("step_2_",sub("_raw_data.txt","",basename(rawdata_files)),"_chr",chr)
    merge_df<-data.frame(bed=paste0(merge_files,".bed"),bim=paste0(merge_files,".bim"),fam=paste0(merge_files,".fam"))
    merge_df<-merge_df[2:nrow(merge_df),]
    write.table(merge_df,file="step_2_merge_list.txt",sep="\t",quote=F,row.names=F,col.names=F)
    

    #check that all files are there
    missing <- vector()
    for(i in 1:nrow(merge_df)){
      for(j in 1:ncol(merge_df)){
        f<-as.character(merge_df[i,j])
        if(!file.exists(f)){
          missing <-c(missing, f)
        }
      }
    }
    if(length(missing)>0)stop(paste("Didn't find these",length(missing),"files:",paste(missing,collapse=", ")))
    
    
    
    #use plink to merge
    cmd1_a <- paste0(plink," --bfile ",merge_files[1]," --merge-list step_2_merge_list.txt --recode --out step_2m_chr",chr)
    out1_a<-system(cmd1_a)
    
    
    #handling nonbiallelic (just excluding them)
    if(out1_a==3){
      missnp<-read.table(paste0("step_2m_chr",chr,".missnp"),stringsAsFactors = F)[,1]
      if(length(missnp) > 500) {
        cat(paste("\n\nThe missnp>500 error was triggered. Outputting debug info\n\n"))
        debug_info<-list()
        for(uniqueID in uniqueIDs){
          cmd1_b<-paste0(plink," --bfile step_2_",uniqueID,"_chr",chr," --recode --out step_2_",uniqueID,"_chr",chr,"_missnp_hunt --extract step_2m_chr",chr,".missnp")
          system(cmd1_b)
          debug_ped<-readLines(paste0("step_2_",uniqueID,"_chr",chr,"_missnp_hunt.ped"))
          debug_gt<-strsplit(debug_ped," ")[[1]]
          debug_gt<-debug_gt[7:length(debug_gt)]
          debug_df<-data.frame(A1=debug_gt[c(T,F)], A2=debug_gt[c(F,T)])
          debug_df[,"snp"]<-read.table(paste0("step_2_",uniqueID,"_chr",chr,"_missnp_hunt.map"))[,2]
          debug_df[,"uniqueID"] <- uniqueID
          debug_info[[uniqueID]]<-debug_df
        }
        debug_all<-do.call(rbind,debug_info)
        for(msnp in sample(missnp,5)){
          s<-debug_all[debug_all[,"snp"]%in%msnp,]
          rownames(s)<-NULL
          print(msnp)
          print(s)
        }
        stop("Too many non-biallelic SNPs. This must be investigated. Check above debugging info")
          
      }
      for(uniqueID in uniqueIDs){
        #Go back to the previous files from previous step and take them, now excluding triallelics.
        cmd1_c<-paste0(plink," --bfile step_2_",uniqueID,"_chr",chr," --make-bed --out step_2_",uniqueID,"_chr",chr," --exclude step_2m_chr",chr,".missnp")
        system(cmd1_c)
      }
      #then retry merge
      cmd1_a <- paste0(plink," --bfile ",merge_files[1]," --merge-list step_2_merge_list.txt --recode --out step_2m_chr",chr)
      out1_a<-system(cmd1_a)
    }
    
    
    #check for position duplicates
    map<-read.table(paste0("step_2m_chr",chr,".map"),stringsAsFactors = F,comment.char = "")
    if(sum(duplicated(map[,4]))>10000)stop("Found way too many duplicate positions")
    exclude<-unique(map[duplicated(map[,4]),2])
    write.table(exclude,file=paste0('step_2_overall_exclusions_chr',chr),sep='\t',row.names=FALSE,col.names=F,quote=F)
    
    
    cmd1_b<-paste(plink," --file step_2m_chr",chr," --exclude step_2_overall_exclusions_chr",chr," --recode --out step_2_chr",chr,sep="")
    system(cmd1_b)
    
    
    
    #Then check for strand flips etc. 
    cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
    system(cmd3)
    
    
    #Many homozygote SNPs will fail the check, because, well - of course, they don't have the ref-allele. So we make more detailed R script for sorting them
    logFile<-read.table(paste("step_2_chr",chr,"_shapeit_log.snp.strand",sep=""),sep='\t',stringsAsFactors=FALSE,header=F,skip=1,comment.char="")
    omitMissing<-logFile[logFile[,1] %in% 'Missing',3] #SNPs that were not found in 1kgenomes. Lot's of 23andme iXXXXX here.
    logStrand<-logFile[logFile[,1] %in% 'Strand',]
    omitNonIdentical<-logStrand[logStrand[,5] != logStrand[,6],3] #typically indels with different notation than 1kgenomes (<10 counts is usual)
    omitBlank<-logStrand[logStrand[,5]%in%'',3] #these are SNPs that are in map-file but contents is all-blank in input data. Safe to omit
    
    #These are super-annoying. We have to create another (fake) person with the alternative allele just for their sake. This next command takes all the homozygotes, minus the indels (which are too complicated to lift out from 23andme)
    forceHomozygoteTable<-logStrand[
      logStrand[,5] == logStrand[,6] & 
        nchar(logStrand[,9])==1 & 
        nchar(logStrand[,10])==1 &
        !logStrand[,5] %in% c("D","I") &
        !logStrand[,6] %in% c("D","I") 
      ,]
    
    #This removes any cases where there are more than two alleles involved
    forceHomozygoteTable<-forceHomozygoteTable[sapply(apply(forceHomozygoteTable[,c(5,6,9,10)],1,unique),length)==2,]
    
    #This removes any duplicates there might be
    forceHomozygoteTable<-forceHomozygoteTable[!duplicated(forceHomozygoteTable[,4]),]
    map<-read.table(paste("step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F,comment.char = "")
    #This loads the ped file, and doubles it
    p_all<-strsplit(readLines(paste("step_2_chr",chr,".ped",sep=""))," ")
    ped2<-ped1<-p_all[[1]]
    ped2[1]<-"Temporary"
    ped2[2]<-"Non_person"
    if((length(ped1)-6) / 2 !=nrow(map))stop("mismatch between map and ped")
    replacementPos<-which(map[,2]%in%forceHomozygoteTable[,4])
    A1_pos<-7+2*(replacementPos-1)
    A2_pos<-8+2*(replacementPos-1)
    ped2[A1_pos]<-forceHomozygoteTable[,9]
    ped2[A2_pos]<-forceHomozygoteTable[,10]
    ped<-rbind(do.call(rbind,p_all),ped2)
    write.table(ped,paste("step_3_chr",chr,".ped",sep=""),sep=" ",col.names=F,row.names=F,quote=F)
    omitRemaining<-logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
    print(paste('Omitting',length(omitMissing),'because of missing',length(omitBlank),'because they are blank, and',length(omitNonIdentical),'true strand flips'))
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    
    #running the shapeit command (with eleven people, the ten right ones and a placeholder heterozygote
    cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
    system(cmd4)
    
    
    #checking for errors and stopping if there are any. No point to continue otherwise
    log<-readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
    if(substr(log[length(log)],1,5)=="ERROR"){
      stop(paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep=""))
    }
    
    #removing the placeholder person again
    left_limit <- 5 + length(merge_files) * 2
    cmd5_1<-paste0("cut --delimiter=' ' -f 1-",left_limit," step_4_chr",chr,".haps > step_5_chr",chr,".haps")
    system(cmd5_1)
    top_limit <- 2 + length(merge_files) 
    cmd5_2<-paste0("head -n ",top_limit," step_4_chr",chr,".sample > step_5_chr",chr,".sample")
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
  
  
  #clean up pre-step-5 files to save space
  print("Performing file-deletion and clean-up")
  unlink(list.files(pattern="^step_1.+"))
  unlink(list.files(pattern="^step_2.+"))
  unlink(list.files(pattern="^step_3.+"))
  
  
  
  #then copy out each of the step_7 files into separate imputation folders
  sample_file<-grep("step_5",grep("\\.sample$",list.files(),value=T),value=T)[1] #just pick the first one - they should be identical
  samples<-read.table(sample_file,stringsAsFactors=F)
  allFiles1<-list.files(runDir)
  for(chr in chromosomes){
    for(uniqueID in uniqueIDs){
      print(paste("Cutting from",uniqueID,"chromosome",chr))
      # uniqueID<-sub("^.+/","",sub("_raw_data.txt$","",rawdata_file))
      outfolder <- paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/")
      w<-which(samples[,1]%in%uniqueID) -2
      print(paste("Retrieving",uniqueID,"which is",w,"of",nrow(samples)-2))
      
      #cut and transfer sample file
      write.table(samples[c(1:2,w+2),],file=paste0(outfolder,"step_4_chr",chr,".sample"),quote=F,row.names=F,col.names = F)
      
      #cut and transfer gen files
      step7Files<-grep(paste0("^step_7_chr",chr,"_"),allFiles1,value=T)
      step7ResultsFiles<-grep("[0-9]$",step7Files,value=T)
      left <- 5 + (w-1) * 3 + 1
      middle <- 5 + (w-1) * 3 + 2
      right <- 5 + (w-1) * 3 + 3
      for(step7ResultsFile in step7ResultsFiles){
        cmd8<-paste0("cut --delimiter=' ' -f 1,2,3,4,5,",left,",",middle,",",right," ",step7ResultsFile," > ",outfolder,step7ResultsFile)
        system(cmd8)
      }
    }
    Sys.sleep(0.5) #take a break
    unlink(step7Files) #clean up files afterwards (or else we break the 30GB limit)
  }
}

















genes_for_good_cleaner<-function(uniqueID,runDir,plink="/home/ubuntu/impute_dir/plink"){
  print("The genes_for_good_cleaner was activated")
  rawdata_file<-paste("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/",uniqueID,"_raw_data.txt",sep="")
  if(!file.exists(rawdata_file))stop(paste("error in special-error-check: didn't find file at",rawdata_file))
  #Common problem 1 -  # signs in the rsids. Should remove those lines.
  cmd_special_8<-paste0("sed -i.bak6 '/#/d' ",rawdata_file)
  system(cmd_special_8)
}









special_error_check<-function(uniqueID,runDir,plink="/home/ubuntu/impute_dir/plink"){
  print("The special_error_check was activated")
  rawdata_file<-paste("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/",uniqueID,"_raw_data.txt",sep="")
  if(!file.exists(rawdata_file))stop(paste("error in special-error-check: didn't find file at",rawdata_file))
  
  
  
  special_error_status<-vector()
  line_count_cmd<-paste0("wc -l ",rawdata_file)
  line_count_0<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
  
  #Common problem 1: mitochondrial SNPs (not used in any analysis anyway)
  cmd_special_1<-paste("sed -i.bak1 '/\\tMT\\t/d'",rawdata_file)
  system(cmd_special_1)
  line_count_1<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
  if(line_count_1-line_count_0<0)special_error_status <- c(special_error_status, paste0("MT removals (",line_count_1-line_count_0,")"))
  
  
  #Common problem 2: Presence of triple dashes, that should just be double dashes
  md5_before <- md5sum(rawdata_file)
  cmd_special_2<-paste0("sed -i.bak2 's/\\t---/\\t--/' ",rawdata_file)
  system(cmd_special_2)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "--- to --")
  
  
  #Common problem 3: Presence of indels that can't be handled by the plink -23file function
  #this needs to be handled in a very weird way, because clearly awk cant distinguish all line endings systematically
  cmd_special_3a<-paste0("awk '!(length($4) != 3)' ",rawdata_file, " > ",runDir,"/temp_indel_01.txt")
  system(cmd_special_3a)
  line_count_3a<-as.integer(sub(" .+$","",system(paste0("wc -l ",runDir,"/temp_indel_01.txt"),intern=T)))
  
  cmd_special_3b<-paste0("awk '!(length($4) != 2)' ",rawdata_file, " > ",runDir,"/temp_indel_02.txt")
  system(cmd_special_3b)
  line_count_3b<-as.integer(sub(" .+$","",system(paste0("wc -l ",runDir,"/temp_indel_02.txt"),intern=T)))
  
  if(line_count_3a > line_count_3b){
    file.rename(paste0(runDir,"/temp_indel_01.txt"),rawdata_file)
  }else{
    file.rename(paste0(runDir,"/temp_indel_02.txt"),rawdata_file)
  }
  line_count_3<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
  
  
  if(line_count_3-line_count_1<0)special_error_status <- c(special_error_status, paste0("INDEL removals (",line_count_3-line_count_1,")"))
  
  
  
  
  #Common problem 4: lack of sorting (first re-check if this is a problem after MT removal)
  cmd_special_3<-paste(plink,"--noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1")
  sorting_check<-system(cmd_special_3,intern=T)
  if(length(grep("are out of order",sorting_check))>0){
    
    #sorting by chr then pos
    cmd_sort_1<-paste0("sort -k2 -k3 -g -o ",runDir,"/temp01.txt ",rawdata_file)
    system(cmd_sort_1)
    
    #removing Y, xY, 23 chr (too risky to keep in after a sort)
    cmd_sort_2<-paste0("sed -e '/\\tY\\t/d' -e '/\\t23\\t/d' -e '/\\tYX\\t/d' -e '/\\tXY\\t/d' ",runDir,"/temp01.txt > ",runDir,"/temp02.txt")
    system(cmd_sort_2)
    
    #switching X chr and the rest (because X gets sorted first)
    cmd_sort_3<-paste0("grep -v \tX\t ",runDir,"/temp02.txt > ",runDir,"/temp03.txt")
    system(cmd_sort_3)
    cmd_sort_4<-paste0("grep \tX\t ",runDir,"/temp02.txt >> ",runDir,"/temp03.txt")
    system(cmd_sort_4)
    
    #replace X with 23
    cmd_sort_5<-paste0("sed 's/\\tX\\t/\\t23\t/' ",runDir,"/temp03.txt > ",rawdata_file)
    system(cmd_sort_5)
    
    line_count_4<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
    special_error_status<- c(special_error_status,paste0("sorting required (",line_count_3-line_count_4," lines removed)"))
  }
  
  
  
  #common problem 5: Removing all front quotes, all back quotes and all quote-comma-quotes
  md5_before <- md5sum(rawdata_file)
  cmd_special_5<-paste0("sed -i.bak3 -e 's/^\"//g' -e 's/\"$//g' -e 's/\",\"/\\t/g' -e 's/\"\\t\"/\\t/g' ",rawdata_file)
  system(cmd_special_5)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "removing weird quotes")
  
  
  
  #common problem 6: also when there is weird carriage returns    
  md5_before <- md5sum(rawdata_file)
  cmd_special_6<-paste0("sed -i.bak4 's/\"\r//g' ",rawdata_file)
  system(cmd_special_6)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "removing weird carriage returns")
  
  
  #Common problem 7 - build version is wrong
  canaries<-rbind(
    c("rs3762954","662955"),
    c("rs390560","2601689"),
    c("rs10043332","3128346"),
    c("rs10070917","4955950"),
    c("rs11740668","404623"),
    c("rs999292","93218958"),
    c("rs13147822","107960572"),
    c("rs62574625","101218552"),
    c("rs11023374","14903636")
  )
  canaries<-data.frame(canaries,stringsAsFactors = F)
  colnames(canaries)<-c("snp","1kg_pos")
  canaries[,"1kg_pos"] <- as.numeric(canaries[,"1kg_pos"])
  
  
  map_file1 <- paste0(runDir,"/step_1_",uniqueID,".map")
  map_file2 <- paste0(runDir,"/step_1.map")
  if(file.exists(map_file1)){
    map_file<-map_file1
  }else if(file.exists(map_file2)){
    map_file<-map_file2
  }else{
    #This special case is when ALL prior plink runs have failed (probably due to sorting or whatever). In that case we re run plink, we can use the pre-sorting check command which really should work now
    print("re-running to map")
    system(cmd_special_3,intern=F)
    map_file<-map_file2
    if(!file.exists(map_file)){stop("Didn't find map file")}
    }
  map<-read.table(map_file,sep='\t',stringsAsFactors=F,comment.char = "")
  map<-map[!duplicated(map[,2]),]
  rownames(map) <- map[,2]
  canaries<-canaries[canaries[,"snp"]%in%rownames(map),]
  if(nrow(canaries)==0){
    c(special_error_status, "no snps for built check")
  }else{
    canaries[,"input_pos"]<-map[canaries[,"snp"],4]
    if(all(canaries[,"1kg_pos"] == canaries[,"input_pos"])){
      c(special_error_status, paste("passed built check with",nrow(canaries),"snps"))
    }else{
      c(special_error_status, paste("failed built check with",sum(canaries[,"1kg_pos"] != canaries[,"input_pos"]),"of",nrow(canaries),"snps"))
    }
  }
  
  
  
  #Common problem 8 - some providers have started putting # signs in the rsids (genes for good for example). Should remove those lines.
  md5_before <- md5sum(rawdata_file)
  cmd_special_8<-paste0("sed -i.bak5 '/#/d' ",rawdata_file)
  system(cmd_special_8)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "hashtags in rsids")
  
  
  #then re-check and decide future action
  cmd1<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1_",uniqueID)
  out1<-system(cmd1)
  if(out1 == 0 & length(grep("failed built check",special_error_status))==0){
    print(paste0("ok, somehow the special errors section actually cleared up this one. These were the error messages: ",paste(special_error_status,collapse=", ")))
    #and move on
  }else{
    #however if still failing, we have to send a mail
    library("mailR")
    error1<-system(cmd1,intern=T)
    message <- paste0(uniqueID," failed all attempts at starting imputation. It came with special error status:<b> ", paste(special_error_status,collapse=", "),". </b>The last error message was this: ",paste(error1,collapse="\n"),"\n\nThe files under analysis were these:\nc('",paste(uniqueIDs,collapse="','"),"')")
    send.mail(from = email_address,
              to = "lassefolkersen@gmail.com",
              subject = "Imputation has problem",
              body = message,
              html=T,
              smtp = list(
                host.name = "smtp.gmail.com", 
                port = 465, 
                user.name = email_address, 
                passwd = email_password, 
                ssl = TRUE),
              authenticate = TRUE,
              send = TRUE)
    stop("Sending error mail and giving up")
  }
  return(special_error_status)
}  






reset_runs_from_node<-function(uniqueIDs,check_is_running=T){
  #function to reset a bulk-run from Node. Useful if there was a crash and we need to re-run
  
  if(class(uniqueIDs)!="character")stop("uniqueIDs must be a character")
  if(!all(nchar(uniqueIDs)==12))stop("uniqueIDs must be of length 12")
  if(length(grep("^id_",uniqueIDs)) != length(uniqueIDs))stop("all uniqueIDs must start with id_")
  
  if(check_is_running){
    for(uniqueID in uniqueIDs){
      cmd1 <- paste0("ssh ubuntu@",hubAddress," 'cat /home/ubuntu/imputations/imputation_folder_",uniqueID,"/job_status.txt'")
      status<-system(cmd1,intern=T)
      if(status!="Job is remote-running")stop(paste("Status for",uniqueID,"was not remote-running. This must be the case for a reset. Aborting with no change. Status was",status))
    }
  }
  
  
  imp_to_delete<-list.files("~/imputations/")
  if(!all(uniqueIDs %in% sub("imputation_folder_","",imp_to_delete))){
    missing<-uniqueIDs[!uniqueIDs %in% sub("imputation_folder_","",imp_to_delete)]
    uniqueIDs<-uniqueIDs[uniqueIDs %in% sub("imputation_folder_","",imp_to_delete)]
    print(paste("These",length(missing),"uniqueIDs were not found in local imputation folder. They will be ignored also when resetting hub:",paste(missing,collapse=",")))
  }
  
  if(length(imp_to_delete)>length(uniqueIDs)){
    print(paste("Note that there was",length(imp_to_delete),"folders in  ~/imputations, but only a request for deleting",length(uniqueIDs),"uniqueIDs. The additional will be deleted nonetheless"))
  }else{
    print(paste("Deleting",length(uniqueIDs),"uniqueIDs from local ~/imputation folder."))
  }
  unlink(paste0("~/imputations/",imp_to_delete),recursive=T)  
  
  bulk_to_delete<-list.files("~/bulk_imputations/")
  if(length(bulk_to_delete)==1){
    print("Also deleting one folder in ~/bulk_imputations")
  }else{
    print(paste("Deleting",length(bulk_to_delete),"folders in ~/bulk_imputations:",paste(bulk_to_delete,collapse=", ")))
  }
  unlink(paste0("~/bulk_imputations/",bulk_to_delete),recursive=T)  
  
  print(paste("Setting Job ready tag for",length(uniqueIDs),"uniqueIDs on hub at:",hubAddress))
  for(uniqueID in uniqueIDs){
    cmd2 <- paste0("ssh ubuntu@",hubAddress," 'echo Job is ready > /home/ubuntu/imputations/imputation_folder_",uniqueID,"/job_status.txt'")
    system(cmd2)
  }
  
}