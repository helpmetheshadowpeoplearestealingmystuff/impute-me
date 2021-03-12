

get_conf<-function(
  request,
  configuration_file="/home/ubuntu/misc_files/configuration.R"
){
  #' get configurationfile
  #' 
  #' Function that reads the configuration file and makes choices wether to stop for missing values or to 
  #' insert default values, for less important variables. This function is called on a read of the functions.R
  #' to make sure all variables are available.
  #' 
  #' @param request the name of the requested variable
  #' @param configuration_file path to where the figure file is.
  #' 
  #' @return The value of the requested variable, either from the configuration_file or as a default value if not available in configuration file.
  
  if(!is.vector(request))stop("request not numeric")
  if(length(request)!=1)stop("request not length 1")
  
  
  if(file.exists(configuration_file)){
    source(configuration_file)
  }else{
    print(paste0(Sys.time(),": Configuration file not found at:",configuration_file,"setting only default values."))
  }
  
  #always import verbose
  if(!exists("verbose")){
    verbose<-1
    print(paste0(Sys.time(),": variable verbose not found in configuration.R. Setting to default value of 1"))
  }
  if(!is.numeric(verbose))stop("verbose not numeric")
  if(length(verbose)!=1)stop("verbose not length 1")
  
  #then ask for each of the variables
  if(request == "verbose"){
    #already done
  }else if(request == "maxImputations"){
    if(!exists("maxImputations")){
      maxImputations <- 1
      if(verbose>0)print(paste0(Sys.time(),": variable maxImputations not found in configuration.R. Setting to default value of 1"))
    }
    if(!is.numeric(maxImputations))stop("maxImputations not numeric")
    if(length(maxImputations)!=1)stop("maxImputations not length 1")
    if(maxImputations <= 0 | maxImputations > 1000)stop("maxImputations must be between 1 and 1000")
    
    
  }else if(request == "maxImputationsInQueue"){
    if(!exists("maxImputationsInQueue")){
      maxImputationsInQueue<-200
      if(verbose>0)print(paste0(Sys.time(),": variable maxImputationsInQueue not found in configuration.R. Setting to default value of 200"))
    }
    if(!is.numeric(maxImputationsInQueue))stop("maxImputationsInQueue not numeric")
    if(length(maxImputationsInQueue)!=1)stop("maxImputationsInQueue not length 1")
    if(maxImputationsInQueue <= 0 | maxImputationsInQueue > 100000)stop("maxImputationsInQueue must be between 1 and 100000")
    
  }else if(request == "serverRole"){
    if(!exists("serverRole")){
      serverRole<-"Hub"
      if(verbose>0)print(paste0(Sys.time(),": variable serverRole not found in configuration.R. Setting to default value of 'Hub'"))
    }
    if(!is.character(serverRole))stop("serverRole not character")
    if(length(serverRole)!=1)stop("serverRole not length 1")
    if(!serverRole%in%c("Hub","Node"))stop("serverRole not Hub or Node")
    
  }else if(request == "hubAddress"){
    if(!exists("hubAddress")){
      if(!exists("serverRole") ||   serverRole=="Node"){
        stop("variable hubAddress was not found, and serverRole was set to Node. It is not allowed to default hubAdress when serverRole is set to Node")
      }else{
        hubAddress<-""
        if(verbose>0)print(paste0(Sys.time(),": variable hubAddress not found in configuration.R. Setting to default value of ''"))
      }
    }
    if(!is.character(hubAddress))stop("hubAddress not character")
    if(length(hubAddress)!=1)stop("hubAddress not length 1")
    
  }else if(request == "from_email_password"){
    if(!exists("from_email_password")){
      if(!exists("running_as_docker") ||   running_as_docker==FALSE){
        stop("variable from_email_password was not found, and running_as_docker was set to FALSE It is not allowed to default from_email_password when running_as_docker is set to FALSE")
      }else{
        from_email_password<-""
        if(verbose>0)print(paste0(Sys.time(),": variable from_email_password not found in configuration.R. This is used to log into an email-sender. Setting to default value of '', which means no emails will be sent out"))
      }
    }
    if(!is.character(from_email_password ))stop("from_email_password  not character")
    if(length(from_email_password )!=1)stop("from_email_password  not length 1")
    
  }else if(request == "from_email_address"){
    if(!exists("from_email_address")){
      if(!exists("running_as_docker") ||   running_as_docker==FALSE){
        stop("variable from_email_address was not found, and running_as_docker was set to FALSE It is not allowed to default from_email_address when running_as_docker is set to FALSE")
      }else{
        from_email_address<-""
        if(verbose>0)print(paste0(Sys.time(),": variable from_email_address not found in configuration.R. This is used to log into an email-sender. Setting to default value of '', which means no emails will be sent out"))
      }
    }
    if(!is.character(from_email_address))stop("from_email_address not character")
    if(length(from_email_address)!=1)stop("from_email_address not length 1")
    # str_match(from_email_address,"^[[:alnum:].-_]+@[[:alnum:].-]+$")
    
  }else if(request == "routinely_delete_this"){
    if(!exists("routinely_delete_this")){
      routinely_delete_this<-c("link","data")
      if(verbose>0)print(paste0(Sys.time(),": variable routinely_delete_this not found in configuration.R. Setting to default value of c('link','data'), meaning all genome-wide data will be deleted after 14 days."))
    }
    if(!is.character(routinely_delete_this))stop("routinely_delete_this not character")
    
  }else if(request == "paypal"){
    if(!exists("paypal")){
      paypal<-""
      if(verbose>0)print(paste0(Sys.time(),": variable paypal not found in configuration.R. Setting to default value of ''. This means that the email-sent paypal link won't work, but everything else will work fine."))
    }
    if(!is.character(paypal))stop("paypal not character")
    if(length(paypal)!=1)stop("paypal not length 1")
    
  }else if(request == "bulk_node_count"){
    if(!exists("bulk_node_count")){
      bulk_node_count<-1
      if(verbose>0)print(paste0(Sys.time(),": variable bulk_node_count not found in configuration.R. Setting to default value of 1"))
    }
    if(!is.numeric(bulk_node_count ))stop("bulk_node_count not numeric")
    if(length(bulk_node_count)!=1)stop("bulk_node_count not length 1")
    if(bulk_node_count <= 0 | bulk_node_count > 100)stop("bulk_node_count must be between 1 and 100")
    
  }else if(request == "error_report_mail"){
    if(!exists("error_report_mail")){
      error_report_mail<-""
      if(verbose>0)print(paste0(Sys.time(),": variable error_report_mail not found in configuration.R. Setting to default value of 0. This means that even if email is configured, no error-reports will be sent out."))
    }
    if(!is.character(error_report_mail))stop("error_report_mail not character")
    if(length(error_report_mail)!=1)stop("error_report_mail not length 1")
    
  }else if(request == "seconds_wait_before_start"){
    if(!exists("seconds_wait_before_start")){
      seconds_wait_before_start<-0
      if(verbose>0)print(paste0(Sys.time(),": variable seconds_wait_before_start not found in configuration.R. Setting to default value of 0"))
    }
    if(!is.numeric(seconds_wait_before_start))stop("seconds_wait_before_start not numeric")
    if(length(seconds_wait_before_start)!=1)stop("seconds_wait_before_start not length 1")
    
  }else if(request == "running_as_docker"){
    if(!exists("running_as_docker")){
      running_as_docker<-FALSE
      if(verbose>0)print(paste0(Sys.time(),": variable running_as_docker not found in configuration.R. Setting to default value of FALSE"))
    }
    if(!is.logical(running_as_docker))stop("running_as_docker not logical")
    if(length(running_as_docker)!=1)stop("running_as_docker not length 1")
    
  }else if(request == "max_imputation_chunk_size"){
    if(!exists("max_imputation_chunk_size")){
      max_imputation_chunk_size<-3000
      if(verbose>0)print(paste0(Sys.time(),": variable max_imputation_chunk_size not found in configuration.R. Setting to default value of 3000"))
    }
    if(!is.numeric(max_imputation_chunk_size))stop("max_imputation_chunk_size not numeric")
    if(length(max_imputation_chunk_size)!=1)stop("max_imputation_chunk_size not length 1")
  }else{
    stop(paste("Unknown request:",request))
  }  
  #export the request
  return(get(request))
}


  


prepare_individual_genome<-function(
  path, 
  email=NULL, 
  updateProgress=NULL, 
  protect_from_deletion=FALSE, 
  filename=NULL,
  predefined_uniqueID=NULL,
  overrule_vcf_checks=FALSE
){
  #' prepare individual genome
  #' 
  #' This is the main data-receiving function. It performs all checks that can be made
  #' in web-speed, meaning maximum a few seconds. If these are passed, the file, plus
  #' meta-information is saved in the queueing area, for processing and further checks.
  #' In web-running this function is called in the www.impute.me/imputeme interface,
  #' and will immediately decide if a file-submission is accepted or not. It is also the
  #' main function for accessing submissions programatically e.g. in docker running-
  #' 
  #' @param path The path to an input file. Can be either microarray format or vcf.
  #' @param email The user-email to report back to. Optional.
  #' @param updateProgress The progress update tracker for the shiny interface. Optional.
  #' @param protect_from_deletion  A switch carried down the analysis, that can be set as TRUE for debugging purposes. When TRUE, final results will not be deleted after 14 days, and less cleaning of part-calculations will be done.
  #' @param filename An optional filename of the sample. This is useful to have separate for e.g. shiny file submission, where it otherwise may be called e.g. /tmp/RtmpAKCi8i/9527843b29213cdef70532ff/0.gz. If it is not set, it will default to basename(path).
  #' @param predefined_uniqueID An optional pre-defined uniqueID. This is only used when accessing the function from outside of the usual web-upload interface, and bypasses the random-ID step. Still has to obey the usual 12-digit alphanumeric rules.
  #' @param overrule_vcf_checks A logical indicating if the (strict) vcf-files should be bypassed. Don't do this when accepting web-input, since you get a lot of low-pass and exon-seq vcfs then. But can be done in a controlled environment.
  #' 
  #' @return A short text string with a ready message. Additionally, the file provided as path will be available for processing in the standard-file structure, either in ~/imputations or in ~/vcfs as relevant.
  
  #loading libraries
  library("mailR")
  library("rJava")
  library("tools")
  suppressWarnings(library("shiny"))
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  #check that input path is ok and accesible
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  if(substr(path,1,1)!="/")path<-normalizePath(path)
  
  #Check filename is ok and set to basename(path) if not given
  #the actual file will anyway be renamed as <uniqueID>_raw_data.txt so it is not an 
  #essential variable (except the file-ending), but it does need to be cleaned for special characters 
  #for storing and mailing back to users
  if(is.null(filename))filename<-basename(path)
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be lengh 1, not",length(filename)))
  filename<-gsub("\\ ","_",filename)
  filename<-gsub("[\\$\\&\\+\\,\\:\\;\\=\\?\\@\\#\\\"\\\']","",filename)
  
  #check if this sample should be protected_from_deletion
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be lengh 1, not",length(protect_from_deletion)))
  
  #check the updateProgress object - set to a NULL-returning function if not given.
  if(is.null(updateProgress))updateProgress<-function(detail,value,max){return(NULL)}
  if(class(updateProgress)!="function")stop(paste("updateProgress must be function, not",class(updateProgress)))
  
  #check the user-inputted email, set to the default error_report_mail if not given
  if(is.null(email))email<-get_conf("error_report_mail")
  if(class(email)!="character")stop(paste("email must be character, not",class(email)))
  if(length(email)!=1)stop(paste("email must be lengh 1, not",length(email)))
  if(!get_conf("running_as_docker")){
    if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
      stop(safeError(paste("a real email adress is needed:",email)))
    }
  }  
  
  
  #updating progress
  updateProgress(detail = "Check mail, check queue, check md5sum",value=1,max=4)
  
  
  #check for too many ongoing imputations
  if(verbose>0)print(paste0(Sys.time(),": Checking for too many ongoing imputations"))
  s<-list.files("/home/ubuntu/imputations/")
  if(length(grep("^imputation_folder",s)) >= get_conf("maxImputationsInQueue")){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_many_jobs",email,length(grep("^imputation_folder",s)))
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    
    stop(safeError(paste("Too many imputations are already in progress. Cannot start a new one. The only solution to this is to wait a few days until the queues are shorter.")))
  }
  
  #check for vcf file
  if(length(grep("\\.vcf\\.gz$",filename))==1 | length(grep("\\.vcf$",filename))==1){
    is_vcf_file <- TRUE
  }else{
    is_vcf_file<-FALSE
  }
  
  
  #Create uniqueID and check that it is unique
  if(is.null(predefined_uniqueID)){
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
  }else{ #if pre-supplied, check that is according to ID-rules.
    uniqueID<-predefined_uniqueID
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
  }
  if(uniqueID%in%list.files("/home/ubuntu/data/")){  #Also check for pre-existing uniqueIDs and stop if so (never happened though)
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    stop(safeError("Problem with unique ID generation. Please re-load and try again."))
  }
  if(verbose>0)print(paste0(Sys.time(),": Created uniqueID ",uniqueID))
  
  
  
  
  #This blocks prepares a microarray-based sample directly for imputation
  #the next if-block will instead prepare a vcf-based sample in a separate vcf-area
  if(!is_vcf_file){
    
    #create imputation folder.
    if(verbose>0)print(paste0(Sys.time(),": Create imputation folder for ",uniqueID))
    homeFolderShort<-paste("imputation_folder",uniqueID,sep="_")
    if(!file.exists("/home/ubuntu/imputations/"))dir.create("/home/ubuntu/imputations/")
    homeFolder<-paste("/home/ubuntu/imputations/",homeFolderShort,"/",sep="")
    dir.create(homeFolder)
    setwd(homeFolder)
    write.table("Job is not ready yet",file="job_status.txt",col.names=F,row.names=F,quote=F)
    
    
    #unzipping (or not) and moving to new place
    newTempPath <- paste(homeFolder,paste(uniqueID,"_raw_data",sep=""),sep="/")
    newUnzippedPath <- paste(homeFolder,paste(uniqueID,"_raw_data.txt",sep=""),sep="/")
    file.copy(path, newTempPath)	
    gunzipResults<-unzip(newTempPath,exdir=homeFolder)
    if(length(gunzipResults)==1){ #then its a zip file
      if(verbose>0)print(paste0(Sys.time(),": Unzipping and moving to new place"))
      file.rename(gunzipResults, newUnzippedPath)		
    }else{ #then it's probably not
      #check if it is a gz file
      if(verbose>0)print(paste0(Sys.time(),": Handling non-zip file and moving to new place"))
      filetype<-system(paste("file ", newTempPath),intern=T)
      if(length(grep("gzip compressed",filetype))==1){
        
        
        #if it's a gzip file we generally fail them, because it contains the most
        #odd custom formats. However we have to allow for FTDNA uploads, because
        #since 2020-10-20 they started using .gz as default
        if(length(grep("autosomal_o37",filename,ignore.case = T))>0){
          zcat_result<-system(paste0("zcat ", newTempPath, " | sed 's/,/\\t/g' | grep '^rs' > ",newUnzippedPath),intern=T)
          if(length(zcat_result)!=0){
            if(!protect_from_deletion)unlink(homeFolder,recursive=T)
            m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"gzip_ftdna_problem",email,uniqueID,filename)
            m<-paste(m,collapse="\t")
            write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
            stop(safeError("This file looked like a FTDNA file but could not accurately be handled as such. This is a rare error, but we unfortunately cannot proceed."))
          }
          
          #if it's a non FTDNA gz file we fail it
        }else{
          if(!protect_from_deletion)unlink(homeFolder,recursive=T)
          m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"gzip_file",email,uniqueID,filename)
          m<-paste(m,collapse="\t")
          write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
          stop(safeError("Don't submit gz-files. Only uncompressed text or zip-files. If you already know what a gz file is, this should be easy for you. Please format as tab separated text files."))
        }
      }else{
        #otherwise just rename
        if(verbose>0)print(paste0(Sys.time(),": Moving file to new place"))
        file.rename(newTempPath, newUnzippedPath)		
      }
    }
    path <- newUnzippedPath
    
    
    
    #checking if it is a consistent file
    if(verbose>0)print(paste0(Sys.time(),": checking if it is a consistent file"))
    testRead<-try(read.table(path,nrow=10,stringsAsFactors=F))
    if(class(testRead)=="try-error"){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"general_data_file_problem",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)	
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't seem like genomic data at all. It must contain many rows, one per SNP, with information about your genotype. Please write an email if you think this is a mistake and that this file format should be supported."))
    }
    
    
    #updating progress
    updateProgress(detail = "Consistency checks",value=2,max=4)
    
    
    #checking if there is at least 10k lines (otherwise imputation wouldn't be possible anyway)
    cmd1 <- paste0("wc -l ",path)
    lines<- as.numeric(sub(" .+$","",system(cmd1,intern=T)))
    if(lines < 10000){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_few_lines_error",email,uniqueID)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("Your file only had ",lines," lines. That doesn't look like a genome-wide microarray input file. Genome-wide microarray files have many formats and come from many places (23andme, myheritage, ancestry, geneplaza, etc), but they always have hundreds of thousands of measurements")))
    }
    
    #running the alternative format converters
    if(ncol(testRead)==5){
      #This could be an ancestry.com file. Check that first
      testRead2<-read.table(path,nrow=10,stringsAsFactors=F,header=T)
      if(unique(sub("[0-9]+$","",testRead2[,1])[1])!="rs"){
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"ancestry_problem",email,uniqueID)
        m<-paste(m,collapse="\t")
        write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)	
        if(!protect_from_deletion)unlink(homeFolder,recursive=T)
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
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"reformat_error",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't seem to match any of our import algorithms. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
    }
    
    
    #after reformat attempts, perform one more test read and consider
    testRead2<-read.table(path,nrow=10,stringsAsFactors=F)
    if(ncol(testRead2)!=4){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_4_columns",email,uniqueID)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't have 4 columns (or 5 for ancestry.com data). If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
    }
    if(unique(sub("[0-9]+$","",testRead2[,1])[1])!="rs"){
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_no_rs_id",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't have rs IDs in column 1. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
    }
    
    
    
    
    
    #This block prepares a vcf file in a separate vcf-file holding area for later processing
  }else if(is_vcf_file){
    #create vcf folder
    if(verbose>0)print(paste0(Sys.time(),": create vcf folder for ",uniqueID))
    homeFolderShort<-paste("vcf_folder",uniqueID,sep="_")
    if(!file.exists("/home/ubuntu/vcfs/"))dir.create("/home/ubuntu/vcfs/")
    homeFolder<-paste("/home/ubuntu/vcfs/",homeFolderShort,"/",sep="")
    dir.create(homeFolder)
    setwd(homeFolder)
    write.table("Job is not ready yet",file="job_status.txt",col.names=F,row.names=F,quote=F)

    
    #unzipping (or not) and moving to new place
    if(verbose>0)print(paste0(Sys.time(),": unzipping (or not) and moving to new place"))
    newTempPath <- paste(homeFolder,paste(uniqueID,"_raw_data",sep=""),sep="/")
    newReadyPath <- paste(homeFolder,paste(uniqueID,"_raw_data.vcf.gz",sep=""),sep="/")
    file.copy(path, newTempPath)
    #file.rename(path, newTempPath) #better from space/speed perspective - but unstable across file-systems
    #check if it's gz file - if it is we juse it as-is
    if(substr(filename, nchar(filename)-2,nchar(filename)) == ".gz"){
      filetype<-system(paste("file ", newTempPath),intern=T)
      if(length(grep("gzip",filetype))==1){
        file.rename(newTempPath, newReadyPath)
      }else{ 
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"gzip_wrong_file",email,uniqueID,filename)
        m<-paste(m,collapse="\t")
        write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
        if(!protect_from_deletion)unlink(homeFolder,recursive=T)
        stop(safeError("The submitted file ended in .gz, but was determined to not be gzipped file. It could therefore not be processed and have been deleted."))
      }
    }else{
      #otherwise we bgzip it
      cmd<-paste0("bgzip ",newTempPath)
      system(cmd)
      file.rename(paste0(newTempPath,".gz"), newReadyPath)
    }
    path <- newReadyPath
    
    
    #updating progress
    updateProgress(detail = "Consistency checks",value=2,max=4)
    
    
    
    #checking if it is a consistent file
    if(verbose>0)print(paste0(Sys.time(),":checking if it is a consistent file"))
    testRead<-try(read.table(path,nrow=100,stringsAsFactors=F))
    if(class(testRead)=="try-error"){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"general_data_file_problem_vcf",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't seem like genomic data at all. The filename ended in vcf, so we expected a vcf file, but this did not seem to be the case."))
    }
    
    
    
    
    #check header
    testReadHeader<-try(readLines(path,n=250))
    if(!testReadHeader[1] %in% c("##fileformat=VCFv4.2","##fileformat=VCFv4.1")){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"wrong_starting_vcf_header",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("The first header line of your VCF didn't have exactly '##fileformat=VCFv4.2'. It had ",testReadHeader[1],". This is required, not because these exact columns are important, but because the importer strives to avoid any custom-format imports at all. See the github issue #32 for further discussion (https://github.com/lassefolkersen/impute-me/issues/32). You may also try to follow the cookbook-recipe in this discussion in order to convert your file into microarray-like data, and then retry upload.")))
    }
    
    
    
    #check for check-up escape sentence and/or overrule_vcf_checks argument
    #can be anything in the header, e.g. 
    ###INFO=<ID=Something=I solemnly swear that this file is of good enough quality>
    escape_checks<-FALSE
    escape_sentences <- "I solemnly swear that this file is of good enough quality"
    for(escape_sentence in escape_sentences){
      if(length(agrep(escape_sentences, testReadHeader, max.distance = 0.1,ignore.case=T))){
        escape_checks<-TRUE
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"vcf_escape_sentence_used",email,uniqueID,filename)
        m<-paste(m,collapse="\t")
        write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
      }
    }
    if(overrule_vcf_checks)escape_checks<-TRUE
    
    
    #size check requirement
    #we used to have this as a line-count requirement, but the zcat path | wc -l call
    #is too slow. Instead it's set to at least 100 MB size, which is about half
    #an average Dante lab vcf file. We haven't established a firm lower 
    #bound on this, but it is logged for future fine-tuning.
    #
    size_requirement <- 104857600
    size <- file.info(path)[["size"]]
    if(size < size_requirement && !escape_checks){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_small_vcf_error",email,uniqueID,filename, size,size_requirement)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("Your vcf-file was too small to be accepted. Currently, we only accept vcf files that are larger than ",size_requirement/(1024*1024)," GB (gzipped). Your submitted data was ",signif(size/(1024*1024),3)," GB.  This is done to avoid accidentally processing exon-sequencing data, which would be a problem for most subsequent algorithms. They all require genome-wide data, because the vcf-handling is done without imputation. You can read more about why that is, at http://doi.org/10.13140/RG.2.2.34644.42883. You may also find some tips on converting exon-sequencing data to microarray-like data-format in github issue #32 https://github.com/lassefolkersen/impute-me/issues/32. After doing this, you may resubmit your file and it will be run through the imputation-algorithm. It is possible, although un-tested, that some useful information can be extraxted when using this approach.")))
    }
    
    
    
    
    
    #Try to establish genome build from header
    grch37_hits<-grep("grch37|hg19",testReadHeader,ignore.case = TRUE,value=TRUE)
    grch38_hits<-grep("grch38|hg38",testReadHeader,ignore.case = TRUE,value=TRUE)
    if(length(grch37_hits)>0 & length(grch38_hits)>0 ){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"vcf_both_builds_error",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("The vcf reading algorithm found references to both genome build grch37 and grch38 in the vcf header and was unsure how to proceed. Your data have been deleted. If you want to re-submit the data and try again, we recommend that you edit the header of your vcf file such that there are only references to either grch37/hg19 or grch38/hg38, as the case may be."))
    }
    
    #Can be inserted if you don't want to have grch38 blocks (but it's not a very good filter, only scans header)
    # if(length(grch38_hits)>0){
    #   m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"vcf_grch38_not_allowed",email,uniqueID,filename)
    #   m<-paste(m,collapse="\t")
    #   write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
    #   if(!protect_from_deletion)unlink(homeFolder,recursive=T)
    #   stop(safeError("The vcf reading algorithm determined this data to be from the GRCh38 genome build. We are sorry, but this genome build is not supported yet. In the meantime, you may look into doing a liftover to GRCh37 yourself using tools such as Picard LiftoverVcf. Just remember to delete any references to GRCh38 in the vcf-header if you do so."))
    # }
    
    
    
    #check that first line column 9 is consistent
    #There is a lot of fine-tuning going into this demand, here's some preliminary observations.
    # First: we dislike formats that don't have DP, since it prevents filtering out low-pass sequencing samples
    #
    # That's ok for companies like Dante lab (GT:AD:AF:DP:F1R2:F2R1:GQ:PL:GP:PRI:SB:MB) and Nebula (GT:AD:DP:GQ:PGT:PID:PL:PS)
    #
    # That's a problem for the following companies that only report the GT field:
    # genotek.ru, estonian biobank and sequencing.com ("ultimate DNA test kit"). This is a pity, since
    # it seems the data is of ok quality otherwise. Possibly one could write a GT-only
    # catching mechanism for them. For now we just fail them. At the benefit of not majorly
    # messing up some low-pass or exon-seq submission.
    #
    #
    allowed_formats <- c("GT:AD:AF:DP:F1R2:F2R1:GQ:PL:GP:PRI:SB:MB","GT:AD:DP:GQ:PGT:PID:PL:PS","GT:AD:DP:GQ:PL")
    if(!testRead[1,9] %in% allowed_formats  && !escape_checks){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"wrong_starting_vcf_line",email,uniqueID,filename,testRead[1,9])
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("The first line of the 'format'-column of your VCF  had ",testRead[1,9],". We currently only allow vcf-files with very specific formatting-requirements, corresponding to Dante labs and Nebula. This is necessary in order to avoid grave mistakes with custom-formatted e.g. exon sequencings and also to be able to test the read-depth (the DP format entry). See github issue #32 for further discussion. In this discussion you may also find suggestions on alternative approaches to submitting your file for processing. The currently allowed format-column entries include: ",paste(allowed_formats,collapse=", "))))
    }
    
    
    #check read-depth
    minimum_mean_depth_of_first_hundred_reads <- 10 #would be nice to have genome-wide average depth, since the first chunk of reads in testRead could be hard to sequence regions. But it's not possible to read the entire vcf in browsing-time, which is required for these format rejection-checks
    format<-try(strsplit(testRead[1,9],":")[[1]])
    depth_index <- try(which(format%in%"DP"))
    depths<-try(as.numeric(sapply(strsplit(testRead[,10],":"),function(x,depth_index){x[depth_index]},depth_index)))
    if(any(is.na(depths)) || class(depths) == "try-error" || mean(depths,na.rm=T) < minimum_mean_depth_of_first_hundred_reads  && !escape_checks){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_low_vcf_depth",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("The average depth (DP) entry of the first hundred lines of your vcf file was not at least ",minimum_mean_depth_of_first_hundred_reads," which is required. Low-coverage sequencing will not work in the down-stream algorithms.")))
    }
    
    
    
    #check preceding chr
    #This can be handled downstream, so let's just allow it.
    # if(length(grep("^chr",testRead[,1]))>0){
    #   m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"chr_prefix_error",email,uniqueID,filename,length(grep("^chr",testRead[,1])))
    #   m<-paste(m,collapse="\t")
    #   write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
    #   if(!protect_from_deletion)unlink(homeFolder,recursive=T)
    #   stop(safeError(paste0("The chromosome notation of this vcf file followed the chr1, chr2, chr3 style, rather than the 1, 2, 3 style. Obviously we would like to eventually support both, but currently only the 1, 2, 3, 4 style of chromosome naming is supported.")))
    # }
    
    
    
    #double check build version - done by expecting at least a few common SNP hits in the first 100 lines of the vcf
    # firstHundredNames<-paste(testRead[,1],testRead[,2],sep=":")
    #compare this to … ftp://ftp.ncbi.nlm.nih.gov/snp/organisms/human_9606_b151_GRCh37p13/VCF/common_all_20180423.vcf.gz
    # grch37<-read.table("common_all_20180423.vcf.gz",sep="\t",header=F,nrows=1000,stringsAsFactors=F)
    # grch37<-read.table("~/prs_dir/temp/common_all_20180423.vcf.gz",sep="\t",header=F,nrows=500,stringsAsFactors=F)
    # grch37_check_up_names<-paste(grch37[,1],grch37[,2],sep=":")
    # paste0("c('",paste(grch37_check_up_names,collapse="','"),"')")
    # grch37_check_up_names<-c('1:10177','1:10352','1:10616','1:11012','1:11063','1:13110','1:13116','1:13118','1:13273',
    #   '1:13284','1:13289','1:13380','1:13445','1:13453','1:13483','1:13550','1:14464','1:14599',
    #   '1:14604','1:14930','1:14933','1:15031','1:15089','1:15211','1:15241','1:15245','1:15260',
    #   '1:15274','1:15644','1:15774','1:15777','1:15790','1:15820','1:15849','1:15903','1:16071',
    #   '1:16127','1:16141','1:16142','1:16365','1:16542','1:16949','1:18643','1:18849','1:19391',
    #   '1:20131','1:30923','1:46285','1:46716','1:47159','1:47345','1:48174','1:48180','1:48327',
    #   '1:48328','1:49298','1:49315','1:49343','1:49554','1:49565','1:49988','1:49989','1:50891',
    #   '1:51047','1:51049','1:51050','1:51053','1:51427','1:51479','1:51714','1:51747','1:51751',
    #   '1:51762','1:51765','1:52185','1:52238','1:52253','1:53195','1:53234','1:54353','1:54438',
    #   '1:54490','1:54493','1:54564','1:54591','1:54639','1:54712','1:54712','1:54716','1:54763',
    #   '1:54815','1:54830','1:54945','1:55136','1:55164','1:55326','1:55330','1:55405','1:55416',
    #   '1:55427','1:55545','1:55852','1:56445','1:56586','1:56644','1:56829','1:56931','1:57095',
    #   '1:57107','1:57183','1:57260','1:57262','1:57264','1:57292','1:57463','1:58814','1:59040',
    #   '1:59108','1:59121','1:59504','1:60249','1:60293','1:60351','1:61115','1:61208','1:61543',
    #   '1:61578','1:61743','1:61920','1:61993','1:62024','1:62055','1:62094','1:62124','1:62156',
    #   '1:62157','1:62162','1:62509','1:62595','1:62617','1:62716','1:62777','1:62863','1:62970',
    #   '1:63002','1:63093','1:63134','1:63145','1:63148','1:63268','1:63286','1:63336','1:63381',
    #   '1:63437','1:63671','1:63680','1:63722','1:63735','1:63735','1:64649','1:64670','1:64931',
    #   '1:65009','1:65974','1:66219','1:66231','1:66269','1:66272','1:66381','1:66381','1:66390',
    #   '1:66435','1:66457','1:66461','1:66466','1:67107','1:67179','1:67181','1:67196','1:67223',
    #   '1:67224','1:67580','1:67631','1:67942','1:68082','1:68247','1:68362','1:68596','1:69428',
    #   '1:69496','1:69590','1:69594','1:69610','1:69635','1:69761','1:69808','1:69892','1:69897',
    #   '1:69899','1:70317','1:70351','1:70352','1:72297','1:72526','1:73093','1:73108','1:73490',
    #   '1:74790','1:74792','1:76260','1:76838','1:76854','1:76897','1:77501','1:77502','1:77706',
    #   '1:77726','1:77763','1:77866','1:77874','1:77886','1:78061','1:78942','1:79137','1:79188',
    #   '1:79277','1:79417','1:79629','1:79759','1:79819','1:79898','1:79911','1:79967','1:80054',
    #   '1:80094','1:80221','1:80232','1:80454','1:81031','1:81032','1:81260','1:81587','1:81590',
    #   '1:81654','1:82133','1:82163','1:82365','1:82509','1:82609','1:82610','1:82652','1:82957',
    #   '1:82961','1:82994','1:83084','1:83170','1:83450','1:83484','1:83514','1:83771','1:83866',
    #   '1:84139','1:84156','1:84183','1:84562','1:84618','1:84701','1:84734','1:84752','1:84763',
    #   '1:85022','1:85343','1:85431','1:85508','1:85622','1:85715','1:85892','1:86000','1:86028',
    #   '1:86065','1:86192','1:86331','1:86339','1:86947','1:86982','1:87021','1:87114','1:87259',
    #   '1:87360','1:87377','1:87409','1:87486','1:87647','1:87755','1:87956','1:87988','1:88144',
    #   '1:88169','1:88172','1:88177','1:88188','1:88236','1:88300','1:88316','1:88338','1:88356',
    #   '1:88362','1:88388','1:88429','1:88598','1:88619','1:88688','1:88696','1:88710','1:88762',
    #   '1:88767','1:88794','1:89328','1:89599','1:89654','1:89677','1:89744','1:89946','1:90051',
    #   '1:90061','1:90231','1:91119','1:91127','1:91190','1:91264','1:91340','1:91358','1:91421',
    #   '1:91515','1:91536','1:91551','1:91551','1:91581','1:91588','1:92633','1:92858','1:92875',
    #   '1:92926','1:93248','1:94476','1:94788','1:94961','1:94967','1:94996','1:95046','1:95083',
    #   '1:95440','1:96594','1:96642','1:98325','1:98618','1:98896','1:98946','1:99388','1:99671',
    #   '1:99687','1:99719','1:100676','1:100858','1:102990','1:103547','1:104186','1:104281','1:106027',
    #   '1:108030','1:108230','1:108351','1:108375','1:108411','1:108506','1:108681','1:108869','1:108929',
    #   '1:109503','1:113913','1:114828','1:115729','1:115746','1:116117','1:118588','1:118599','1:118630',
    #   '1:122872','1:125271','1:125957','1:126134','1:126349','1:127794','1:128447','1:128747','1:128751',
    #   '1:129010','1:129971','1:131837','1:131838','1:133110','1:133165','1:133198','1:133217','1:133268',
    #   '1:133433','1:133508','1:133855','1:134133','1:135000','1:135031','1:135094','1:135162','1:135163',
    #   '1:135195','1:135203','1:135265','1:135982','1:136113','1:136131','1:136418','1:136741','1:137093',
    #   '1:137978','1:138041','1:138348','1:138396','1:138484','1:138593','1:138781','1:138802','1:138817',
    #   '1:138829','1:138913','1:139060','1:139189','1:139294','1:139579','1:139929','1:158006','1:173052',
    #   '1:173710','1:174799','1:229913','1:230088','1:230105','1:231817','1:232449','1:232488','1:233092',
    #   '1:233438','1:233473','1:233476','1:233487','1:233515','1:233556','1:234235','1:234408','1:234441',
    #   '1:234639','1:234717','1:234784','1:235180','1:235218','1:235334','1:235452','1:235920','1:236004',
    #   '1:237505','1:237803','1:240436','1:243782','1:244209','1:244953','1:245382','1:247792','1:249275',
    #   '1:249276','1:249293','1:249352','1:249652','1:250191','1:250761','1:251627','1:251663','1:251688',
    #   '1:251819','1:252807','1:254047','1:254186','1:254283','1:254313','1:254575','1:255402','1:255428',
    #   '1:255633','1:255847','1:255923','1:255923','1:256022')
    # found_in_check_up_names<-sum(firstHundredNames%in%grch37_check_up_names)
    # minimum_required_grch37_match<-10
    # if(found_in_check_up_names < minimum_required_grch37_match){
    #   m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"vcf_positional_coherence_check_error",email,uniqueID,filename,found_in_check_up_names,minimum_required_grch37_match)
    #   m<-paste(m,collapse="\t")
    #   write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
    #   if(!protect_from_deletion)unlink(homeFolder,recursive=T)
    #   stop(safeError(paste0("In a quick test read of the first 100 variants in the submitted vcf file, only ",found_in_check_up_names," of the variants were found to be at common GRCh37 positions. The required amount is ",minimum_required_grch37_match,". The submitted file is therefore likely to be from a non-supported genome build, or have other variant-position problems.")))
    # }
    
    
    
    
    
  }
  
  
  ##checking if this job has not actually been run before
  if(verbose>0)print(paste0(Sys.time(),": checking if this job has not actually been run before"))
  this_person_md5sum <- md5sum(path)
  all_md5sums_path<-"/home/ubuntu/misc_files/md5sums.txt"
  if(!file.exists(all_md5sums_path)){write("md5sums",file="/home/ubuntu/misc_files/md5sums.txt")}
  all_md5sums<-read.table(all_md5sums_path,sep="\t",stringsAsFactors = F)[,1]
  if(this_person_md5sum %in% all_md5sums){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"md5sum_match",email,this_person_md5sum,filename)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    if(!protect_from_deletion)unlink(homeFolder,recursive=T)
    stop(safeError("A person with this genome was already analyzed by the system. If this is an error, you can try to re-upload a new version of your DNA-data, e.g. you can go to your data provider (23andme, ancestry.com) and re-download the data you are interested. Then try that file. There will be no block flag for such new file, because any edits to the file will make the system unable to recognize it as a previously analyzed genome."))
  }
  write(this_person_md5sum,file="/home/ubuntu/misc_files/md5sums.txt",append=TRUE)			
  
  
  #finalizing: 
  #1) saving the small job_status.txt that just shows a status for the node/hub setup to read quickly over ssh
  #2) saving the variables.rdata which is a file of processing-specific parameters that is needed in first process, but afterwards deleted
  if(verbose>0)print(paste0(Sys.time(),": Finalize"))
  imputemany_upload <- FALSE
  should_be_imputed <- TRUE
  upload_time<-format(Sys.time(),"%Y-%m-%d-%H-%M-%S")
  save(uniqueID,email,filename,protect_from_deletion,is_vcf_file,imputemany_upload,should_be_imputed,upload_time,file=paste(homeFolder,"variables.rdata",sep=""))
  unlink("job_status.txt")
  write.table("Job is ready",file="job_status.txt",col.names=F,row.names=F,quote=F)
  
  
  #updating progress
  updateProgress(detail = "Unzipping, sending receipt mail",value=3,max=4)
  
  
  
  #Send off a mail as a receipt of data
  queue_length <- length(list.files("/home/ubuntu/imputations/"))
  if(is_vcf_file){
    algorithm_name <- "a vcf-extraction algorithm"
  }else{
    algorithm_name <- "an imputation algorithm"
  } 
  message_start <-paste0("<HTML>We received your data from file <i>", filename,"</i> at www.impute.me. It will now be processed, first through ",algorithm_name," and then through several types of genetic-risk score calculators. This takes a little less than a day per genome.")
  if(queue_length > 50){
    run_time <- 0.75 #days (was 1.6 with t2.medium, but now on c5a.large it has dropped to 0.75 days)
    servers_running <- get_conf("bulk_node_count")  #servers
    genomes_per_server <- 10 #genomes
    genomes_per_day <- servers_running * genomes_per_server / run_time
    days_left <- queue_length / genomes_per_day
    days_left_lower <- round(days_left*1.2) #because always have at least some paid ones skipping the line
    days_left_upper <- round(days_left*2.5)
    queue_message<-paste0(" Currently ",queue_length," other genomes are waiting in queue, so expect approximately ",days_left_lower,"-",days_left_upper," days of waiting.")
  }else if(queue_length > 5){
    queue_message<-paste0(" Currently ",queue_length," other genomes are waiting in queue, so expect several days of waiting.")
  }else{
    queue_message<-""
  }
  message_end1 <- paste0(" The service is non-profit, but because of heavy computing-requirements for the imputation analysis it is not free to run. We therefore strongly encourage you to pay a contribution to keep the servers running (<u><a href='",get_conf("paypal"),"'>paypal</a></u>, suggested 5 USD). Also, if you do this and put your unique ID, (<i>",uniqueID,"</i>) as payment-message, you'll be moved to priority queue. Either way, once the analysis is finished you'll receive a mail containing download links for the imputed data. You will also be able to browse the analytics-interface using this uniqueID.<br><br> ")
  
  #assign the right language book to people (Swedes and Norwegians can get the Danish language one too)
  if(length(grep("\\.dk$",email))==1 | length(grep("\\.no$",email))==1 | length(grep("\\.se$",email))==1){
    booklink<-"https://www.saxo.com/dk/forstaa-dit-dna_lasse-westergaard-folkersen_haeftet_9788770170154"
  }else{
    booklink<-"https://www.worldscientific.com/worldscibooks/10.1142/11070"
  }
  message_end2 <- paste0("In the meantime, may we recommend the book <u><a href='",booklink,"'>'Understand your DNA'</a></u> that serves as a guide for the underlying concepts of this analysis.<br></HTML>")
  
  #send the mail
  message <- paste0(message_start,queue_message,message_end1,message_end2)
  mailingResult<-try(send.mail(from = get_conf("from_email_address"),
                               to = email,
                               subject = "Imputation is queued",
                               body = message,
                               html=T,
                               smtp = list(
                                 host.name = "smtp.gmail.com", 
                                 port = 465, 
                                 user.name = get_conf("from_email_address"), 
                                 passwd = get_conf("from_email_password"), 
                                 ssl = TRUE),
                               authenticate = TRUE,
                               send = TRUE),silent=T)
  if(class(mailingResult)=="try-error"){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"mailing_error",email,uniqueID,filename)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
  }
  
  
  #if running as docker, then kill previous supercronic jobs and restart
  if(get_conf("running_as_docker")){
    processes<-system("ps -e",intern=T)
    processes<-do.call(rbind,strsplit(processes," +"))
    if("supercronic" %in% processes[,5]){
      pids<-processes[processes[,5]%in%"supercronic",2]
      for(pid in pids){
        if(verbose>1)print(paste0(Sys.time(),": Killing previous supercronic process: ",pid," (only need one)"))
        system(paste("kill",pid))
        Sys.sleep(0.1)
      }
    }
    if(!file.exists("/home/ubuntu/misc_files/supercronic.txt"))stop(safeError("Code was found to be running as docker container, but with no misc_files/supercronic.txt file ready for launch. The job will likely never execute."))
    #it's possible that the ending ampersand is not a good idea. The problem is that when the
    #docker is running as a web-interface it works well. But when the function is called from outside of
    #the docker, with docker exec - then supercronic deletes itself after a few seconds. It can be
    #kept with e.g. a Sys.sleep() argument. But that would destroy the feedback on the website (port 3838-based)
    #and there's no easy way to tell which place invoked the command. So right now the web
    #interface "wins" because that's for more casual users. Then super users can
    #separately call and start the supercronic
    supercronic_out<-try(system("supercronic /home/ubuntu/misc_files/supercronic.txt &"))
    if(supercronic_out != 0)stop(safeError("Code was found to be running as docker container, but gave an error when trying to start supercronic. The job will likely not start"))
  }
  
  
  #end with a message. In docker the message includes uniqueID, in web-running it does not (needs email absolutely)
  if(get_conf("running_as_docker")){
    return(paste("Genome files succesfully submitted. <b>The processing of your genome will take several days to run</b>. Typically between 1 and 5 days, depending on server-queue. When the processing is finished you will receive an email to",email,"with uniqueID and download-instructions. Look in your spam filter if not. The uniqueID of this run is <b>", uniqueID,"</b> -  you can close this browser window."))
    
  }else{
    return(paste("Genome files succesfully submitted. <b>The processing of your genome will take several days to run</b>. Typically between 1 and 5 days, depending on server-queue. When the processing is finished you will receive an email to",email,"with uniqueID and download-instructions. Look in your spam filter if not. You can close this browser window."))
  }
  
    
  
}








prepare_imputemany_genome<-function(
  path, 
  email=NULL, 
  updateProgress=NULL,
  should_be_imputed=TRUE, 
  protect_from_deletion=FALSE,
  filename=NULL
){
  #' prepare imputemany genome
  #' 
  #' This is the data-receiving function for batch uploads, performing the 
  #' same function as prepare_individual_genome, only for many genomes
  #' at the same time. Like prepare_individual_genome, it will perform in 
  #' web-speed, meaning a few seconds (more slow than individual genome 
  #' handling though). If these checks are passed, files converted to 
  #' individual genomes, plus meta information are saved in the queueing area, 
  #' for further processing and checks.
  #' 
  #' @param path The path to an input file
  #' @param email The user-email to report back to
  #' @param updateProgress The progress update tracker for the shiny interface
  #' @param protect_from_deletion A switch carried down the analysis, that can be set as TRUE for debugging purposes
  #' @param filename The optional filename of the sample (useful to have separate for e.g. shiny file submission). If not set it will default to basename(path).
  #' 
  #' @return A short text string with a completion message. In addition all submitted samples will be available for processing in the standard impute-me file-structure, at ~/imputations
  
  
  library("mailR")
  library("tools")
  suppressWarnings(library("shiny"))
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  #check path
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  #Check filename is ok - set to basename(path) if not given
  if(is.null(filename))filename<-basename(path)
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be lengh 1, not",length(filename)))
  
  #check if this sample should be protected_from_deletion
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be lengh 1, not",length(protect_from_deletion)))
  
  
  #check the updateProgress object - set to a NULL-returning function if not given.
  if(is.null(updateProgress))updateProgress<-function(detail,value,max){return(NULL)}
  if(class(updateProgress)!="function")stop(paste("updateProgress must be function, not",class(updateProgress)))
  
  
  #check the user-inputted email, set to the default error_report_mail if not given
  if(is.null(email))email<-get_conf("error_report_mail")
  if(class(email)!="character")stop(paste("email must be character, not",class(email)))
  if(length(email)!=1)stop(paste("email must be lengh 1, not",length(email)))
  if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
    stop(safeError(paste("a real email adress is needed:",email)))
  }
  
  #check if should_be_imputed is ok
  if(class(should_be_imputed)!="logical")stop(paste("should_be_imputed must be logical, not",class(should_be_imputed)))
  if(length(should_be_imputed)!=1)stop(paste("should_be_imputed must be lengh 1, not",length(should_be_imputed)))
  
  
  
  
  #check if mail adress is in positive list for bulk upload  
  acceptedMails_path<-"/home/ubuntu/misc_files/accepted_emails.txt"
  if(!file.exists(acceptedMails_path))stop(safeError("Configuration error: Email accepted-emails list not found"))
  acceptedMails<-read.table(acceptedMails_path,stringsAsFactors=F,header=T)
  if(!email%in%acceptedMails[,"email"] & !"any" %in% acceptedMails[,"email"]){ #bulk-upload must adhere to upload criteria
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"not_accepted_email",email,path)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    stop(safeError("Email was not in the accepted-emails list, and/or the entry 'any' was not found in the accepted emails list. Your data will not be processed and have already been deleted."))
  }
  if(should_be_imputed ){
    if(!"any" %in% acceptedMails[,"email"]){
      imputeok <- acceptedMails[acceptedMails[,"email"]%in%email,"imputeok"]
      if(!imputeok)stop(safeError("Email was in the accepted-emails list, but was FALSE for imputeok"))
    }
  }
  
  #set upload time
  upload_time<-format(Sys.time(),"%Y-%m-%d-%H-%M-%S")
  
  
  
  
  #check for too many ongoing imputations
  print("check for too many ongoing imputations")
  s<-list.files("/home/ubuntu/imputations/")
  if(length(grep("^imputation_folder",s)) >= get_conf("maxImputationsInQueue")){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_many_jobs",email,length(grep("^imputation_folder",s)))
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    
    stop(safeError(paste("Too many imputations are already in progress. Cannot start a new one. Limited server capacity was the reason for our kickstarter campaign. Supporters were first in line: kck.st/1VlrTlf")))
  }
  
  
  ##checking if this job has not actually been run before
  print("checking if this job has not actually been run before")
  this_person_md5sum <- md5sum(path)
  all_md5sums_path<-"/home/ubuntu/misc_files/md5sums.txt"
  if(!file.exists(all_md5sums_path)){write("md5sums",file="/home/ubuntu/misc_files/md5sums.txt")}
  all_md5sums<-read.table(all_md5sums_path,sep="\t",stringsAsFactors = F)[,1]
  if(this_person_md5sum %in% all_md5sums){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"md5sum_match",email,this_person_md5sum)
    m<-paste(m,collapse="\t")
    write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
    stop(safeError(paste0("This file was already analyzed by the system. Write an email if you wish to clear this flag (or make a small change in your file so it doesn't have the same md5sum, i.e. ",this_person_md5sum,").")))
  }
  write(this_person_md5sum,file="/home/ubuntu/misc_files/md5sums.txt",append=TRUE)			
  
  
  #check if it should be moved to the non-impute server:
  if(should_be_imputed){
    
    
    #unpacking
    if(!file.exists("/home/ubuntu/uploads_for_imputemany/"))dir.create("/home/ubuntu/uploads_for_imputemany/")
    newUnzippedPath <- paste0("/home/ubuntu/uploads_for_imputemany/",upload_time,"_input.txt")
    gunzipResults<-unzip(path,exdir="/home/ubuntu/uploads_for_imputemany/")
    gunzipResults<-grep("_MACOSX",gunzipResults,invert=T,value=T)
    if(length(gunzipResults)==1){ #then its a zip file
      file.rename(gunzipResults, newUnzippedPath)		
    }else if(length(gunzipResults)>1){
      stop(safeError("Don't submit zip files with more than one file in"))
    }else{ #then it's probably not
      #check if it is a gz file
      filetype<-system(paste("file ", path),intern=T)
      if(length(grep("gzip compressed",filetype))==1){
        stop(safeError("Don't submit gz-files. Only uncompressed text or zip-files. If you already know what a gz file is, this should be easy for you. Please format as tab separated text files."))
      }else{
        #otherwise just rename
        file.rename(path, newUnzippedPath)		
      }
    }
    path <- newUnzippedPath
    
    #updating progress
    updateProgress(detail = "Check mail, check queue, check md5sum")
    
    
    
    
    #checking if it is a consistent file
    print("checking if it is a consistent file")
    testRead<-try(read.table(path,nrow=10,stringsAsFactors=F,sep="\t"))
    if(class(testRead)=="try-error"){
      stop(safeError("Your file didn't seem like genomic data at all. It must contain many rows, one per SNP, with information about your genotype. Please write an email if you think this is a mistake and that this file format should be supported."))
    }
    
    
    
    
    #reading and checking
    print("checking if it is a consistent file")
    d<-try(read.table(path,stringsAsFactors=F,header=T,sep="\t"))
    
    if(class(d)=="try-error"){
      stop(safeError(paste("Problem with reading data. This was the error:",d)))
    }
    
    updateProgress(detail = "Check test-reads")
    
    
    #check header
    if(!all(colnames(d)[1:3] == c("RsID","Chr","Position")))stop(safeError("First three column headers must be RsID, Chr, and Position"))
    
    
    #check max-size
    if(ncol(d)>100){
      stop(safeError("The file contained more than 100 individuals. This is currently not allowed. It is very easy to lift the ceiling, however, it's just a precaution to not overload servers. Write an email if you need to upload more."))
    }
    
    
    #check sample names
    sampleNames<-colnames(d)[4:ncol(d)]
    if(length(sampleNames)<2)stop(safeError("Have to upload at least data with two samples"))
    
    #removing theempty ones RsIDs (have happened in several upload types)
    d<-d[d[,"RsID"] != "" & d[,"RsID"] != ".",]
    
    
    
    
    updateProgress(detail = "Check sample splits")
    
    
    
    #split the , separated
    splitLines <- grep(",",d[,"RsID"])
    if(length(splitLines)>0){
      print("have to split ,")  
      
      d0 <- d[0,]
      for(splitLine in splitLines){
        snps<-gsub(" ","",unlist(strsplit(d[splitLine,"RsID"],",")[[1]]))
        for(snp in snps){
          d0<-rbind(d0, d[splitLine,])
          d0[nrow(d0),"RsID"] <- snp
        }
      }
      d<-rbind(d[grep(",",d[,"RsID"],invert=T),],d0)
    }
    
    
    
    #check duplicate
    if(any(duplicated(d[,"RsID"]))){
      m<-unique(d[duplicated(d[,"RsID"]),"RsID"])
      if(length(m)>10)m<-c(m[1:10],"...")
      print(paste("Some SNPs were duplicated:",paste(m,collapse=", "),"Note if you want an automatic removal of duplicated SNP, just say, keep first occurence - then it's very easy to implement. Could be sharpened."))
      #But for now, just dump them
      d<-d[!duplicated(d[,"RsID"]),]
      updateProgress(detail = "Remove duplicate SNPs")
    }
    
    
    
    
    #check pos chr and sort
    expect_alphabetical<-c('1','10','11','12','13','14','15','16','17','18','19','2','20','21','22','3','4','5','6','7','8','9','MT','X','XY','Y')
    wanted_alphabetical<-c('1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21','22','X','Y','XY','MT')
    if(all(sort(unique(d[,"Chr"])) != wanted_alphabetical)){
      if(all(sort(unique(d[,"Chr"])) != expect_alphabetical)){
        stop(safeError(paste("Chromosomes were not all as expected. Expect all these to be present:",paste(expect_alphabetical,collapse=", "))))
      }
    }
    if(class(d[,"Position"])!="integer")stop(safeError(paste("Position expect to be only integers")))
    d<-d[order(factor(d[,"Chr"],levels=wanted_alphabetical),d[,"Position"]),]
    
    
    
    
    # Create uniqueID for each sample
    for(sampleName in sampleNames){
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
      print(paste("created uniqueID for",sampleName,"which is now called",uniqueID))
      names(sampleNames)[sampleNames%in%sampleName] <- uniqueID
      
      #check that the ID doesn't already exists (never happened though)
      if(uniqueID%in%list.files("/home/ubuntu/data/")){
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
        m<-paste(m,collapse="\t")
        write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)			
        stop(safeError("Problem with unique ID generation. Please re-load and try again."))
      }
      
    }
    
    #check and export each sample
    for(uniqueID in names(sampleNames)){
      sampleName <- sampleNames[uniqueID]
      
      #updating progress
      text <- paste0("Sample: ",sampleName," - ",which(names(sampleNames)%in%uniqueID)," of ",length(sampleNames))
      updateProgress(detail = text,max=length(sampleNames))
      
      
      #create imputation folder and output data folder
      print(paste("create imputation folder and output data folder for",uniqueID,"sample",sampleName))
      if(uniqueID%in%list.files("/home/ubuntu/data/")){
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
        m<-paste(m,collapse="\t")
        write(m,file="/home/ubuntu/logs/submission/submission_log.txt",append=TRUE)
        stop(safeError("Problem with unique ID generation. Please re-load and try again."))
      }
      
      homeFolder<-paste("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/",sep="")
      dir.create(homeFolder)
      write.table("Job is not ready yet",file=paste0(homeFolder,"job_status.txt"),col.names=F,row.names=F,quote=F)
      
      #check all is double digit
      if(length(unique(nchar(d[,sampleName])))!=1)stop(safeError(paste("All genotypes must be of two characters. This was not the case for",sampleName)))
      
      
      #export
      o<-data.frame(rsid=d[,"RsID"],chromosome=d[,"Chr"],position=d[,"Position"],genotype=d[,sampleName],stringsAsFactors=F)
      fileout_name <- paste0(homeFolder,uniqueID,"_raw_data.txt" )
      write.table(o,file=fileout_name, row.names=F,col.names=F, quote=F, sep ="\t")
      
      filename <- sampleName
      imputemany_upload <- TRUE
      save(uniqueID,email,filename,protect_from_deletion,sampleNames,upload_time,should_be_imputed,imputemany_upload,file=paste(homeFolder,"variables.rdata",sep=""))
      unlink(paste0(homeFolder,"job_status.txt"))
      write.table("Job is ready",file=paste0(homeFolder,"job_status.txt"),col.names=F,row.names=F,quote=F)
      
    }
    
    
    #prepare write-out to bulk-impute register file 
    imputemany_registry_path <- "/home/ubuntu/misc_files/imputemany_registry.txt"
    if(!file.exists(imputemany_registry_path)){
      f<-file(imputemany_registry_path,"w")
      writeLines(paste(c("upload_time","has_been_sent","error_sent","length","email","uniqueIDs"),collapse="\t"),f)
      close(f)
    }
    #headers are: upload time, has-been-sent,length,  error-sent, email, uniqueIDs
    registry_entry <-paste(c(upload_time,FALSE, FALSE, length(sampleNames),email, paste(names(sampleNames),collapse=",")),collapse="\t")
    write(registry_entry,file=imputemany_registry_path,append=TRUE)			
    
    
  }else{
    outfolder <- "/home/ubuntu/exports_for_imputemany/"
    if(!file.exists(outfolder))dir.create(outfolder)
    file.copy(path,paste0(outfolder,upload_time,".zip") )
    
  }  
  
  
  #always admin-mail a notification that an imputemany upload has happened
  mailingResult<-try(send.mail(from = get_conf("from_email_address"),to = get_conf("error_report_mail"),
                               subject = "Impute-many data set uploaded",
                               body = paste0("A data set with name ",upload_time," was uploaded to the server by ",email," (imputation was set to ",should_be_imputed,")"),
                               html=T,
                               smtp = list(host.name = "smtp.gmail.com",port = 465,user.name = get_conf("from_email_address"),passwd = get_conf("from_email_password"),ssl = TRUE),authenticate = TRUE,send = TRUE))
  
  
  
  #if running as docker, then kill previous supercronic jobs and restart
  if(get_conf("running_as_docker")){
    processes<-system("ps -e",intern=T)
    processes<-do.call(rbind,strsplit(processes," +"))
    if("supercronic" %in% processes[,5]){
      pids<-processes[processes[,5]%in%"supercronic",2]
      for(pid in pids){
        print(paste("Killing previous supercronic process:",pid,"(only need one)"))
        system(paste("kill",pid))
        Sys.sleep(0.1)
      }
    }
    if(!file.exists("/home/ubuntu/misc_files/supercronic.txt"))stop(safeError("Code was found to be running as docker container, but with no misc_files/supercronic.txt file ready for launch. The job will likely never execute."))
    supercronic_out<-try(system("supercronic /home/ubuntu/misc_files/supercronic.txt &"))
    if(supercronic_out != 0)stop(safeError("Code was found to be running as docker container, but gave an error when trying to start supercronic. The job will likely not start"))
  }
  
  
  
  
  #then send in interface (doesn't matter too much if an admin-email was sent or not, it'll run either way)
  return(paste("<b>Genome files succesfully submitted</b>. The processing of your genome will take some time to run. We will email you at",email,"with download-instructions during the next days."))
  
  
}







check_for_cron_ready_jobs<-function(
  job_type
){
  #' check for cron ready jobs
  #'
  #' Checks what jobs to run. In the simplest form, this function simply just returns 
  #' the uniqueID of the next jobs in line, sorted jobs in line, by submission-time 
  #' and mark that job as 'Job is running' in the job_status.txt. However, with further 
  #' complexities (Node-running, priority queue,  timing delay, vcf-jobs etc)
  #' this core function needs more complex handling, including transferring the 
  #' files from Hub to Node computer and marking them as remote-running in 
  #' job_status.txt, both on Hub and Node. The function also takes care of picking 
  #' first from ~/misc_files/fast_queue_emails.txt, the priority queue, as well
  #' as distinguishing if genomes marked as non-regular are allowed into bulk
  #' imputation pipelines.
  #'
  #' @param job_type Can be either single, bulk, or vcf - corresponding to the processing types.
  #' 
  #' @return one or more uniqueIDs ready for processing
  
  
  
  #get global variables
  serverRole <- get_conf("serverRole")
  hubAddress <- get_conf("hubAddress")
  verbose <- get_conf("verbose")
  seconds_wait_before_start<-get_conf("seconds_wait_before_start")
  
  
  
  #This block serves to space the runs some time apart. It is mostly relevant on e.g. t2.medium AWS instances
  #that can build up computing power. But it may also be useful in debugging situations.
  if(seconds_wait_before_start>0){
    #First checking if node is already at max load (maxImputations, set in configuration.R)
    foldersToCheck<-grep("^imputation_folder",list.files("/home/ubuntu/imputations/"),value=T)
    runningJobCount<-0
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
      if(file.exists(job_status_file)){
        job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
        if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
      }
    }
    #then stop job if runs are already running
    if(runningJobCount>(get_conf("maxImputations")-1)){
      stop(paste("Found",runningJobCount,"running jobs, which is more than maxImputations so doing nothing"))
    }else{
      if(verbose>0)print(paste0(Sys.time(),": Waiting ",seconds_wait_before_start/(60)," minutes before re-checking. Set seconds_wait_before_start variable in ~/misc_files/configuration.R if this is not ok."))
      Sys.sleep(seconds_wait_before_start)
    }
    #After sleeping period, wake up and re-check (equal to the seconds_wait_before_start=0 setting)
  }
  
  
  
  
  #Then, after optional-wait, we check if enough stuff is already running, and abort if so
  foldersToCheck<-grep("^imputation_folder",list.files("/home/ubuntu/imputations/"),value=T)
  runningJobCount<-0
  for(folderToCheck in foldersToCheck){
    job_status_file<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
    if(file.exists(job_status_file)){
      job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
      if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
    }
  }
  if(runningJobCount>(get_conf("maxImputations")-1)){
    stop(paste("Found",runningJobCount,"running jobs, which is more than maxImputations, so doing nothing"))
  }
  
  
  if(job_type=="single"){
    
    #If the computer is not too busy and the serverRole is node - we fetch ONE job (if it is hub, the jobs are already there)
    if(serverRole== "Node"){
      #sort checking order by time entered
      cmd1 <- paste("ssh ubuntu@",hubAddress," ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' /home/ubuntu/imputations/  | tail -n +2",sep="")
      remotedata<-system(cmd1,intern=T)
      Sys.sleep(0.2)
      remotedata_df<-as.data.frame(do.call(rbind,strsplit(remotedata,"\\s+")),stringsAsFactors=F)
      remotedata_df<-remotedata_df[order(remotedata_df[,6]),]
      remoteFoldersToCheck<-remotedata_df[,7]
      
      
      #check if there's any fast-queue jobs to put up-front. The fast-queue jobs is just a file with uniqueID
      #and then TRUE or FALSE. The TRUE or FALSE means if a bulk impute is allowed to take it or not
      #which is not relevant here in single-running.
      cmd0 <- paste("ssh ubuntu@",hubAddress," cat /home/ubuntu/misc_files/fast_queue_emails.txt
                ",sep="")
      f1<-system(cmd0,intern=T)
      Sys.sleep(0.2)
      if(length(f1)>0){ #if there is a fast-queue file, we handle it
        f2<-do.call(rbind,strsplit(f1,"\t"))
        f3<-f2[,1]
        remoteFoldersToCheck<-c(remoteFoldersToCheck[remoteFoldersToCheck%in%f3],remoteFoldersToCheck[!remoteFoldersToCheck%in%f3])
      }  
      
      #then loop over all remote folders
      for(remoteFolderToCheck in remoteFoldersToCheck){
        cmd2 <- paste("ssh ubuntu@",hubAddress," cat /home/ubuntu/imputations/",remoteFolderToCheck,"/job_status.txt",sep="")
        job_status<-system(cmd2,intern=T)
        #Check if the job is ready
        if(job_status=="Job is ready"){
          if(verbose>0)print(paste0(Sys.time(),": Found remote job-status file and job is ready ",remoteFolderToCheck," - will copy to local Node"))
          
          #First write to job-status that now the job is off to a remote server
          cmd3 <- paste("ssh ubuntu@",hubAddress," 'echo Job is remote-running > /home/ubuntu/imputations/",remoteFolderToCheck,"/job_status.txt'",sep="")
          system(cmd3)
          
          #then copy all the files to here
          cmd4 <- paste("scp -r ubuntu@",hubAddress,":/home/ubuntu/imputations/",remoteFolderToCheck," /home/ubuntu/imputations/",remoteFolderToCheck,sep="")
          system(cmd4)
          
          #Then write locally that job is ready
          job_status_file<-paste("/home/ubuntu/imputations/",remoteFolderToCheck,"/job_status.txt",sep="")
          unlink(job_status_file)
          write.table("Job is ready",file=job_status_file,col.names=F,row.names=F,quote=F)
          break
        }
      }
      #Update the local foldersToCheck to reflect new arrivals
      foldersToCheck<-grep("^imputation_folder",list.files("/home/ubuntu/imputations/"),value=T)
    }
    
    
    #Then - no matter the role - we check locally which, if any, folders are ready to run
    imputeThisFolder<-NA
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
      if(!file.exists(job_status_file)){
        if(verbose>0)print(paste0(Sys.time(),": Didn't find a job-status file - should probably auto-delete ",folderToCheck))
        next
      }
      job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
      if(job_status=="Job is not ready yet"){
        if(verbose>0)print(paste0(Sys.time(),": Found job-status file - but job is not ready yet ",folderToCheck))
        next
      }
      if(job_status=="Job is running"){
        if(verbose>0)print(paste0(Sys.time(),": Found job-status file - but job is already running ",folderToCheck))
        next
      }
      if(job_status=="Job is ready"){
        if(verbose>0)print(paste0(Sys.time(),": Found local job-status file and job is ready ",folderToCheck))
        unlink(job_status_file)
        write.table("Job is running",file=job_status_file,col.names=F,row.names=F,quote=F)
        imputeThisFolder<-folderToCheck
        break
      }
    }
    #Stop if none are found
    if(is.na(imputeThisFolder)){
      stop("No folders were found to be ready for imputation")
    }
    
    uniqueIDs<-sub("imputation_folder_","",imputeThisFolder)
    
  }else if(job_type =="bulk"){
    #bulk running parameter (never tested with anything else than 10)
    length_requested <- 10
    #If the computer is not too busy and the serverRole is node - we fetch 10 jobs
    if(serverRole== "Node"){
      #sort checking order by time entered
      cmd1 <- paste("ssh ubuntu@",hubAddress," ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' /home/ubuntu/imputations/  | tail -n +2",sep="")
      remotedata<-system(cmd1,intern=T)
      
      Sys.sleep(0.2) #And remember to pause if running interactive. It'll fail otherwise
      remotedata_df<-as.data.frame(do.call(rbind,strsplit(remotedata,"\\s+")),stringsAsFactors=F)
      if(ncol(remotedata_df)==0)stop("Nothing found at hub server. Exit, stop and wait.")
      remotedata_df<-remotedata_df[order(remotedata_df[,6]),]
      remoteFoldersToCheck<-remotedata_df[,7]
      
      
      #check if there's any fast-queue jobs to put up-front. The fast-queue jobs is just a file with uniqueID
      #and then TRUE or FALSE. The TRUE or FALSE means if a bulk impute is allowed to take it or not
      #(they can be in priority queue either because they are paid, or because they are error-prone. 
      #Don't put error-prone in the bulk imputing line)
      cmd0 <- paste("ssh ubuntu@",hubAddress," cat /home/ubuntu/misc_files/fast_queue_emails.txt
                ",sep="")
      f1<-system(cmd0,intern=T)
      Sys.sleep(0.2)
      if(length(f1)>0){ #if there is a fast-queue file, we handle it
        f2<-do.call(rbind,strsplit(f1,"\t"))
        
        #first prioritize the TRUE fast-queue entries (they are ok to run in bulk-impute)
        f3<-f2[f2[,2]%in%"TRUE",1,drop=FALSE]
        if(nrow(f3)>0){
          remoteFoldersToCheck<-c(remoteFoldersToCheck[remoteFoldersToCheck%in%f3],remoteFoldersToCheck[!remoteFoldersToCheck%in%f3])  
        }
        
        #then remove the FALSE fast-queue entries (they are NOT ok to run in bulk-impute)
        f4<-f2[f2[,2]%in%"FALSE",1,drop=FALSE]
        if(nrow(f4)>0){
          remoteFoldersToCheck<-remoteFoldersToCheck[!remoteFoldersToCheck%in%f4]
        }
      }
      
      #then loop over all remote folders - first checking if there is 'length_requested'  ready jobs
      remoteFoldersToRun <- vector()
      for(remoteFolderToCheck in remoteFoldersToCheck){
        if(length(remoteFoldersToRun)>=length_requested)break
        cmd2 <- paste("ssh ubuntu@",hubAddress," cat /home/ubuntu/imputations/",remoteFolderToCheck,"/job_status.txt",sep="")
        job_status<-system(cmd2,intern=T)
        #Check if the job is ready
        if(job_status=="Job is ready"){
          if(verbose > 1)print(paste0(Sys.time(),": Found remote job-status file and job is ready ",remoteFolderToCheck," - will copy to local"))
          remoteFoldersToRun <- c(remoteFoldersToRun,remoteFolderToCheck)
        }
      }
      
      #if exactly 'length_requested' - then we copy the files from hub to node
      if(length(remoteFoldersToRun)==length_requested){
        #Then write to job-status that now the job is off to a remote server
        for(remoteFolderToRun in remoteFoldersToRun){
          cmd3 <- paste("ssh ubuntu@",hubAddress," 'echo Job is remote-running > /home/ubuntu/imputations/",remoteFolderToRun,"/job_status.txt'",sep="")
          system(cmd3)
        }
        
        #then copy all the files to here
        for(remoteFolderToRun in remoteFoldersToRun){
          cmd4 <- paste("scp -r ubuntu@",hubAddress,":/home/ubuntu/imputations/",remoteFolderToRun," /home/ubuntu/imputations/",remoteFolderToRun,sep="")
          system(cmd4)
          
          #And write locally that job is ready
          job_status_file<-paste("/home/ubuntu/imputations/",remoteFolderToRun,"/job_status.txt",sep="")
          unlink(job_status_file)
          write.table("Job is ready",file=job_status_file,col.names=F,row.names=F,quote=F)
        }
        
        #Update the local foldersToCheck to reflect new arrivals
        # foldersToCheck<-grep("^imputation_folder",list.files("/home/ubuntu/imputations/"),value=T)
        #Update the local folder file, but keeping the original order to reflect priority runs
        foldersToCheck<-remoteFoldersToRun
      }
    }
    
    
    
    #Then - no matter the role - we check locally which, if any, folders are ready to run
    imputeThisFolder<-vector()
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
      
      if(!file.exists(job_status_file)){
        if(verbose>0)print(paste0(Sys.time(),": Didn't find a job-status file - should probably auto-delete ",folderToCheck))
        next
      }
      job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
      if(job_status=="Job is not ready yet"){
        if(verbose>0)print(paste0(Sys.time(),": Found job-status file - but job is not ready yet ",folderToCheck))
        next
      }
      if(job_status=="Job is running"){
        if(verbose>0)print(paste(Sys.time(),": Found job-status file - but job is already running ",folderToCheck))
        next
      }
      if(job_status=="Job is ready"){
        if(verbose>1)print(paste0(Sys.time(),": Found local job-status file and job is ready ",folderToCheck))
        unlink(job_status_file)
        write.table("Job is running",file=job_status_file,col.names=F,row.names=F,quote=F)
        imputeThisFolder<-c(imputeThisFolder,folderToCheck)
        
      }
    }
    
    
    #Stop if none are found
    if(length(imputeThisFolder)!= length_requested){
      stop(paste0(length(imputeThisFolder)," impute-queue folders were found to be ready for imputation. The requested number is ",length_requested,".")) #here we could easily have a mechanism for proceeded if the found number is <10, but for now we just fail so we can sort it out later. It's not applicable in node-running-setups anyway, because they always take 10.
    }else{
      print(paste0(Sys.time(),": Found ",length_requested," ready sets to impute"))
    }
    
    
    uniqueIDs<-sub("imputation_folder_","",imputeThisFolder)
    
    
  }else if(job_type =="vcf"){
    
    #Currently not tested for node-running. Only hub. Mainly because there has been no need, since the requirements are lower for a seq-handling
    if(get_conf("serverRole")== "Node"){
      stop("The vcf job_type has not been tested for node running at all, because it has been sufficient to run it as hub so far.")
    }
    
    #sort checking order by time entered
    cmd1 <- paste("ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' /home/ubuntu/vcfs/  | tail -n +2",sep="")
    localdata<-system(cmd1,intern=T)
    Sys.sleep(0.2)
    localdata_df<-as.data.frame(do.call(rbind,strsplit(localdata,"\\s+")),stringsAsFactors=F)
    if(nrow(localdata_df)==0)stop("No vcf-jobs found to be ready")
    localdata_df<-localdata_df[order(localdata_df[,6]),]
    foldersToCheck<-localdata_df[,7]
    
    #check if there's any fast-queue jobs to put up-front.
    # fast_queue_emails<-read.table("/home/ubuntu/misc_files/fast_queue_emails.txt",stringsAsFactors = F,sep="\t")
    # THIS IS NOT IMPLEMENTED YET, too rare
    
    
    runningJobCount<-0
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste("/home/ubuntu/vcfs/",folderToCheck,"/job_status.txt",sep="")
      if(file.exists(job_status_file)){
        job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
        if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
      }
    }
    if(runningJobCount>(get_conf("maxImputations")-1)){
      stop(paste("Found",runningJobCount,"running jobs, which is more than maxImputations so doing nothing"))
    }
    
    
    
    
    #Then - no matter the role - we check locally which, if any, folders are ready to run
    imputeThisFolder<-NA
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste("/home/ubuntu/vcfs/",folderToCheck,"/job_status.txt",sep="")
      if(!file.exists(job_status_file)){
        if(verbose>0)print(paste0(Sys.time(),": Didn't find a job-status file - should probably auto-delete ",folderToCheck))
        next
      }
      job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
      if(job_status=="Job is not ready yet"){
        if(verbose>0)print(paste0(Sys.time(),": Found job-status file - but job is not ready yet ",folderToCheck))
        next
      }
      if(job_status=="Job is running"){
        if(verbose>0)print(paste0(Sys.time(),": Found job-status file - but job is already running ",folderToCheck))
        next
      }
      if(job_status=="Job is ready"){
        if(verbose>0)print(paste0(Sys.time(),": Found local job-status file and job is ready ",folderToCheck))
        unlink(job_status_file)
        write.table("Job is running",file=job_status_file,col.names=F,row.names=F,quote=F)
        imputeThisFolder<-folderToCheck
        break
      }
    }
    #Stop if none are found
    if(is.na(imputeThisFolder)){
      stop("No folders were found to be ready for imputation")
    }
    
    
    uniqueIDs<-sub("vcf_folder_","",imputeThisFolder)
    
    
    
  }else{stop(paste("Unknown job_type:",job_type," - must be single, bulk or vcf"))}
  
  if(serverRole=="Hub"){
    if(verbose>0)print(paste0(Sys.time(),": Found ",length(uniqueIDs)," jobs of type ",job_type," and marked them as job_running."))
  }else if(serverRole=="Node"){
    if(verbose>0)print(paste0(Sys.time(),": Found ",length(uniqueIDs)," jobs of type ",job_type,", copied them to this Node and marked them as job_running on the Hub."))
  }
  
  return(uniqueIDs)
  
}






run_imputation<-function(
  uniqueID
){
  #' run imputation
  #' 
  #' This is the function that runs imputation on an individual genome (not bulk).
  #'It starts from the folder as prepared in the prepare_individual_genome 
  #'and ends with imputed, but not summarized data, typically for handling off 
  #' to summarize_imputation, and subsequent export functions. These are the exact 
  #' same steps as those performed by run_bulk_imputation, only on an individual genome,
  #' but there are considerable economical/computational gains by waiting for a full set 
  #' of genomes for a bulk-genome run. Therefore this function is typically used only
  #' for fast-turnaround time genomes, for tricky genomes to avoid a full set crashing,
  #' as well as for testing and development.
  #'
  #' @param uniqueID The uniqueID with a folder at ~/imputations/imputation_folder_<uniqueID>
  #' 
  #' @return No return value, but on completion the runDir will contain imputed chunks of genomic data.

  
  #load libraries
  library(tools)
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  #define program paths
  shapeit="/home/ubuntu/programs/shapeit.v2.904.3.10.0-693.11.6.el7.x86_64/bin/shapeit"
  plink="/home/ubuntu/programs/plink"
  impute2="/home/ubuntu/programs/impute_v2.3.2_x86_64_static/impute2"
  sample_ref="/home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
  
  
  #check uniqueID is ok
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
  
  
  #set runDir and check that it exists
  start_wd<-getwd()
  runDir <- paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID)
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)runDir <- sub("/$","",runDir) #remove trailing slash
  setwd(runDir)
  load(paste(runDir,"/variables.rdata",sep=""))
  rawdata<-paste(runDir,"/",uniqueID,"_raw_data.txt",sep="")

  #test that impute2 static can run (or switch to dynamic)
  impute2_blank_run_out <- suppressWarnings(system(impute2,intern=T,ignore.stderr = T) )
  if(attr(impute2_blank_run_out,"status")==139){
    impute2 <- "/home/ubuntu/programs/impute_v2.3.2_x86_64_dynamic/impute2"
    if(verbose>2)print(paste0(Sys.time(),": Problem with impute_v2.3.2_x86_64_static detected. Switching to impute_v2.3.2_x86_64_dynamic."))
  }
   
  #check other arguments 
  if(class(rawdata)!="character")stop(paste("rawdata must be character, not",class(rawdata)))
  if(length(rawdata)!=1)stop(paste("rawdata must be lengh 1, not",length(rawdata)))
  if(!file.exists(rawdata))stop(paste("Did not find rawdata at path:",rawdata))
  
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
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be lengh 1, not",length(verbose)))
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
  
  
  
  
  #need to always check if the genes_for_good_cleaner should be run
  if(length(grep("genes for good",tolower(readLines(rawdata,n=5)))>0)){
    genes_for_good_cleaner(uniqueID,runDir)
  }
  
  #print start message
  if(verbose>0)print(paste0(Sys.time(),": Starting imputation running on this file: ",uniqueID," - this may take 3-8 hours."))
  if(verbose>1)print(paste0(Sys.time(),": Good luck"))
  
  
  #Load data using plink 1.9
  cmd1<-paste0(plink," --23file ",rawdata," ",uniqueID," ",uniqueID," --recode --out step_1")
  out1<-system(cmd1,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
  
  
  #If the standard command fails, we run an extensive error rescue. Hopefully shouldn't be used too often, but is nice for when people submit weird custom-setup data
  if(out1 == 3){
    special_error_check(uniqueID,runDir)
  }  
  
  #Rscript to omit duplicates
  map<-read.table('step_1.map',sep='\t',stringsAsFactors=F,comment.char="")
  exclude<-map[duplicated(map[,4]),2]
  if(verbose>1)print(paste0(Sys.time(),': Removed ',length(exclude),' SNPs that were duplicated'))
  write.table(exclude,file='step_2_exclusions',sep='\t',row.names=FALSE,col.names=F,quote=F)
  
  
  #loop over chromosomes
  for(chr in c("X",as.character(1:22))){
    if(verbose>0)print(paste0(Sys.time(),": Starting run on chromosome ",chr))  
  
    
    #First in loop - extract only one specific chromosome
    cmd2<-paste(plink," --file step_1 --chr ",chr," --recode --out step_2_chr",chr," --exclude step_2_exclusions",sep="")
    out2<-system(cmd2,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    #if X chromosome is missing it is allowed to skip forward
    if(out2 %in% c(12,13) & chr == "X"){
      if(verbose>0)print(paste0(Sys.time(),": Didn't find X-chr data, so skipping that"))
      next
    }
    
    #Then check for strand flips etc. 
    cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
    system(cmd3,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    
    
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
    
    #This removes any cases where there are more than two alleles involved
    forceHomozygoteTable<-forceHomozygoteTable[sapply(apply(forceHomozygoteTable[,c(5,6,9,10)],1,unique),length)==2,]
    
    #This removes any duplicates there might be
    forceHomozygoteTable<-forceHomozygoteTable[!duplicated(forceHomozygoteTable[,4]),]
    map<-read.table(paste("step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F,comment.char = "")

    #This loads the ped file, and doubles it
    ped2<-ped1<-strsplit(readLines(paste("step_2_chr",chr,".ped",sep=""))," ")[[1]]
    
    #this checks that nothing went wrong with reading in the map file in R (happens sometimes with quotes etc)
    if((length(ped1)-6) / 2 !=nrow(map)){
      have_quotes<-system(paste0("grep '\"' step_2_chr",chr,".map"),intern=T)
      if(length(have_quotes)>0){
        have_quotes_count <- length(have_quotes)
        if(have_quotes_count>5)have_quotes<-have_quotes[1:5]
        stop(paste0("Mismatch between read-in map and ped length. This has happened before, because of special characters in the SNP names. In this case ", have_quotes_count," SNP(s) were found to have quotes in them. These should be manually removed, e.g.: ",paste(have_quotes,collapse=", ")))  
      }else{
        stop("Mismatch between read-in map and ped length.")
      }
    }
    
    #this continues the handling of an extra spike-in person, for the shape-it run
    ped2[1]<-"Temporary"
    ped2[2]<-"Non_person"
    replacementPos<-which(map[,2]%in%forceHomozygoteTable[,4])
    A1_pos<-7+2*(replacementPos-1)
    A2_pos<-8+2*(replacementPos-1)
    ped2[A1_pos]<-forceHomozygoteTable[,9]
    ped2[A2_pos]<-forceHomozygoteTable[,10]
    ped<-rbind(ped1,ped2)
    write.table(ped,paste("step_3_chr",chr,".ped",sep=""),sep=" ",col.names=F,row.names=F,quote=F)
    omitRemaining<-logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
    if(verbose>1)print(paste0(Sys.time(),': Strand-flip handling. Omitting ',length(omitMissing),' because of missing, ',length(omitBlank),' because they are blank, and ',length(omitNonIdentical),' true strand flips'))
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    
    #running the shapeit command (with two people, the right one and a placeholder heterozygote
    cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
    system(cmd4,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    
    #checking for errors and stopping if there are any. 
    #No point to continue otherwise
    #There's a few specialized bug-hunter scripts that may be activated at this point
    log<-readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
    if(substr(log[length(log)],1,5)=="ERROR"){
      if(length(grep("Non biallelic site",log[length(log)]))>0){
        check_for_rare_nonbiallic_snps(uniqueID)
      }
      stop(paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep=""))
    }
    
    #removing the placeholder person again
    cmd5_1<-paste("cut --delimiter=' ' -f 1-7 step_4_chr",chr,".haps > step_5_chr",chr,".haps",sep="")
    system(cmd5_1)
    cmd5_2<-paste("head -n 3 step_4_chr",chr,".sample > step_5_chr",chr,".sample",sep="")
    system(cmd5_2)
    
    
    
    #detect max length of each chromosome
    cmd6<-paste("zcat /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2",sep="")
    maxPos<-as.numeric(system(cmd6,intern=T))
    
    
    #iterate over 5e6 chunks
    starts<-seq(0,maxPos,5e6)
    for(i in 1:length(starts)){
      start <- starts[i]
      end <- start+5e6
      
      
      #2020-07-05 Adding in the extra error logger and catcher
      #the full analysis showed that on current server-setup, the typical break point was around
      #4000 lines: 88.5% of failed chunks had more than 4000 lines (compare to 9% of non-failed chunks)
      #2020-12-25: changing max_imputation_chunk_size from 4000 to 3000 - we've had too many 'snapd.service: Watchdog timeout' errors lately, and they often seem to come just around the 3000 to 4000 range.
      #2021-01-01: make the max_imputation_chunk_size configurable
      cmd_special_1 <- paste0("awk '$3>",start," && $3<",end,"' step_5_chr",chr,".haps")
      chunk_lines_length<-length(system(cmd_special_1,intern=T))
      if(verbose>1)print(paste0(Sys.time(),": initiating impute2 run with a chunk of ",chunk_lines_length," lines, i is ",i))

      
      #This block checks the size of the chunk being prepared and executes the impute2 step accordingly.
      #If we know it's a big chunk, we split it up. If not we run it, but carefully catching errors and
      #re-running (this is the number 1 cause of non-caught issues with user data, because it fails so unpredictably)
      if(chunk_lines_length == 0){ #if it's zero we should just skip this
        if(verbose>2)print(paste0(Sys.time(),": skip impute2 run at ",i," with because chunk_lines_length was ",chunk_lines_length ))
        next
        
      #if the chunk is smaller than the max_imputation_chunk_size we try to run it, but catch any errors in preparation for re-run
      }else if(chunk_lines_length < get_conf("max_imputation_chunk_size")){
        cmd7<-paste(impute2," -m /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")
        step_7_log<-system(cmd7,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          
        #test for memory-lack bug (step_7_log will be 137 if killed, otherwise 0)
        if(step_7_log == 137){
          re_run_chunk <- TRUE
          divisions<-3
          print(paste0(Sys.time(),": restart impute2 run at ",i," with new subset to avoid memory-lack bug. This was done because of impute2-error-137. Chunk_lines_length was ",chunk_lines_length, ". If this error is observed often or around the time of known pipeline failures, it may be smart to reduce the max_imputation_chunk_size in the ~/misc_files/configuration.R to a lower number." ))
        }else{
          re_run_chunk <- FALSE
        }
        
      #if we already know the chunk is too big to handle, we demand a split of it  
      }else{
        re_run_chunk <- TRUE
        
        #calibrating how many divisions to use
        if(chunk_lines_length > get_conf("max_imputation_chunk_size") *3){
          divisions <- 9
        }else{
          divisions <-3
        }
        print(paste0(Sys.time(),": restart impute2 run at ",i," with new subset to avoid memory-lack bug. This was done before attempting impute2-run because chunk_lines_length was ",chunk_lines_length," and the re-run was divided in ",divisions," divisions." ))
      }
      
      
      
      #when prompted by above, we divide the job in smaller splits (n=divisions) 
      if(re_run_chunk){
        for(j in 1:divisions){
          start_2 <- floor(starts[i] + (j-1)*(5e6/ divisions))
          end_2 <- floor(starts[i]+ (j)*(5e6/ divisions))
          
          cmd7<-paste(impute2," -m /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o step_7_chr",chr,"_",i,"-",j,sep="")
          step_7_log_2<-system(cmd7,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          if(step_7_log_2 == 137)stop("the memory problem was still active after second round. It may be smart to reduce the max_imputation_chunk_size in the ~/misc_files/configuration.R to a lower number.")
          
        }
      }
    }
  }
  setwd(start_wd)
}








run_bulk_imputation<-function(
  uniqueIDs,
  runDir
){
  #' run bulk imputation
  #' 
  #' this is the function that runs imputation in bulk. It starts from 
  #' folders as prepared in the prepare_individual_genome and ends with 
  #' imputed, but not summarized data, typically for handling off to
  #' summarize_imputation, and subsequent export functions. These are 
  #' the exact same steps as those performed by bulk_imputation, only in bulk.
  #' See run_imputation function for further details.
  #' 
  #' @param uniqueIDs A set of 10 uniqueIDs to run in bulk imputation. 
  #' @param runDir The folder where the bulk imputation will be run. Note that this is usually is not under ~/imputations, but instead a dedicated ~/bulk_imputations folder where all 10 runs are collected. Thefore it is not an optional argument.
  #' 
  #' @return No return value, but on completion the runDir will contain imputed chunks of genomic data for each of the submitted bulk samples
  

  #load libraries
  library(tools)
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  #define program paths
  shapeit="/home/ubuntu/programs/shapeit.v2.904.3.10.0-693.11.6.el7.x86_64/bin/shapeit"
  plink="/home/ubuntu/programs/plink"
  impute2="/home/ubuntu/programs/impute_v2.3.2_x86_64_static/impute2"
  sample_ref="/home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
  
  
  
  #check uniqueID
  if(class(uniqueIDs)!="character")stop(paste("uniqueIDs must be character, not",class(uniqueIDs)))
  if(length(uniqueIDs)!=10)stop(paste("rawdata must be lengh 10, not",length(uniqueIDs)))
  
  #Check and set runDir
  start_wd <- getwd()
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  setwd(runDir)
  
  
  #test that impute2 static can run (or switch to dynamic)
  impute2_blank_run_out <- suppressWarnings(system(impute2,intern=T,ignore.stderr = T) )
  if(attr(impute2_blank_run_out,"status")==139){
    impute2 <- "/home/ubuntu/programs/impute_v2.3.2_x86_64_dynamic/impute2"
    if(verbose>2)print(paste0(Sys.time(),": Problem with impute_v2.3.2_x86_64_static detected. Switching to impute_v2.3.2_x86_64_dynamic."))
  }
  
  #check other arguments
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
  
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be lengh 1, not",length(verbose)))
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
  
  
  
  rawdata_files<-paste("/home/ubuntu/imputations/imputation_folder_",uniqueIDs,"/",uniqueIDs,"_raw_data.txt",sep="")
  names(rawdata_files)<-uniqueIDs
  if(!all(file.exists(rawdata_files))){
    missing<-rawdata_files[!file.exists(rawdata_files)]
    stop(paste("Did not find these rawdata files:",paste(missing,collapse=", ")))
  }
  
  
  #set runDir to the bulk_impute folder, define chromosomes and print a ready start message
  print(paste0(Sys.time(),": Starting imputation running on these uniqueIDs:"))
  cat(paste0("\n   a<-c('",paste(uniqueIDs,collapse="','"),"')\n\n"))
  
  setwd(runDir)
  chromosomes <- c("X",as.character(1:22))
  
  #First loop that basically just handles the uploaded data a little better, reformat it all to plink and do few check-ups of data consistency
  if(verbose>0)print(paste0(Sys.time(),": First loop of bulk processing. Checking files for each genome.")) 
  for(uniqueID in uniqueIDs){
    
    if(verbose>0)print(paste0(Sys.time(),": Preparing files for ",uniqueID))
    
    rawdata_file<-rawdata_files[uniqueID]
    
    #need to always check if the genes_for_good_cleaner should be run
    if(length(grep("genes for good",tolower(readLines(rawdata_file,n=5)))>0)){
      genes_for_good_cleaner(uniqueID)
    }
    
    #Load data using plink 1.9 (note it's pretty important which version to use, because they have different functionalities)
    cmd1<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1_",uniqueID)
    out1<-system(cmd1,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    
    
    #If the standard command fails, we run an extensive error rescue. Hopefully shouldn't be used too often, but is nice for when people submit weird custom-setup data
    if(out1 == 3 ){
      special_error_check(uniqueID)
    }  
    
    #Rscript to omit duplicates
    map<-read.table(paste0("step_1_",uniqueID,".map"),sep='\t',stringsAsFactors=F,comment.char = "")
    exclude<-map[duplicated(map[,4]),2]
    if(verbose>1)print(paste0(Sys.time(),': Removed ',length(exclude),' SNPs that were duplicated'))
    write.table(exclude,file=paste0('step_2_',uniqueID,'_exclusions'),sep='\t',row.names=FALSE,col.names=F,quote=F)
    
    #loop over chromosomes
    for(chr in chromosomes){
      #First in loop - extract only one specific chromosome
      cmd2<-paste(plink," --file step_1_",uniqueID," --chr ",chr," --make-bed --out step_2_",uniqueID,"_chr",chr," --exclude step_2_",uniqueID,"_exclusions",sep="")
      system(cmd2,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
    }  
  }
  
  
  
  #Second loop over chromosomes, in this one we merge the plink files from the different samples
  #and run the actual imputation
  if(verbose>0)print(paste0(Sys.time(),": Second loop of bulk-processing. Extracting data for each chromosome on all samples and imputing them. This loop will take 5-10 hours.")) 
  for(chr in chromosomes){
    if(verbose>0)print(paste0(Sys.time(),": Extracting chromosome ",chr," and imputing it.")) 
    merge_files<-paste0("step_2_",sub("_raw_data.txt","",basename(rawdata_files)),"_chr",chr)
    merge_df<-data.frame(bed=paste0(merge_files,".bed"),bim=paste0(merge_files,".bim"),fam=paste0(merge_files,".fam"))
    
    
    #check that all files are there
    missing <- vector()
    row_to_remove <- vector()
    for(i in 1:nrow(merge_df)){
      for(j in 1:ncol(merge_df)){
        f<-as.character(merge_df[i,j])
        if(!file.exists(f)){
          missing <-c(missing, f)
          row_to_remove <- unique(c(row_to_remove, i))
        }
      }
    }
    if(length(missing)>0){
      if(verbose>=0)print(paste0(Sys.time(),": WARNING These ",length(missing)," files from ",length(row_to_remove)," samples, were not found and will not be processed at chr",chr,": ",paste(missing,collapse=", ")))
      merge_df<-merge_df[!(1:nrow(merge_df))%in%row_to_remove,]
    }    
    
    #Consider if we should continue at all: if there's none, we don't. If there's more we do. 
    #However, if there's only one we have to proceed in a special way because the merge commands would fail
    if(nrow(merge_df)==0){ #no continue case
      if(verbose>0)print(paste0(Sys.time(),": NOTE Skipping chr",chr," because none of the samples had it"))
      next
      
    }else if(nrow(merge_df)==1){ #no merge case
      if(verbose>0)print(paste0(Sys.time(),": NOTE In chr",chr," there was no merging, because only one sample had it"))
      first_bed_file <- sub(".bed","",merge_df[1,1])
      cmd3 <- paste0(plink," --bfile ",first_bed_file," --recode --out step_2_chr",chr)
      out2<-system(cmd3,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
    }else if(nrow(merge_df)>1){ #merge case
      first_bed_file <- sub(".bed","",merge_df[1,1])
      merge_df<-merge_df[2:nrow(merge_df),]
      write.table(merge_df,file="step_2_merge_list.txt",sep="\t",quote=F,row.names=F,col.names=F)
      cmd4 <- paste0(plink," --bfile ",first_bed_file," --merge-list step_2_merge_list.txt --recode --out step_2m_chr",chr)
      out2<-system(cmd4,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
      
      
      
      #handling nonbiallelic (just excluding them)
      if(out2==3){
        missnp<-read.table(paste0("step_2m_chr",chr,".missnp"),stringsAsFactors = F)[,1]
        if(length(missnp) > 500) {
          messages<-paste0(Sys.time(),": ERROR The missnp>500 error was triggered. Outputting debug info.")
          debug_info<-list()
          for(uniqueID in uniqueIDs){
            cmd5<-paste0(plink," --bfile step_2_",uniqueID,"_chr",chr," --recode --out step_2_",uniqueID,"_chr",chr,"_missnp_hunt --extract step_2m_chr",chr,".missnp")
            system(cmd5,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
            debug_ped<-try(readLines(paste0("step_2_",uniqueID,"_chr",chr,"_missnp_hunt.ped")))
            if(class(debug_ped)=="try_error"){
              if(verbose>=0)print(paste(Sys.time(),": WARNING In missnp>500 debugging, could not read from",uniqueID))
              next
            }
            debug_gt<-strsplit(debug_ped," ")[[1]]
            debug_gt<-debug_gt[7:length(debug_gt)]
            debug_df<-data.frame(A1=debug_gt[c(T,F)], A2=debug_gt[c(F,T)])
            debug_df[,"snp"]<-read.table(paste0("step_2_",uniqueID,"_chr",chr,"_missnp_hunt.map"))[,2]
            debug_df[,"uniqueID"] <- uniqueID
            debug_info[[uniqueID]]<-debug_df
          }
          debug_all<-do.call(rbind,debug_info)
          for(msnp in sample(missnp,5)){
            messages <- c(messages,msnp)
            s<-debug_all[debug_all[,"snp"]%in%msnp,]
            for(i in 1:nrow(s)){
              messages<-c(messages,paste(t(s[i,])[,1],collapse=" "))
            }
          }
          
          #printing to log
          for(message in messages){
            cat(paste0(message,"\n"))
          }
          
          #Then sending as error mail (if possible)
          if(get_conf("from_email_address") != "" & get_conf("from_email_password") != "" & get_conf("error_report_mail")!= ""){
            if(verbose>=0)print(paste0(Sys.time(),": Sending error mail."))
            library("mailR")
            mailingResult<-try(send.mail(from = get_conf("from_email_address"),
                                         to = get_conf("error_report_mail"),
                                         subject = "An impute-me run has problem",
                                         body = paste(messages,collapse="<br>"),
                                         html=T,
                                         smtp = list(
                                           host.name = "smtp.gmail.com", 
                                           port = 465, 
                                           user.name = get_conf("from_email_address"), 
                                           passwd = get_conf("from_email_password"), 
                                           ssl = TRUE),
                                         authenticate = TRUE,
                                         send = TRUE),silent=T)
            if(class(mailingResult)=="try-error" & verbose > 2)print(paste0(Sys.time(),": Mailing failed."))
            
          }
            
            
          stop("Too many non-biallelic SNPs. This must be investigated. Check above debugging info")
          
        }
        for(uniqueID in uniqueIDs){
          #Go back to the previous files from previous step and take them, now excluding triallelics.
          cmd6<-paste0(plink," --bfile step_2_",uniqueID,"_chr",chr," --make-bed --out step_2_",uniqueID,"_chr",chr," --exclude step_2m_chr",chr,".missnp")
          system(cmd6,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
        }
        #then retry merge
        cmd7 <- paste0(plink," --bfile ",first_bed_file," --merge-list step_2_merge_list.txt --recode --out step_2m_chr",chr)
        out1_a<-system(cmd7,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      }
      
      
      #check for position duplicates
      map<-read.table(paste0("step_2m_chr",chr,".map"),stringsAsFactors = F,comment.char = "")
      if(sum(duplicated(map[,4]))>10000){
        duplicates_found <- sum(duplicated(map[,4]))
        stop(paste("Found",duplicates_found,"duplicate positions, and the current threshold is 10000"))
      }
      exclude<-unique(map[duplicated(map[,4]),2])
      write.table(exclude,file=paste0('step_2_overall_exclusions_chr',chr),sep='\t',row.names=FALSE,col.names=F,quote=F)
      
      
      cmd8<-paste(plink," --file step_2m_chr",chr," --exclude step_2_overall_exclusions_chr",chr," --recode --out step_2_chr",chr,sep="")
      system(cmd8,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
    }    
    
    #Then check for strand flips etc. 
    cmd9<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
    system(cmd9,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    
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
    if(verbose>1)print(paste0(Sys.time(),': Stand-flip handling. Omitting ',length(omitMissing),' because of missing, ',length(omitBlank),' because they are blank, and ',length(omitNonIdentical),' true strand flips'))
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    
    #running the shapeit command (with up to eleven people, the ten right ones and a placeholder heterozygote - or less if some where skipped)
    cmd10<-paste(shapeit," --force --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
    system(cmd10,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    
    #checking for errors and stopping if there are any. 
    #No point to continue otherwise
    #There's a few specialized bug-hunter scripts that may be activated at this point
    log<-readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
    if(substr(log[length(log)],1,5)=="ERROR"){
      if(length(grep("fully missing individuals",log[length(log)]))>0){
        count_x_chr_entries(chr)
      }
      stop(paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit_log.log",sep=""))
    }
    
    #removing the placeholder person again
    left_limit <- 5 + length(merge_files) * 2
    cmd11<-paste0("cut --delimiter=' ' -f 1-",left_limit," step_4_chr",chr,".haps > step_5_chr",chr,".haps")
    system(cmd11)
    top_limit <- 2 + length(merge_files) 
    cmd12<-paste0("head -n ",top_limit," step_4_chr",chr,".sample > step_5_chr",chr,".sample")
    system(cmd12)
    
    
    
    #detect max length of each chromosome
    cmd13<-paste("zcat /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2",sep="")
    maxPos<-as.numeric(system(cmd13,intern=T))
    
    
    #iterate over 5e6 chunks
    starts<-seq(0,maxPos,5e6)
    for(i in 1:length(starts)){
      start <- starts[i]
      end <- start+5e6
      
      
      
      
      
      
      
      #2020-07-05 Adding in the extra error logger and catcher
      #the full analysis showed that on current server-setup, the typical break point was around
      #4000 lines: 88.5% of failed chunks had more than 4000 lines (compare to 9% of non-failed chunks)
      cmd_special_1 <- paste0("awk '$3>",start," && $3<",end,"' step_5_chr",chr,".haps")
      chunk_lines_length<-length(system(cmd_special_1,intern=T))
      if(verbose>1)print(paste0(Sys.time(),": initiating impute2 run with a chunk of ",chunk_lines_length," lines, i is ",i))
      
      
      #execute impute2 step depending on how many lines we have
      if(chunk_lines_length == 0){ #if it's zero we should just skip this
        if(verbose>2)print(paste0(Sys.time(),": skip impute2 run at ",i," with because chunk_lines_length was ",chunk_lines_length ))
        next  
      }else if(chunk_lines_length < get_conf("max_imputation_chunk_size")){
        cmd14<-paste(impute2," -m /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")
        step_7_log<-system(cmd14,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
        
        
        #test for memory-lack bug (step_7_log will be 137 if killed, otherwise 0)
        if(step_7_log == 137){
          re_run_chunk <- TRUE
          divisions<-3
          if(verbose>=0)print(paste0(Sys.time(),": restart impute2 run at ",i," with new subset to avoid memory-lack bug. This was done because of impute2-error-137. Chunk_lines_length was ",chunk_lines_length, ". If this error is observed often or around the time of known pipeline failures, it may be smart to reduce the max_imputation_chunk_size in the ~/misc_files/configuration.R to a lower number." ))
        }else{
          re_run_chunk <- FALSE
        }
        #if we already know the chunk is too big to handle, we demand a split of it  
      }else{
        re_run_chunk <- TRUE
        
        #calibrating how many divisions to use, based on tests with very dense data it's good to divide in more chunks
        if(chunk_lines_length > get_conf("max_imputation_chunk_size")*3){
          divisions <- 9
        }else{
          divisions <-3
        }
        if(verbose>1)print(paste0(Sys.time(),": restart impute2 run at ",i," with new subset to avoid memory-lack bug. This was done before attempting impute2-run because chunk_lines_length was ",chunk_lines_length," and the re-run was divided in ",divisions," divisions." ))
      }
      
      
      #if necessary, per above, we divide the job in smaller bits 
      if(re_run_chunk){
        divisions<-3
        for(j in 1:divisions){
          start_2 <- floor(starts[i] + (j-1)*(5e6/ divisions))
          end_2 <- floor(starts[i]+ (j)*(5e6/ divisions))
          if(verbose>1)print(paste0(Sys.time(),": restart impute2 run at ",i,"-",j," with new subset to avoid memory-lack bug: ",start_2," to ",end_2)   )
          
          cmd15<-paste(impute2," -m /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o step_7_chr",chr,"_",i,"-",j,sep="")
          step_7_log_2<-system(cmd15,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          if(step_7_log_2 == 137)stop("the memory problem was still active after second round. It may be smart to reduce the max_imputation_chunk_size in the ~/misc_files/configuration.R to a lower number.")
        }
      }
    }
  }
  
  
  
  #clean up pre-step-5 files to save space
  if(verbose>0)print(paste0(Sys.time(),": Performing file-deletion and clean-up"))
  unlink(list.files(pattern="^step_1.+"))
  unlink(list.files(pattern="^step_2.+"))
  unlink(list.files(pattern="^step_3.+"))
  
  
  #In this loop over chromosomes, we copy out each of the step_7 files into separate imputation folders
  #making them ready for later (per-sample) run of the summarize_imputation function
  allFiles1<-list.files(runDir)
  for(chr in chromosomes){
    samples_path <- paste0("step_5_chr",chr,".sample")
    if(!file.exists(samples_path)){
      if(chr =="X"){
        if(verbose>0)print(paste0(Sys.time(),": Skipping the step_7-file division for chrX because no samples file found."))
        next
      }else{
        stop("Didn't find the step_5 samples file")
      }
    }
    samples<-read.table(samples_path,stringsAsFactors = F)
    for(uniqueID in uniqueIDs){
      outfolder <- paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/")
      w<-which(samples[,1]%in%uniqueID) -2
      
      if(length(w)==0 & chr=="X"){
        if(verbose>0)print(paste0(Sys.time(),": NOTE for ",uniqueID," at chr",chr," no imputed data was found, so this was skipped"))
        next
      }else if(length(w)!=1 & chr!="X"){
        stop(paste0("With ",uniqueID," at chr",chr," no imputed data was found."))
      }else{
        if(verbose>1)print(paste0(Sys.time(),": Extracting chromosome ",chr," from ",uniqueID," (sample ",w," of ",nrow(samples)-3,")"))  
      }
      
      
      
      
      #cut and transfer sample file
      write.table(samples[c(1:2,w+2),],file=paste0(outfolder,"step_4_chr",chr,".sample"),quote=F,row.names=F,col.names = F)
      
      #cut and transfer gen files
      step7Files<-grep(paste0("^step_7_chr",chr,"_"),allFiles1,value=T)
      step7ResultsFiles<-grep("[0-9]$",step7Files,value=T)
      left <- 5 + (w-1) * 3 + 1
      middle <- 5 + (w-1) * 3 + 2
      right <- 5 + (w-1) * 3 + 3
      for(step7ResultsFile in step7ResultsFiles){
        cmd16<-paste0("cut --delimiter=' ' -f 1,2,3,4,5,",left,",",middle,",",right," ",step7ResultsFile," > ",outfolder,step7ResultsFile)
        system(cmd16)
      }
    }
    Sys.sleep(0.5) #take a break
    
    #clean up files afterwards (or else we break the 30GB limit)
    unlink(step7Files) 
  }
  setwd(start_wd)
}





convert_vcfs_to_simple_format<-function(
  uniqueID
){
  #' convert vcfs to simple format
  #' 
  #' Function to check for an convert vcf to simple format files
  #' The overall idea is to subset the vcf file using a ~1.2 M long list of
  #' common SNPs of particular importance to the downstream algorithms
  #' This typically results in 200-400k actual VCF lines. These are extracted.
  #' Remaining genotypes from the start-list are filled with the assumption of
  #' always being homozygote-reference.
  #' To avoid accidentally filling a users DNA completely with homozygote reference
  #' several checks to make sure the VCF is from a standardized source are required.
  #' Most of these are found in the prepare_individual_genome function which is
  #' directly facing the user and running in real time. It will be beneficial 
  #' to tie together these two functions when used as part of a pipeline.
  #' A more thorough write-up of the cost-benefits of this approach is available
  #' here: https://doi.org/10.13140/RG.2.2.34644.42883)
  #' 
  #' @param uniqueID The uniqueID to process. Most be found in ~/vcfs/vcf_folder_<uniqueID>
  #' 
  #' @return No return value, but on completion the specified vcf-files from ~/vcfs will have been imported to the internal impute-me common-SNP format and placed at ~/data/<uniqueID>
  
  
  #set logging level load libraries and define chromosomes
  library(tools)
  chromosomes <- c(as.character(1:22),"X")
  verbose <- get_conf("verbose")
  if(verbose > 2){
    ignore.stdout <- FALSE 
  }else{
    ignore.stdout<- TRUE
  }
  if(verbose > 1){
    ignore.stderr <- FALSE 
  }else{
    ignore.stderr<- TRUE
  }
  
  
  #set paths (there's quite a few!)
  if(verbose>0)print(paste0(Sys.time(),": Converting the VCF-file from ",uniqueID," to internal standard files"))
  vcf_folder <- paste0("/home/ubuntu/vcfs/vcf_folder_",uniqueID,"/")
  bed_path<-"/home/ubuntu/srv/impute-me/imputeme/2021-01-03_common_snps.txt.gz"
  applied_bed_path <- paste0(vcf_folder,"temporary_bed.txt")
  vcf_path<-paste0(vcf_folder,uniqueID,"_raw_data.vcf.gz")
  variables_path <- paste0(vcf_folder,"variables.rdata")
  out_folder<-paste0("/home/ubuntu/data/",uniqueID)
  out_pdata_path<-paste0(out_folder,"/pData.txt")
  out_temp_path<-paste0(out_folder,"/temp")
  out_input_path<-paste0(out_folder,"/",uniqueID,".input_data.zip") #Odd naming, "out_input_path", I know, but it's because it should be a copy of the input-file saved in the ~/data folder. For VCFs however, it's the post-subsetting input file. They are too big otherwise.
  out_input_temp_path<-paste0(out_temp_path,"/",uniqueID,"_raw_data.txt")
  out_temp_gen_per_chr_path<-paste0(out_temp_path,"/",uniqueID,"_chr__CHR__.gen")
  out_gen_path<-paste0(out_folder,"/",uniqueID,".gen.zip")
  out_temp_simple_per_chr_path<-paste0(out_temp_path,"/",uniqueID,"_chr__CHR__.simple_format.txt")
  out_simple_path<-paste0(out_folder,"/",uniqueID,".simple_format.zip")
  minimum_required_variant_count <- 200000
  start_wd<-getwd()
  
  #checks
  if(!file.exists(vcf_folder))stop(paste0("For ",uniqueID," did not find a vcf_folder at: ",vcf_folder))
  
  
  #check if it possible to determine grch37/grch38 format
  testReadHeader<-try(readLines(vcf_path,n=150))
  grch37_hits<-grep("grch37|hg19",testReadHeader,ignore.case = TRUE,value=TRUE)
  grch38_hits<-grep("grch38|hg38",testReadHeader,ignore.case = TRUE,value=TRUE)
  if(length(grch37_hits)>0 & length(grch38_hits)>0){
    stop("The vcf reading algorithm found references to both genome build grch37 and grch38 in the vcf header and was unsure how to proceed.")
  }else if(length(grch37_hits)>0){
    build_guess <- "grch37"
  }else if(length(grch38_hits)>0){
    build_guess <- "grch38"
  }else{
    build_guess <- "none"
  }
  
  #handle initial ^chr if present. If it is present, i.e. chr1, chr2, chr3, etc, it will be handled later by conversion of bed file (more robust than tinkering with vcf itself)
  testRead<-try(read.table(vcf_path,nrow=100,stringsAsFactors=F))
  if(length(grep("^chr",testRead[,1]))>0){
    chr_prefix <- TRUE
  }else{
    chr_prefix <- FALSE
  }
  
  
  
  #convert the bed file to whichever is appropriate for importing these data
  if(build_guess %in% c("none","grch37") & !chr_prefix){
    cmd1 <- paste0("zcat ",bed_path," > ",applied_bed_path)
    system(cmd1)
    
  }else if(build_guess %in% c("none","grch37") & chr_prefix){
    cmd1 <- paste0("zcat ",bed_path," | sed 's/^/chr/g' > ",applied_bed_path)
    system(cmd1)
    
  }else if(build_guess %in% c("grch38") & !chr_prefix){
    cmd1 <- paste0("zcat ",bed_path," | cut -f 1,8,9 > ",applied_bed_path)
    system(cmd1)
    
  }else if(build_guess %in% c("grch38") & chr_prefix){
    cmd1 <- paste0("zcat ",bed_path," | cut -f 1,8,9 | sed 's/^/chr/g'> ",applied_bed_path)
    system(cmd1)
    
  }else{stop("impossible")}
  
  
  
  
  
  #Initiate conversion with vcftools call 
  cmd2 <- paste0("vcftools --gzvcf ",vcf_path," --positions ",applied_bed_path," --out ",vcf_folder,"extracted --plink-tped")
  system(cmd2,ignore.stdout=ignore.stdout, ignore.stderr=ignore.stderr)
  
  #read and check vcftool output
  tped<-read.table(paste0(vcf_folder,"extracted.tped"),stringsAsFactors=F,sep="\t")
  if(nrow(tped) < minimum_required_variant_count){
    if(verbose>0)print(paste0(Sys.time(),": Observed less than minimum_required_variant_count (",nrow(tped),"). Will try to re-run extraction step assuming GRCh38."))
    
    #ideally we would have liked to only test build-version once (above), but since major formats like e.g. 
    #Nebula seems to be in GRCh38 - without saying so in the header - we'll re-run the relevant import 
    #test once more for GRCh38 before failing it.
    build_guess<-"grch38" #update guess (it'll fail anyway if it is not true)
    if( !chr_prefix){
      cmd1 <- paste0("zcat ",bed_path," | cut -f 1,8,9 > ",applied_bed_path)
      system(cmd1)
    }else if(chr_prefix){
      cmd1 <- paste0("zcat ",bed_path," | cut -f 1,8,9 | sed 's/^/chr/g'> ",applied_bed_path)
      system(cmd1)
    }
    cmd2 <- paste0("vcftools --gzvcf ",vcf_path," --positions ",applied_bed_path," --out ",vcf_folder,"extracted --plink-tped")
    system(cmd2,ignore.stdout=ignore.stdout, ignore.stderr=ignore.stderr)
    tped<-read.table(paste0(vcf_folder,"extracted.tped"),stringsAsFactors=F,sep="\t")
    if(nrow(tped) < minimum_required_variant_count){
      stop(paste("This VCF-file only had",nrow(tped),"recognized SNPs from the 1M requested common-SNP set. This must be at least ",minimum_required_variant_count," or else we suspect a problem with the DNA sequencing"))
    }
    
    #But in the (best) success case, we continue
  }else{
    if(verbose>0)print(paste0(Sys.time(),": ",uniqueID," had ",nrow(tped)," variants matched with the request-bed file"))
  }
  
  
  #Reading in the full unmodified bed - there's a few difficult steps here as well, 
  #because right now the 'tped' object is all vcf-variants matched on positions (GCRh38 or GCRh37 as it may be)
  #and no hard requirements for variant-names, which is the second column in the tped. They should be matched.
  #Ideally on rsid, but not all vcfs will have them. If they don't we will keep matching by position
  #but there likely would be a problem with wrongly taking some indels or rare-variants at the same position unless 
  #we are careful. "Careful beyond rsid match", currently means accepting only the Dante lab style of chr:pos variant naming, 
  #since that approach is well-tested. Future releases could include a chr:pos:A1-A2 matching logic.
  bed<-read.table(bed_path,stringsAsFactors=F,sep="\t",header=T)
  
  #First check if they can be matched by rsid
  if(length(grep("^rs",tped[,"V2"])) > minimum_required_variant_count){
    rownames(tped) <- tped[,"V2"]
    rownames(bed) <- bed[,"rsid"]
    
    #Or else revert to positional matching (be careful here - as explained above!)
  }else if(length(grep("[0-9]+:[0-9]+",tped[,"V2"])) > minimum_required_variant_count){
    if(verbose>0)print(paste0(Sys.time(),": Switching to chr:pos positional matching in the style of Dante lab"))
    rownames(tped) <- tped[,"V2"]
    
    if(build_guess %in% c("none","grch37")){
      if(chr_prefix){
        rownames(bed) <- paste0("chr",bed[,"chr_pos_name"])
      }else{
        rownames(bed) <- bed[,"chr_pos_name"]  
      }
    }else if(build_guess %in% c("grch38")){
      if(chr_prefix){
        bed[,"chr_pos_name_hg38"] <- paste0("chr",bed[,"chr"],":",bed[,"grch38_start"])
      }else{
        bed[,"chr_pos_name_hg38"] <- paste0(bed[,"chr"],":",bed[,"grch38_start"]) 
      }
      bed<-bed[!duplicated(bed[,"chr_pos_name_hg38"] ),]
      rownames(bed)<-bed[,"chr_pos_name_hg38"]
    }else{stop("impossible2")}
    
  }else{
    stop("Not matching on rsid in vcf-name field and also not recognised as Dante lab chr:pos naming. Needs manual evaluation.")
  }
  
  #double check that a reasonable number of variants are matched (we still use the minimum_required_variant_count variable, although
  #it now lost meaning a little because it's a lot of different counts. Fact is
  #that these values are still up for tuning, but a fairly wide canyon - all the
  #way down to exon seq is accepted)
  if(length(intersect(rownames(tped), rownames(bed))) < minimum_required_variant_count){
    stop("Too few vcf-name fields could be matched in the reference bed file")
  }
  
  
  
  #Now all relevant SNPs have been exported to a tped, we have ensured that the bed-file has matching information
  #and both GRCh37 and GRCh38 locations, and there is a key to extract the alt/ref info from the bed file.
  #then we first create a data.frame with the inferred homozygote refs
  w1 <- which(!rownames(bed) %in% rownames(tped))
  homozygote_refs<-data.frame(
    rsid=bed[w1,"rsid"],
    chr=bed[w1,"chr"],
    pos=bed[w1,"grch37_start"],
    genotype=paste0(bed[w1,"ref"],bed[w1,"ref"]),
    stringsAsFactors=F)
  
  
  #create data.frame with vcf-extracted data (i.e. those that are not homozygote ref)
  w2 <- which(rownames(bed) %in% rownames(tped))
  non_homozygote_refs<-data.frame(
    rsid=bed[w2,"rsid"],
    chr=bed[w2,"chr"],
    pos=bed[w2,"grch37_start"],
    genotype=apply(tped[rownames(bed)[w2],c(5,6)],1,paste,collapse=""),
    stringsAsFactors=F)
  
  
  #switch allele order (alphabetical, we don't know the phase anyway)
  switch_alleles <- function(x){
    x<-sub("/","",x)
    corrector <- c("AA","AC","AG","AT","AC","CC","CG","CT","AG","CG","GG","GT","AT","CT","GT","TT")
    names(corrector) <- c("AA","AC","AG","AT","CA","CC","CG","CT","GA","GC","GG","GT","TA","TC","TG","TT")
    as.character(corrector[x])
  }
  non_homozygote_refs[,"genotype"] <- switch_alleles(non_homozygote_refs[,"genotype"])
  
  
  #merge and sort by pos
  output <- rbind(non_homozygote_refs,homozygote_refs)
  output[,"chr"]<-factor(output[,"chr"], levels=c(1:22,"X","Y"))
  output <- output[order(output[,"chr"], output[,"pos"]),]
  
  
  #determine sex,with custom trained approach that counts the heterozygote fraction
  #of X-chromosome SNPs. Why not the plink method? The plink method is great
  #when everything is there, but too often there's missing data, non-standard
  #submissions etc, so we've changed to using this approach since 2021-02-15
  main_cutoff<-0.006
  x_variants<-which(output[,"chr"] %in% "X")
  x_homozygotes <- sum(substr(output[x_variants,"genotype"],1,1) == substr(output[x_variants,"genotype"],2,2),na.rm=T)
  x_heterozygotes <- sum(substr(output[x_variants,"genotype"],1,1) != substr(output[x_variants,"genotype"],2,2),na.rm=T)
  het_frac<-signif(x_heterozygotes / (x_homozygotes+x_heterozygotes),3)
  if(is.na(het_frac)){
    sex<-0
  }else if(het_frac <= main_cutoff){
    sex<- 1
  }else if(het_frac > main_cutoff){
    sex<- 2
  }else{
    sex<-0
  }
  
  
  #Prepare data out_folder
  if(file.exists(out_folder)){
    if(length(list.files(out_folder))>0){
      stop(paste("The out_folder",out_folder,"already exists and has other files in it"))
    }
  }else{
    dir.create(out_folder)
  }
  #also write a temp folder already
  dir.create(out_temp_path)
  
  
  #write an input-type file. This is going to contain the same data as the simple.format file.,
  #because we don't impute anything. But it has to be there to avoid crashing dependencies
  write.table(output, file=out_input_temp_path,sep="\t",row.names=F,col.names=F,quote=F)
  setwd(out_temp_path)
  zip(out_input_path, basename(out_input_temp_path), flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  
  
  
  #Write a pData file
  load(variables_path)
  timeStamp<-format(Sys.time(),"%Y-%m-%d-%H-%M")
  md5sum <- md5sum(vcf_path)
  imputation_type<-"vcf"
  f<-file(out_pdata_path,"w")
  writeLines(paste(c("uniqueID","filename","email","first_timeStamp","md5sum","gender","protect_from_deletion","should_be_imputed","imputemany_upload","upload_time","imputation_type"),collapse="\t"),f)
  # writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,sex,protect_from_deletion,should_be_imputed,imputemany_upload,upload_time,imputation_type),collapse="\t"),f)
  writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,sex,TRUE,should_be_imputed,imputemany_upload,upload_time,imputation_type),collapse="\t"),f)
  
  close(f)
  
  
  
  
  #write a simple-format-type file.
  files_for_zipping <- vector()
  for(chr in chromosomes){
    filename <- sub("__CHR__",chr,out_temp_simple_per_chr_path)
    o <- output[output[,"chr"]%in%chr,]
    write.table(o, file=filename,sep="\t",row.names=F,col.names=F,quote=F)
    files_for_zipping <- c(files_for_zipping, filename)
  }
  setwd(out_temp_path)
  zip(out_simple_path, basename(files_for_zipping), flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  
  
  
  
  
  #write a gen-format-type file.
  #e.g.
  # --- rs149201999 16050408 T C 0.947 0.053 0
  # --- rs146752890 16050612 C G 0.946 0.054 0
  # This is a bit more complicated. The output object currently don't contain ref/alt info.
  #the easiest and most proximal source for that is the bed-file already loaded,
  #(note, in a sense this is a silly excercise - we shouldn't convert vcf-measurements back to 
  #a probabilistic imputation-output format - but we have to, to make sure everything don't crash
  #downstream)
  files_for_zipping <- vector()
  for(chr in chromosomes){
    filename <- sub("__CHR__",chr,out_temp_gen_per_chr_path)
    files_for_zipping <- c(files_for_zipping, filename)
    
    #subset to relevant chromosome  - both tped genotypes and the input bed
    o1 <- output[output[,"chr"]%in%chr,]
    bed_here <- bed[bed[,1]%in%chr,]
    if(!all(o1[,"pos"] == bed_here[,2]))stop(paste("Very odd error when comparing bed and output for chr",chr,uniqueID))#This is probably waste-ful, there's absolutely no reason the bed and the output wouldn't be identical.
    
    #generate new output format emulating gen format
    o2<-data.frame(
      chr=rep("---",nrow(o1)),
      rsid=o1[,"rsid"],
      pos=o1[,"pos"],
      ref=bed_here[,5],
      alt=bed_here[,6],
      prob1=NA,
      prob2=NA,
      prob3=NA,
      genotype=o1[,"genotype"],
      stringsAsFactors = F
    )
    
    
    
    #insert probabilities - homozygote refs
    w1<-which(o1[,"genotype"] == paste0(o2[,"ref"],o2[,"ref"]))
    o2[w1,"prob1"] <- 1
    o2[w1,"prob2"] <- 0
    o2[w1,"prob3"] <- 0
    
    
    #insert probabilities - heterozygotes
    w2<-which(o1[,"genotype"] == paste0(o2[,"alt"],o2[,"ref"]) | o1[,"genotype"] == paste0(o2[,"ref"],o2[,"alt"]))
    o2[w2,"prob1"] <- 0
    o2[w2,"prob2"] <- 1
    o2[w2,"prob3"] <- 0
    
    
    
    #insert probabilities - homozygote alts
    w3<-which(o1[,"genotype"] == paste0(o2[,"alt"],o2[,"alt"]))
    o2[w3,"prob1"] <- 0
    o2[w3,"prob2"] <- 0
    o2[w3,"prob3"] <- 1
    
    
    #check how many are missing still (typically just a few, due to odd alt-notation)
    w4<-which(apply(is.na(o2[,c("prob1","prob2","prob3")]),1,sum)>0)
    o2[w4,"prob1"] <- 0
    o2[w4,"prob2"] <- 0
    o2[w4,"prob3"] <- 0
    if(verbose>2)print(paste0(Sys.time(),": When generating pseudo-.gen file from the vcf-data, there was ",length(w4)," missing variants assigned a 0-0-0 score at chr ",chr))
    
    #then write.out
    write.table(o2, file=filename,sep=" ",row.names=F,col.names=F,quote=F)
  }
  setwd(out_temp_path)
  zip(out_gen_path, basename(files_for_zipping), flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  
  
  
  #deleting working folders and reset wd folder
  # if(!protect_from_deletion)unlink(vcf_folder,recursive = T)
  unlink(out_temp_path,recursive=T)
  setwd(start_wd)
  
  
  
}








summarize_imputation<-function(
  uniqueID,
  runDir,
  destinationDir="/home/ubuntu/data",
  export_simple_format = FALSE
){
  #' summarize imputation
  #'
  #' A function to summarize the output of an ~/imputation_folder imputation-run 
  #' into  gen and simple-format files in the ~/data folder. Particularly the simple-format
  #' conversion is computationally expensive, but is required for users who wish to
  #' download their own imputed data in an easily-understandable format. In addtion the 
  #' function performs merging of the chunks that are produced in imputation. Since there
  #' are no paralelization benefits to merging this process, it is separated from
  #' the run_imputation and run_bulk_imputation functions - but should always be run
  #' after each of these.
  #'
  #' @param runDir A folder where the imputation process was run (no matter if it's bulk or single running)
  #' @param uniqueID The uniqueID to process. Will be obvious when the runDir folder is the product of a single run, but is required regardless to avoid mix-ups.
  #' @param destinationDir The folder where the data will be copied to. Defaults to ~/data
  #' @param export_simple_format A logical indicating whether the simple format script should be run (23andme-like format, based on hard-calls on .gen file)
  #' 
  #' @return The two paths to the downloadble .gen and simple-format zip-files. In addition these will be available in the ~/data/<uniqueID> folder

  #Set libraries
  library(tools)
  
  #set logging level
  verbose <- get_conf("verbose")
  
  #define programs
  gtools="/home/ubuntu/programs/gtool"
  # plink="/home/ubuntu/programs/plink-1.07-x86_64/plink" #note, as of 2015-08-31 this must be plink 1.07, otherwise we get a bug
  plink="/home/ubuntu/programs/plink" #note, as of 2020-01-06 - this seems to have been resolved now and we can use the regular plink (1.9)
  
  start_wd<-getwd()
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  setwd(runDir)
  
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
 
  if(class(export_simple_format)!="logical")stop(paste("export_simple_format must be logical, not",class(export_simple_format)))
  if(length(export_simple_format)!=1)stop(paste("export_simple_format must be lengh 1, not",length(export_simple_format)))
   
  if(class(destinationDir)!="character")stop(paste("destinationDir must be character, not",class(destinationDir)))
  if(length(destinationDir)!=1)stop(paste("destinationDir must be lengh 1, not",length(destinationDir)))
  if(!file.exists(destinationDir))stop(paste("Did not find destinationDir at path:",destinationDir))
  if(length(grep("/$",destinationDir))!=0)stop("Please don't use a trailing slash in the destinationDir")
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be lengh 1, not",length(verbose)))
  if(verbose > 2){
    ignore.stdout <- FALSE 
  }else{
    ignore.stdout<- TRUE
  }
  if(verbose > 1){
    ignore.stderr <- FALSE 
  }else{
    ignore.stderr<- TRUE
  }
  
  if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
  if(length(gtools)!=1)stop(paste("gtools must be lengh 1, not",length(gtools)))
  if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be lengh 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(file.exists(paste0(destinationDir,"/",uniqueID))){
    if(length(list.files(paste0(destinationDir,"/",uniqueID)))>0){
      stop(paste0("The destinationDir '",paste0(destinationDir,"/",uniqueID),"' already exists and has files in it. This is a major unforeseen error")  )
    }else{
      dir.create(paste0(destinationDir,"/",uniqueID))
    }
  }
  
  
  #getting files to merge in (using grep on list.files, to be more robust on lost files)
  allFiles1<-list.files(runDir)
  step7Files<-grep("^step_7_chr",allFiles1,value=T)
  step7ResultsFiles<-grep("[0-9]$",step7Files,value=T)
  chromosomes<-unique(sub("_[0-9-]+$","",sub("^step_7_chr","",step7ResultsFiles)))
  chromosomes<-chromosomes[order(suppressWarnings(as.numeric(chromosomes)))]
  
  #start message
  if(verbose>0)print(paste0(Sys.time(),": Starting summarize_imputation on uniqueID ",uniqueID))
  
  #get the right order, note this is somewhat complicated by the fact that most chunks are marked numerically, then some are marked as e.g. 10-1, 10-2 (see the memory limit issue)
  for(chr in chromosomes){
    if(verbose>0)print(paste0(Sys.time(),": Merging chunks in chromosome ",chr))
    s <-grep(paste("^step_7_chr",chr,"_",sep=""), step7ResultsFiles,value=T)
    main_number <- as.numeric(sub("-[0-9]","",sub("^.+_","",s)))
    suffix_number <- suppressWarnings(as.numeric(sub("^-","",sub("^[0-9]+","",sub("^.+_","",s)))))
    s<-s[order(main_number,suffix_number)]
    if(verbose>1)print(paste0(Sys.time(),": For chr",chr," these were the files to merge: ",paste(s,collapse=", ")))
    cmd1<-paste("cat ",paste(s,collapse=" ")," > ",uniqueID,"_chr",chr,".gen",sep="")
    system(cmd1)
    unlink(s)
  }	
  
  
  #Checking that all required output .gen files are present and report
  genFiles<-paste(uniqueID,"_chr",chromosomes,".gen",sep="")
  if(length(genFiles)==0){
    stop("Didn't find a single gen-file")
  }else if(length(genFiles)==1){
    stop(paste0("Only found a single gen-file: ",genFiles," - this usually happens if the summarize_imputation function have already been run on this uniqueID (and deleted input files)."))
  }else{
    if(export_simple_format){
      if(verbose>0)print(paste0(Sys.time(),": conversion of ",length(genFiles)," concatenated .gen files into simple-format and plink files"))  
    }else{
      if(verbose>0)print(paste0(Sys.time(),": finished preparing ",length(genFiles)," chromosomal .gen files into one"))
    }
  }
  
  
  #determine the type of imputation: if it is a vcf file or if it is bulk or single imputation
  #Note that in the current data-flow plan, vcf-files will not pass this way - so the vcf-check should be uncessary.
  if(exists("is_vcf_file") && is_vcf_file){
    imputation_type<-"vcf"
  }else{
    crontabs<-try(grep("^#",system("crontab -l",intern=T),invert = T,value=T),silent=T)
    crontabs<-sub(" .+$","",sub("^.+Rscript /home/ubuntu/srv/impute-me/imputeme/","",crontabs))
    if(any(c("bulk_imputation_cron_job.R","imputation_cron_job.R")%in%crontabs)){
      if("imputation_cron_job.R"%in%crontabs){
        imputation_type<-"single"
      }else{
        imputation_type<-"bulk"
      }
    }else{
      imputation_type <- NA
    }
  }
  
  
  
  #determine sex,with custom trained approach that counts the heterozygote fraction
  #of X-chromosome SNPs. Why not the plink method? The plink method is great
  #when everything is there, but too often there's missing data, non-standard
  #submissions etc, so we've changed to using this approach since 2021-02-15
  main_cutoff<-0.006
  if(!"X" %in% chromosomes){
    sex <- 0
  }else{
    x_file_path<-paste(uniqueID,"_chrX.gen",sep="")
    x_file<-read.table(x_file_path,sep=" ",stringsAsFactors = F)[,6:8]
    x_homozygotes<-sum(x_file[,1]+x_file[,3] > 0.8,na.rm=T)
    x_heterozygotes<-sum(x_file[,2] > 0.8,na.rm=T)
    het_frac<-signif(x_heterozygotes / (x_homozygotes+x_heterozygotes),3)
    
    if(is.na(het_frac)){
      sex<-0
    }else if(het_frac <= main_cutoff){
      sex<- 1
    }else if(het_frac > main_cutoff){
      sex<- 2
    }else{
      sex<-0
    }
    rm(x_file)
  }
  #prepare with the old method
  # gender<-system(paste("cut --delimiter=' ' -f 6 ",runDir,"/step_4_chr22.sample",sep=""),intern=T)[3]
  # if(verbose>0)print(paste0(Sys.time(),": the new approach to sex gives ",sex,", the old approach gives ",gender))
  
  
  #preparing destinationDir
  prepDestinationDir<-paste(destinationDir,"/",uniqueID,sep="")
  if(!file.exists(prepDestinationDir))dir.create(prepDestinationDir)
  

  #Optional - export to simple format
  if(export_simple_format){
    for(genFile in genFiles){
      chr <- sub("\\.gen$","",sub("^.+_chr","",genFile))
      
      #catching some odd observations of empty 'chr' handles, seen when running low on memory
      if(nchar(chr)==0){
        if(verbose>0)print(paste0(Sys.time(),": Observed an odd length-0 chromosome genFiles with filename: ",genFile,". It was skipped, but may want to investigate logs closer."))
        next
      }
      
      #Else continue
      if(verbose>1)print(paste0(Sys.time(),": Creating simple-format data from chromosome ",chr))
      sampleFile<-paste("step_4_chr",chr,".sample",sep="")
      
      #make list of indels
      cmd2<-paste("awk -F' ' '{ if ((length($4) > 1 ) || (length($5) > 1 ) || $4 == \"-\" || $5 == \"-\") print $2 }'",genFile,">",paste("step_8_chr",chr,"_snps_to_exclude",sep=""))
      system(cmd2)
      
      #exclude indels
      cmd3 <- paste(gtools," -S --g ",genFile," --s ",sampleFile," --exclusion step_8_chr",chr,"_snps_to_exclude --og step_8_chr",chr,".gen",sep="")
      system(cmd3,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
      #Convert to ped format
      cmd4 <- paste(gtools," -G --g step_8_chr",chr,".gen --s ",sampleFile," --chr ",chr," --snp",sep="")
      system(cmd4,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
      #reform to plink fam/bim/bed file			
      cmd5 <- paste(plink," --file step_8_chr",chr,".gen --recode --transpose --noweb --out step_9_chr",chr,sep="")
      system(cmd5,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
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
      
      #	re-run if it's less than 100 bytes (fair to assume something was wrong then)
      if(size<100 ){
        if(verbose>0)print(paste0("retrying step 8-9 command for chr",chr,". Trying to split it in pieces (non-normal low memory running)"))
        cmd7 <- paste("split --lines 5000000 step_8_chr",chr,".gen step_8_extra_chr",chr,".gen",sep="")
        system(cmd7)
        chunks<-grep(paste("step_8_extra_chr",chr,"\\.gena[a-z]$",sep=""),list.files(runDir),value=T)
        for(chunk in chunks){
          ch<-sub("^.+\\.","",chunk)
          cmd8 <- paste(gtools," -G --g ",chunk," --s ",sampleFile," --chr ",chr," --snp",sep="")
          system(cmd8,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          #reform to plink fam/bim/bed file			
          cmd9 <- paste(plink," --file ",chunk," --recode --transpose --noweb --out step_9_chr",chr,"_",ch,sep="")
          system(cmd9,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          #re-order to 23andme format
          cmd10<-paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' step_9_chr",chr,"_",ch,".tped  > step_9_chr",chr,"_split_",ch,".txt",sep="")
          system(cmd10)
        }
        cmd11<-paste("cat ",paste(paste("step_9_chr",chr,"_split_",sub("^.+\\.","",chunks),".txt",sep=""),collapse=" ")," > step_10_chr",chr,".txt",sep="")
        system(cmd11)			
      }
      
      #remove NN
      cmd12 <- paste("awk '{ if($4 != \"NN\") print}' step_10_chr",chr,".txt  > step_11_chr",chr,".txt",sep="")
      system(cmd12)
      
      
      #remove duplicates
      cmd13 <- paste("awk -F',' '!seen[$1]++' step_11_chr",chr,".txt > ", sub("\\.gen$","",genFile),".simple_format.txt",sep="")
      system(cmd13)
      
      
      #removing some particularly big temporary files
      unlink(list.files(runDir,pattern=paste0("^step_8_chr",chr),full.names=T))
      unlink(list.files(runDir,pattern=paste0("^step_9_chr",chr),full.names=T))
      unlink(list.files(runDir,pattern=paste0("^step_10_chr",chr),full.names=T))
    }
    
    
    #removing a bunch of remaining half-way files from simple-format generation -  
    #all smaller size (that were still nice to have if the above loop failed)
    unlink(list.files(runDir,pattern="^step_5_",full.names=T))
    unlink(list.files(runDir,pattern="^step_6_",full.names=T))
    unlink(list.files(runDir,pattern="^step_7_",full.names=T))
    unlink(list.files(runDir,pattern="^step_11_",full.names=T))
    
    #zipping and moving simple_format files
    if(verbose>0)print(paste0(Sys.time(),": Zipping files for simple-format export"))
    zipFile_simpleformat<-paste(runDir,paste(uniqueID,".simple_format.zip",sep=""),sep="/")
    twentythreeandmeFiles<-paste(uniqueID,"_chr",chromosomes,".simple_format.txt",sep="")
    zip(zipFile_simpleformat, twentythreeandmeFiles, flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
    zipFile_simpleformat_destination <- paste(prepDestinationDir,basename(zipFile_simpleformat),sep="/")
    move_result <- suppressWarnings(file.rename(zipFile_simpleformat, zipFile_simpleformat_destination))
    if(!move_result){ #this would fail, for example if data is on another volume. But we can still copy
      file.copy(zipFile_simpleformat, zipFile_simpleformat_destination)
      unlink(zipFile_simpleformat)
    }
    unlink(list.files(runDir,pattern="simple_format",full.names=T))
    
  }
  
  
  #cleaning up remaining step_x files (not needed after this point)
  unlink(list.files(runDir,pattern="^step_1_",full.names=T))
  unlink(list.files(runDir,pattern="^step_2_",full.names=T))
  unlink(list.files(runDir,pattern="^step_3_",full.names=T))
  unlink(list.files(runDir,pattern="^step_4_",full.names=T))

  #zipping gen files
  if(verbose>0)print(paste0(Sys.time(),": Zipping files for gen-format export"))
  zipFileGen<-paste(runDir,paste(uniqueID,".gen.zip",sep=""),sep="/")
  zip(zipFileGen, genFiles, flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  zipFileGen_destination <- paste(prepDestinationDir,basename(zipFileGen),sep="/")
  move_result <- suppressWarnings(file.rename(zipFileGen, zipFileGen_destination))
  if(!move_result){ #this would fail, for example if data is on another volume. But we can still copy
    file.copy(zipFileGen, zipFileGen_destination)
    unlink(zipFileGen)
  }
  unlink(genFiles)
  
  
  
  
  #move the original file to ~/data/<uniqueID> as well
  zipFileOriginal<-paste(runDir,paste(uniqueID,".input_data.zip",sep=""),sep="/")
  zip(zipFileOriginal, paste(uniqueID,"_raw_data.txt",sep=""), flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  zipFileOriginal_destination <- paste(prepDestinationDir,basename(zipFileOriginal),sep="/")
  move_result <- suppressWarnings(file.rename(zipFileOriginal, zipFileOriginal_destination))
  if(!move_result){ #this would fail, for example if data is on another volume. But we can still copy
    file.copy(zipFileOriginal, zipFileOriginal_destination)
    unlink(zipFileOriginal)
  }
  
  
  
  
  #creating the pData file
  if(verbose>0)print(paste0(Sys.time(),": Creating pdata file for ",uniqueID))
  load(paste0(runDir,"/variables.rdata"))
  if(!exists("should_be_imputed")) should_be_imputed  <- NA
  if(!exists("imputemany_upload")) imputemany_upload  <- NA
  if(!exists("upload_time")) upload_time <- NA
  timeStamp<-format(Sys.time(),"%Y-%m-%d-%H-%M")
  md5sum <- md5sum(paste(uniqueID,"_raw_data.txt",sep=""))
  # gender<-system(paste("cut --delimiter=' ' -f 6 ",runDir,"/step_4_chr22.sample",sep=""),intern=T)[3]
  f<-file(paste0(prepDestinationDir,"/pData.txt"),"w")
  writeLines(paste(c("uniqueID","filename","email","first_timeStamp","md5sum","gender","protect_from_deletion","should_be_imputed","imputemany_upload","upload_time","imputation_type"),collapse="\t"),f)
  writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,sex,protect_from_deletion,should_be_imputed,imputemany_upload,upload_time,imputation_type),collapse="\t"),f)
  close(f)

  
  #return paths
  if(export_simple_format){
    returnPaths<-c(
      paste(prepDestinationDir,basename(zipFile_simpleformat),sep="/"),
      paste(prepDestinationDir,basename(zipFileGen),sep="/")
    )
    names(returnPaths)<-c("23andme","gen")
  }else{
    returnPaths<-paste(prepDestinationDir,basename(zipFileGen),sep="/")
    names(returnPaths)<-c("gen")
  }
  
  
  #checking out how much trash this function leaves behind. Could clean up a little actually.
  if(verbose>2)print(paste0(Sys.time(), ": summarize_imputation did not remove files on the following list. They may be removed in the future if space requirements are tight: ",paste(list.files(runDir),collapse=", ")))
  
  
  #revert to base folder and return paths
  setwd(start_wd)
  invisible(returnPaths)
}




transfer_cleanup_and_mailout<-function(
  uniqueID){
  #' transfer cleanup and mailout
  #' 
  #' This is the final part of most cron-job runs. It checks wether Hub or Node running is in effect
  #' and, if Node, transfers Node:~/data/<uniqueID> to Hub:~/data/<uniqueID>. It also creates downloadable
  #' links in the www-folder for download during the 14 day time-span. Finally it send off results per mail,
  #' if configured, and cleans up ~/imputations/imputation_folder_<uniqueID> (and ~/data/<uniqueID> if running
  #' as Node).
  #' 
  #' @param uniqueID The uniqueID to perform transfer cleanup and mailout for
  
  
  #libraries
  library("mailR")
  library("rJava")
  
  #get logging level and other global variables
  verbose <- get_conf("verbose")
  serverRole <- get_conf("serverRole")
  hubAddress <- get_conf("hubAddress")
  
  
  #check arguments
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))

  #get sample specific variables
  pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
  pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
  email<-pData[1,"email"]
  filename<-pData[1,"filename"]
  if("imputemany_upload"%in%colnames(pData)){
    imputemany_upload<-pData[1,"imputemany_upload"]
  }else{
    imputemany_upload<-FALSE
  }
  if("imputation_type"%in%colnames(pData)){
    imputation_type<-pData[1,"imputation_type"]
    if(!is.na(imputation_type) && imputation_type=="vcf"){
      is_vcf_file<-TRUE
    }else{
      is_vcf_file<-FALSE
    }
  }else{
    is_vcf_file<-FALSE
  }
  if(is_vcf_file){
    summary_folder<-paste0("/home/ubuntu/vcfs/vcf_folder_",uniqueID)
  }else{
    summary_folder<-paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID)  
  }
  
  
  #check if a simple-format file is available
  if(file.exists(paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip"))){
    export_simple_format<-TRUE
  }else{
    export_simple_format<-FALSE
    if(verbose>1)print(paste0(Sys.time(),": simple.format file not found - writing message accordingly"))
  }
  
  #If this is running as a node, we need to copy it back
  if(serverRole== "Node"){
    cmd5 <- paste("scp -r /home/ubuntu/data/",uniqueID," ubuntu@",hubAddress,":/home/ubuntu/data",sep="")
    out5<-system(cmd5)
    if(out5!=0)stop(paste0("Problem with transferring the data for ",uniqueID,". Possible connection error. Aborting. Nothing was changed, so after solving connectivity the function can be re-run as transfer_cleanup_and_mailout('",uniqueID,"') without problems"))
  }
  
  
  #making a link out to where the data can be retrieved	(different on hub and node)
  if(serverRole== "Node" & !is_vcf_file){
    
    if(export_simple_format){
      cmd6 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip /home/ubuntu/srv/impute-me/www/",uniqueID,".simple_format.zip'",sep="")
      out6<-system(cmd6)
    }else{
      out6<-0
    }
    
    cmd7 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip /home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip'",sep="")
    out7<-system(cmd7)
    
    cmd8 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json /home/ubuntu/srv/impute-me/www/",uniqueID,"_data.json'",sep="")
    out8<-system(cmd8)
    
    if(out6+out7+out8 > 0)stop(paste0("Problem with symlink-creation on Hub for ",uniqueID,". Possible connection error. Aborting."))
    
  }else if(serverRole== "Hub" & is_vcf_file){
    file.symlink(
      from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json",sep=""),
      to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,"_data.json",sep="")
    )
    
  }else if(serverRole== "Hub" & !is_vcf_file){
    
    if(export_simple_format){
      file.symlink(
        from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip",sep=""),
        to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".simple_format.zip",sep="")
      )
    }
    file.symlink(
      from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip",sep=""),
      to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip",sep="")
    )
    file.symlink(
      from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json",sep=""),
      to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,"_data.json",sep="")
    )
    
  }else{stop("very odd")}
  
  
  
  
  if(exists("imputemany_upload") && imputemany_upload){
    print(paste0(Sys.time(),": Skipping mail because it's from imputemany_upload"))
  }else{
    print(paste0(Sys.time(),": Sending results mail"))
    ip<-"https://www.impute.me"
    
    if(export_simple_format){
      simple_format_message<-paste0(" <a href=",ip,"/www/",uniqueID,".simple_format.zip>simple-format</a>,")
    }else{
      simple_format_message<-""
    }
    
    #put in always-on locations
    location_gen <- paste(ip,"/www/",uniqueID,".gen.zip",sep="")
    location_json <- paste(ip,"/www/",uniqueID,"_data.json",sep="")
    
    #assign the right language book to people (Swedes and Norwegians can get the Danish language one too)
    if(length(grep("\\.dk$",email))==1 | length(grep("\\.no$",email))==1 | length(grep("\\.se$",email))==1){
      booklink<-"https://www.saxo.com/dk/forstaa-dit-dna_lasse-westergaard-folkersen_haeftet_9788770170154"
    }else{
      booklink<-"https://www.worldscientific.com/worldscibooks/10.1142/11070"
    }
    
    
    if(!is_vcf_file){
      message <- paste("<HTML>We have completed processing the file <i>",filename,"</i>. You can now go to <a href='www.impute.me'>www.impute.me</a> and explore the analysis-modules using this ID:<br><br> <b>",uniqueID,"</b><br><br>The service is non-profit, but the computing price for an imputation is approximately 5 USD per imputation. So if you have not done so already, please make a contribution to keep the servers running (<u><a href='",get_conf("paypal"),"'>paypal</a></u>).<br><br>If you have any further questions, please refer to the book <u><a href='",booklink,"'>'Understand your DNA'</a></u> that serves as a guide for the underlying concepts of this analysis.<br><br>For advanced users, it is also possible to download full data as ",simple_format_message,"<a href=",location_gen,">gen-format</a> and <a href=",location_json,">json-format</a> files. These contain imputed data and calculated phenotype information.<br></HTML>",sep="")
    }else{
      message <- paste("<HTML>We have completed processing the file <i>",filename,"</i>. You can now go to <a href='www.impute.me'>www.impute.me</a> and explore the analysis-modules using this ID:<br><br> <b>",uniqueID,"</b><br><br>The service is non-profit, but the computing price for an imputation is approximately 5 USD per imputation. So if you have not done so already, please make a contribution to keep the servers running (<u><a href='",get_conf("paypal"),"'>paypal</a></u>).<br><br>If you have any further questions, please refer to the book <u><a href='",booklink,"'>'Understand your DNA'</a></u> that serves as a guide for the underlying concepts of this analysis.<br><br>For advanced users, it is also possible to download analyzed data as <a href=",location_json,">json-format</a> files. These contain calculated phenotype information.<br></HTML>",sep="")
    }
    
    
    if(get_conf("from_email_address") != "" & get_conf("from_email_password") != ""){
      for(tryCount in 1:3){
        print(paste0(Sys.time(),": Trying to mail to ",email))
        mailingResult<-try(send.mail(from = get_conf("from_email_address"),
                                     to = email,
                                     subject = "Imputation is ready",
                                     body = message,
                                     html=T,
                                     smtp = list(
                                       host.name = "smtp.gmail.com", 
                                       port = 465, 
                                       user.name = get_conf("from_email_address"), 
                                       passwd = get_conf("from_email_password"), 
                                       ssl = TRUE),
                                     authenticate = TRUE,
                                     send = TRUE))
        Sys.sleep(5)
        if(class(mailingResult)!="try-error")break
        if(tryCount == 3)stop("MAILING FAILED. THIS SHOULD BE FOLLOWED UP")
      }
    }else{
      print(paste0(Sys.time(),": Email is not configured - this is the intended message: ",message))
    }
    
    
    
  }
  
  #Only completely remove the run folder towards the end, otherwise new run may start up while running export scripts
  unlink(summary_folder,recursive = T)
  
  
  #also clear the hub imputation_folder if running as node
  if(serverRole== "Node"){
    cmd9 <- paste("ssh ubuntu@",hubAddress," 'rm -r /home/ubuntu/imputations/imputation_folder_",uniqueID,"'",sep="")
    out9<-system(cmd9)
    
    #also don't leave the finished data here, if running as Node
    if(out9==0){
      unlink(paste("/home/ubuntu/data/",uniqueID,sep=""),recursive=TRUE)
    }else{
      stop("Error code for shh call to Hub. Final removal of data aborted, but note that several other transfers already have taken place, including results mailing. Need to manually untangle.")
    }
  }
  
}






format_ancestry_com_as_23andme<-function(
  path
){
  #' format ancestry com as 23andme
  #' 
  #' this is a function to be called whenever a text file with 5 columns and 
  #' header row needs to be reformatted to a text file with 4 columns and no 
  #' header rows (and 20 commented out lines at the top). I.e. when reforming 
  #' from ancestry.com to 23andme format.
  #'
  #' @param path The path of the file to reformat
  #' 

  
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







format_myheritage_as_23andme<-function(
  path
){
  #' format myheritage as 23andme
  #' 
  #' this is a function to be called whenever a text file with , separation needs 
  #' to be reformatted to a text file with 4 columns and no header rows. I.e. 
  #' when reforming from myheritage.com to 23andme format.
  #' 
  #' @param path The path of the file to reformat
  
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  
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






genes_for_good_cleaner<-function(
  uniqueID,
  runDir=NULL
){
  #' genes for good cleaner
  #' 
  #' A small extra function for removing some odd characters in genes_for_good
  #' data
  #' 
  #' @param uniqueID The uniqueID of the sample to check
  #' @param runDir The folder where the imputation is found. Will set to standard location at ~/imputations/imputation_folder_<uniqueID> if left un-specified

  plink="/home/ubuntu/programs/plink"
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  if(class(uniqueID)!="character")stop(paste("uniqueID must be a character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  
  if(is.null(runDir)){
    runDir<-paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/")
  }
  if(class(runDir)!="character")stop(paste("runDir must be a character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be length 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at",runDir))
  
  if(verbose>=0)print(paste0(Sys.time(),": The genes_for_good_cleaner was activated"))
  
  rawdata_file<-paste("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/",uniqueID,"_raw_data.txt",sep="")
  if(!file.exists(rawdata_file))stop(paste("error in genes-for-good-cleaner: didn't find file at",rawdata_file))
  #Common problem 1 -  # signs in the rsids. Should remove those lines.
  cmd_special_8<-paste0("sed -i.bak6 '/#/d' ",rawdata_file)
  system(cmd_special_8)
}









special_error_check<-function(
  uniqueID,
  runDir=NULL
){
  #' special error check
  #' 
  #' The function that is activated if anything looks odd in standard-plink handling
  #' of the files in a prepare_individual_genome run. It is not meant for essential
  #' standard running, but may often catch additional errors
  #' 
  #' @param uniqueID The uniqueID of the sample to check
  #' @param runDir The folder where the imputation is found. Will set to standard location at ~/imputations/imputation_folder_<uniqueID> if left un-specified

  #set plink path
  plink="/home/ubuntu/programs/plink"
  
  #set logging level
  verbose <- get_conf("verbose")
  

  if(class(uniqueID)!="character")stop(paste("uniqueID must be a character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  
  if(is.null(runDir)){
    runDir<-paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID,"/")
  }
  if(class(runDir)!="character")stop(paste("runDir must be a character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be length 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at",runDir))
  
  rawdata_file<-paste(runDir,"/",uniqueID,"_raw_data.txt",sep="")
  if(!file.exists(rawdata_file))stop(paste("error in special-error-check: didn't find file at",rawdata_file))
  
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be lengh 1, not",length(verbose)))
  
  if(verbose>=0)print(paste0(Sys.time(),": The special_error_check was activated"))
  
  
  special_error_status<-vector()
  line_count_cmd<-paste0("wc -l ",rawdata_file)
  line_count_0<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
  
  #Common problem 1: mitochondrial SNPs (not used in any analysis anyway)
  cmd_special_1<-paste("sed -i.bak1 '/\\tMT\\t/d'",rawdata_file)
  system(cmd_special_1)
  line_count_1<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
  if(line_count_0-line_count_1>0)special_error_status <- c(special_error_status, paste0("MT removals (",line_count_0-line_count_1,")"))
  
  
  #Common problem 2: Presence of triple dashes, that should just be double dashes
  md5_before <- md5sum(rawdata_file)
  cmd_special_2<-paste0("sed -i.bak2 's/\\t---/\\t--/' ",rawdata_file)
  system(cmd_special_2)
  if(md5_before != md5sum(rawdata_file))c(special_error_status, "--- to --")
  
  
  #Common problem 3: Presence of indels that can't be handled by the plink -23file function
  #this needs to be handled in a very weird way, because clearly awk cant distinguish all line endings systematically
  cmd_special_3a<-paste0("awk '!(length($4) != 3)' ",rawdata_file, " > ",runDir,"/step_1_temp_indel_01.txt")
  system(cmd_special_3a)
  line_count_3a<-as.integer(sub(" .+$","",system(paste0("wc -l ",runDir,"/step_1_temp_indel_01.txt"),intern=T)))
  cmd_special_3b<-paste0("awk '!(length($4) != 2)' ",rawdata_file, " > ",runDir,"/step_1_temp_indel_02.txt")
  system(cmd_special_3b)
  line_count_3b<-as.integer(sub(" .+$","",system(paste0("wc -l ",runDir,"/step_1_temp_indel_02.txt"),intern=T)))
  if(line_count_3a > line_count_3b){
    file.rename(paste0(runDir,"/step_1_temp_indel_01.txt"),rawdata_file)
  }else{
    file.rename(paste0(runDir,"/step_1_temp_indel_02.txt"),rawdata_file)
  }
  line_count_3<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
  if(line_count_1-line_count_3>0)special_error_status <- c(special_error_status, paste0("INDEL removals (",line_count_1-line_count_3,")"))
  
  
  
  
  #Common problem 4: lack of sorting (first re-check if this is a problem after MT removal)
  cmd_special_3<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out ",runDir,"/step_1_",uniqueID,"_sorting_check")
  system(cmd_special_3,intern=T)
  sorting_check<-readLines(paste0(runDir,"/step_1_",uniqueID,"_sorting_check.log"))
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
  #First - for the special case that no plink run have been successful so far - 
  #we re-run plink because the map file is needed. Then we check using a specialized
  #check_genome_build function
  cmd_special_3<-system(cmd_special_3,intern=T)
  map_file1 <- paste0(runDir,"/step_1_",uniqueID,".map") #source: a started bulk run
  map_file2 <- paste0(runDir,"/step_1.map")  #source: a started individual run
  map_file3 <- paste0(runDir,"/step_1_",uniqueID,"_sorting_check.map") #source: post-plink run after
  if(file.exists(map_file1) | file.exists(map_file2) | file.exists(map_file3)){
    if(file.exists(map_file1)){
      map_file<-map_file1
    }else if(file.exists(map_file2)){
      map_file<-map_file2
    }else if(file.exists(map_file3)){
      map_file<-map_file3
    }
    special_error_status<-c(special_error_status,check_genome_build(map_file))
  }else{
    special_error_status<-c(special_error_status, "built check could not be completed due to missing map file.")
  }
  
  
  #Common problem 8 - some providers have started putting # signs in the rsids (genes for good for example). Should remove those lines.
  md5_before <- md5sum(rawdata_file)
  cmd_special_8<-paste0("sed -i.bak5 '/#/d' ",rawdata_file)
  system(cmd_special_8)
  if(md5_before != md5sum(rawdata_file))special_error_status<-c(special_error_status, "hashtags in rsids")
  
  
  #then re-check and decide future action (note, it's a little silly, but we have to run it twice because bulk running has step_1_[uniqueID] naming and single running just has step_1 naming. And we don't know which this is, and it's more cumbersome to set up a check for it. In the future, maybe just have all runs do step_1_[uniqueID] naming. At least silence it so it don't clutter logs... it takes just a half second)
  cmd_final1<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1_",uniqueID)  
  cmd_final2<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1")  
  out_final1<-system(cmd_final1,ignore.stdout = T, ignore.stderr = T)
  out_final2<-system(cmd_final2,ignore.stdout = T, ignore.stderr = T)
  
  if(out_final1 == 0 & length(grep("failed built check",special_error_status))==0){
    if(verbose>0)print(paste0(Sys.time(),": Somehow the special errors section actually cleared up this one. These were the error messages: ",paste(special_error_status,collapse=", ")))
    #and move on
  }else{
    #however if still failing, we have to send a mail
    library("mailR")
    error1<-system(cmd_final1,intern=T)
    error1 <- gsub("\b","",error1)
    message <- paste0("<HTML>",uniqueID," failed all attempts at starting imputation. It came with special error status:<br><b>", paste(special_error_status,collapse="<br>"),".</b><br><br>The last error message was this: <br><br><small>",paste(error1,collapse="<br>"),"</small><br></HTML>")
    if(verbose>=0)print(paste0(Sys.time(),": Sending error mail and giving up. This is the error:"))
    print(special_error_status)
    mailingResult<-try(send.mail(from = get_conf("from_email_address"),
                                 to = get_conf("error_report_mail"),
                                 subject = "An impute-me sample has problem",
                                 body = message,
                                 html=T,
                                 smtp = list(
                                   host.name = "smtp.gmail.com", 
                                   port = 465, 
                                   user.name = get_conf("from_email_address"), 
                                   passwd = get_conf("from_email_password"), 
                                   ssl = TRUE),
                                 authenticate = TRUE,
                                 send = TRUE),silent=T)
    stop("Special error check failed")
  }
  return(special_error_status)
}  









get_genotypes<-function(
  uniqueID,
  request,
  namingLabel="cached", 
  call_threshold = 0.8,
  ignore_indels = TRUE
){
  #' get genotypes
  #' 
  #' The function to extract specific genotypes for a given uniqueID. The function
  #' will firstly check the cached-files in the ~/data/<uniqueID>/ folder, and
  #' can quickly obtain genotypes that are already saved there. If the requested
  #' genotypes are not found there, it will extract them from the genome-wide
  #' data using gtools (at the specified threshold in the dosage data). This takes a 
  #' bit more time, and is best done in pre-processing.
  #'  
  #' @param namingLabel should default to cached, but it's a way of separately saving larger cached sets in a different file
  #' @param call_threshold threshold for calling SNP. Ok with 0.8 for multi-SNP signatures, but should definetly be increased in special high-importance SNPs. Default from gtool is suggested at 0.9.
  #' @param ignore_indels is the most programmatic-robust setting (because it gives the letter-based genotypes, not just e.g. 1/1), but obviously not very smart for some things, like BRCA.
  #' 
  #' @return A data.frame containing the genotypes of the requested SNPs for the requested uniqueID

  gtools="/home/ubuntu/programs/gtool"
  
  #set logging level
  verbose <- get_conf("verbose")
  suppressWarnings(library("shiny"))

  #checking namingLabel
  if(class(namingLabel)!="character")stop(paste("namingLabel must be character, not",class(namingLabel)))
  if(length(namingLabel)!=1)stop(paste("namingLabel must be lengh 1, not",length(namingLabel)))
  
  #checking verbosity
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be lengh 1, not",length(verbose)))
  if(verbose > 2){
    ignore.stdout <- FALSE 
  }else{
    ignore.stdout<- TRUE
  }
  if(verbose > 1){
    ignore.stderr <- FALSE 
  }else{
    ignore.stderr<- TRUE
  }

  
  #checking data in uniqueID's home folder
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
  idFolder<-paste("/home/ubuntu/data",uniqueID,sep="/")
  if(!file.exists(idFolder))stop(paste0(Sys.time(),": Did not find an idFolder at ",idFolder))
  genZipFile<-paste(idFolder,"/",uniqueID,".gen.zip",sep="")
  inputZipFile<-paste(idFolder,"/",uniqueID,".input_data.zip",sep="")
  cachedGenotypeFile<-paste(idFolder,"/",uniqueID,".",namingLabel,".gz",sep="")
  
  #start message
  if(file.exists(cachedGenotypeFile) & verbose>1)print(paste0(Sys.time(),": Found quick-access cache-file for label ",namingLabel,", getting required variants."))  
  if(!file.exists(cachedGenotypeFile) & verbose>0)print(paste0(Sys.time(),": Extracting variants in label ",namingLabel," directly from genome-wide data and saving them."))  
    
    
    
  #creating a temp folder to use
  idTempFolder<-paste("/home/ubuntu/data",uniqueID,"temp",sep="/")
  if(file.exists(idTempFolder))stop(safeError(paste("Temp folder exists, this could indicate that",uniqueID,"is already worked on. Wait a little, or write administrators if you think this is a mistake")))
  
  
  #checking other variables
  if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
  if(length(gtools)!=1)stop(paste("gtools must be lengh 1, not",length(gtools)))
  if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
  if(class(ignore_indels)!="logical")stop(paste("ignore_indels must be ignore_indels, not",class(ignore_indels)))
  if(length(ignore_indels)!=1)stop(paste("ignore_indels must be lengh 1, not",length(ignore_indels)))
  if(class(request)!="data.frame")stop(paste("request must be data.frame, not",class(request)))
  if(!"chr_name"%in%colnames(request))stop("request object must have a column 'chr_name'")
  if(length(grep("^chr",request[,"chr_name"]))>0)stop("Don't precede chromosomes by chr")
  if("already_exists"%in%colnames(request) & verbose > 0)print(paste0(Sys.time(),": Request object had a column 'already_exists', this will be overwritten"))
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
      stop(safeError(paste("Did not find genome-wide data for this uniqueID. This probably means that raw data has been deleted and no furter SNP-data can be retrieved. Since our policy is to delete personally traceable data, such as genome-wide SNP data, after 14 days, this can often affect users that submitted their data before an update. Their is no solution to this, other than re-upload of data.")))
    }
    
    
    dir.create(idTempFolder)
    chromosomes<-unique(requestDeNovo[,"chr_name"])
    contents<-unzip(genZipFile,list=T)
    
    #create a blank genotypes object
    genotypes<-data.frame(genotype=vector(),stringsAsFactors=F)
    
    #if input is in as a chromosome, use the simple-format as input
    if("input"%in%chromosomes){
      snpsFromInput<-rownames(requestDeNovo[requestDeNovo[,"chr_name"]%in%"input",])
      outZip<-unzip(inputZipFile, overwrite = TRUE,exdir = idTempFolder, unzip = "internal")
      cmd0 <- paste("grep -E '",paste(paste(snpsFromInput,"\t",sep=""),collapse="|"),"' ",outZip,sep="")
      input_genotypes<-suppressWarnings(system(cmd0,intern=T))
      if(length(input_genotypes)>0){
        input_genotypes<-do.call(rbind,strsplit(input_genotypes,"\t"))
        input_genotypes[,4]<-sub("\r$","",input_genotypes[,4])
        if(any(nchar(input_genotypes[,4])!=2)){
          if(verbose>0)print(paste0(Sys.time(),": WARNING input data should have length 2 genotypes. We try to clean the \r ending once more"))
          input_genotypes[,4]<-sub("\r$","",input_genotypes[,4])
        }
        if(any(nchar(input_genotypes[,4])!=2))stop("Input data must have length 2 genotypes")
          
        
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
        if(verbose>=0)print(paste0(Sys.time(),": These files were missing in the zip-gen file:",paste(missing,collapse=", ")))
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
          if(verbose>1)print(paste0(Sys.time(),": Extracting quick-access cache-SNPs at chr",chr," - try",tryCount))
          gen<-paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen",sep="")
          snpsHere<-rownames(requestDeNovo)[requestDeNovo[,"chr_name"]%in%chr]
          write.table(snpsHere,file=paste(idTempFolder,"/snps_in_chr",chr,".txt",sep=""),quote=F,row.names=F,col.names=F)
          cmd1<-paste0(gtools," -S --g " , gen, " --s ",idTempFolder,"/samples.txt --inclusion ",idTempFolder,"/snps_in_chr",chr,".txt --log ",idTempFolder,"/gtool_log1.txt")
          system(cmd1,ignore.stdout=ignore.stdout, ignore.stderr=ignore.stderr)
          subsetFile<-paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset",sep="")
          if(!file.exists(subsetFile)){
            print(paste0(Sys.time(),": Did not find any of the SNPs on chr",chr))	
            next
          }
          
          if(ignore_indels){
            cmd2<-paste0(gtools," -G --g " ,subsetFile," --s ",idTempFolder,"/samples.txt --snp --threshold ",call_threshold," --log ",idTempFolder,"/gtool_log2.txt")  
          }else{
            cmd2<-paste0(gtools," -G --g " ,subsetFile," --s ",idTempFolder,"/samples.txt --threshold ",call_threshold," --log ",idTempFolder,"/gtool_log2.txt")
          }
          system(cmd2,  ignore.stdout=ignore.stdout, ignore.stderr=ignore.stderr)
          
          
          ped<-try(strsplit(readLines(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.ped",sep="")),"\t")[[1]],silent=T)
          map<-try(read.table(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.map",sep=""),stringsAsFactors=FALSE),silent=T)
          
          if(class(ped)!="try-error" & class(map)!="try-error"){
            ped<-ped[7:length(ped)]
            genotypes_here<-try(data.frame(row.names=map[,2],genotype=sub(" ","/",ped),stringsAsFactors=F))
            #error where there's duplicate row names
            if(class(genotypes_here)=="try-error"){
              if(verbose>0)print(paste0(Sys.time(),": WARNING Found a duplicate row names error. Removing it."))
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













get_GRS<-function(
  genotypes, 
  betas
){
  #' get GRS
  #' 
  #' this function is deprecated --- use get_GRS_2 instead
  #' 
  
  # warning("Please don't use get_GRS anymore. Use get_GRS_2. Or the plink callers.")
  
  if(class(genotypes)!="data.frame")stop(paste("genotypes must be data.frame, not",class(genotypes)))
  if(!"genotype"%in%colnames(genotypes))stop(paste("genotypes must have a column genotype"))
  if(!all(unique(sub("[0-9].+$","",rownames(genotypes)))%in%c("i","rs"))){
    stop(paste("genotypes must have rownames starting with rs. You had these:",paste(unique(sub("[0-9].+$","",rownames(genotypes))),collapse=", ")))
  }
  
  if(class(betas)!="data.frame")stop(paste("genotypes must be data.frame, not",class(betas)))
  necessary_columns<-c("effect_allele","non_effect_allele","Beta")
  if(!all(necessary_columns%in%colnames(betas)))stop(paste("betas must have a column",paste(necessary_columns,collapse=", ")))
  if(!all(unique(sub("[0-9].+$","",rownames(betas)))%in%c("i","rs")))stop("betas must have rownames starting with rs")
  
  if(!all(rownames(betas)%in%rownames(genotypes)))stop("all SNPs in betas must be present in genotypes")
  
  
  
  geneticRiskScore<-0
  for(snp in rownames(betas)){
    genotype<-strsplit(genotypes[snp,],"/")[[1]]
    effect_allele<-betas[snp,"effect_allele"]
    non_effect_allele<-betas[snp,"non_effect_allele"]
    
    beta<-betas[snp,"Beta"]	
    geneticRiskScore <- geneticRiskScore + sum(genotype%in%effect_allele) * beta
  }
  return(geneticRiskScore)
  
}





get_GRS_2<-function(
  snp_data, 
  mean_scale=T, 
  unit_variance=T
){
  #' get GRS 2
  #' 
  #' The main genetic risk score calculator. This function is the one described 
  #' here https://doi.org/10.3389/fgene.2020.00578 as well as in the readme.md in 
  #' the impute-me github repository. It is used to take care of
  #' mean-scaling and unit-variance. The older functin get_GRS is rarely used.
  #' For some use-cases with tens of thousands or more SNPs, pre-calculatedwork using
  #' the plink --score is used instead of this function as is further documented
  #' in the prs module, as well as at this write-up: https://doi.org/10.13140/RG.2.2.10081.53602
  #' 
  #' @param snp_data A data frame with genotype, effect sizes and information on effect/non-effect allele. Optionally also information about minor allele frequency and minor/major allele (for use with mean scaling etc)
  #' @param mean_scale A logical. If TRUE the GRS output is scaled so that the average person, by MAF-information, will have a score of 0
  #' @param unit_variance A logical. If TRUE the GRS output is scaled so that 68% of everyone, by MAF/HWE-information, are within 1 unit of 0 (=1 SD)
  #' 
  #' @return a data.frame which is a copy of the submitted snp_data object, with polygenic risk score information added as additional columns
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  if(class(snp_data)!="data.frame")stop(paste("snp_data must be data.frame, not",class(snp_data)))
  if("Beta"%in%colnames(snp_data) & !"effect_size"%in%colnames(snp_data)){
    if(verbose>0)print(paste0("No 'effect_size' column was found by get_GRS_2, as is necessary per 2017-03-14 - but a 'Beta' column was renamed to 'effect_size'. Do fix in the future"))
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
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
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
  if(length(missing_snps)>0 & verbose>2){
    print(paste0(Sys.time()," Note, for ",length(missing_snps)," SNPs, we found missing genotypes. This can cause errors particularly if the data is not mean centered. These were skipped: ",paste(missing_snps,collapse=", ")))
  }
  
  if(length(missing_major_minor_snps)>0 & verbose>2){
    print(paste0(Sys.time()," Note, for ",length(missing_major_minor_snps)," SNPs, we found missing major/minor/freq-allele information. These SNPs were skipped: ",paste(missing_major_minor_snps,collapse=", ")))      
  }
  
  if(length(missing_effect_info_snps)>0  & verbose>2){
    print(paste0(Sys.time()," Note, for ",length(missing_effect_info_snps)," SNPs, we found wrong or missing information on what was effect-allele and what was non-effect-allele. They were skipped: ",paste(missing_effect_info_snps,collapse=", ")))
    
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







crawl_for_snps_to_analyze<-function(
  uniqueIDs=NULL
){
  #' crawl for snps to analyze
  #' 
  #' A function that will crawl all ~/data directories to extract all currently 
  #' worked on SNPs. A worked on SNPs is stored in one of the cache files, and 
  #' can be accessed much quicker than when going through the genome-wide data
  #' 
  #' @param uniqueIDs Optional vector of uniqueIDs to check. Otherwise all uniqueIDs in ~/data are checked - which may take a very long time.
  #' 
  #' @return No return value, but on completion the ~/data/<uniqueID> folder will contain cache-versions of relevant SNPS, available for quick-access 


  #set libraries
  suppressWarnings(library("shiny"))
  
  
  #set logging level
  verbose <- get_conf("verbose")
  if(verbose>0)print(paste0(Sys.time(),": Starting the crawl_for_snps_to_analyze function to extract quick-access cache txt-files for important variants."))
  
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
    if(verbose>0)print(paste0(Sys.time(),": Checking all requested SNPs from ",uniqueID))	
    
    genotypes<-try(get_genotypes(uniqueID=uniqueID,request=all_SNPs))
    if(class(genotypes)=="try-error"){
      if(file.exists(paste("/home/ubuntu/data/",uniqueID,"/temp",sep=""))){
        next
      }else{
        if(verbose>0){
          print(paste0(Sys.time(),": NOTE Some other error happened in the extraction crawler, but probably no cause for alarm. Printing genotypes:"))
          print(genotypes)
        }
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
  load("/home/ubuntu/srv/impute-me/AllDiseases/2021-01-28_all_gwas_snps.rdata")
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








make_overview_of_samples<-function(
  type="bash"
){
  #' make overview of samples
  #' 
  #' A function to make a data.frame overview of all genomes in ~/data
  #' Works by looping through the pData.txt files in each folder
  #' and putting them together in a data.frame with columns corresponding
  #' to the largest set found in any pData
  #' 
  #' @param type R or bash - the type of reading of pData files. Will work faster with bash, but the R-based reading is kept as a secondary option
  #' @param verbose How verbose the function is.
  #' 
  #' @return A data.frame with pData-information of all samples in ~/data
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  uniqueIDs<-list.files("/home/ubuntu/data/")
  if(length(verbose)!=1)stop("verbose must be length 1")
  if(length(type)!=1)stop("type must be length 1")
  if(class(verbose)!="numeric")stop("verbose must be class numeric")
  if(class(type)!="character")stop("type must be class character")
  allowedTypes <- c("bash","R")
  if(!type%in%allowedTypes)stop("Type must be one of",paste(allowedTypes,collapse=", "))
  t0 <- Sys.time()
  
  if(type == "bash"){
    cmd1 <- paste(c("echo -n > /home/ubuntu/misc_files/temporary_file_for_overview.txt;",
                    "for filename in /home/ubuntu/data/id_*;",
                    "do",
                    "file=$filename/pData.txt;",
                    "cat $file >> /home/ubuntu/misc_files/temporary_file_for_overview.txt;",
                    "done"),collapse=" ")
    system(cmd1)
    d<-readLines("/home/ubuntu/misc_files/temporary_file_for_overview.txt")
    unlink("/home/ubuntu/misc_files/temporary_file_for_overview.txt")
    d1<-strsplit(d,"\t")
    all_columns<-unique(unlist(d1[seq(1,length(d1),by=2)]))
    output <- data.frame(matrix(ncol=length(all_columns),nrow=length(uniqueIDs),dimnames=list(uniqueIDs,all_columns)))
    for(i in seq(1,length(d1),by=2)){
      content<-d1[[i+1]]
      names(content)<-d1[[i]]
      output[content["uniqueID"],]<-content[all_columns]
    }
    t1<-Sys.time()
    
    if(verbose>0)print(paste("Retrieved",nrow(output),"entries in", signif(as.numeric(difftime(t1,t0,units="mins")),2),"minutes"))
    return(output)
  }
  
  if(type == "R"){
    all_pData<-list()
    for(uniqueID in uniqueIDs){
      pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
      if(file.exists(pDataFile)){
        all_pData[[uniqueID]]<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
      }else{
        if(verbose>0)print(paste("Didn't find a pData file for",uniqueID))	
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
    t1<-Sys.time()
    if(verbose>0)print(paste("Retrieved",nrow(pData),"entries in", signif(as.numeric(difftime(t1,t0,units="mins")),2),"minutes"))
    return(pData)
  }
  
}






remove_snps_from_cache<-function(
  snps
){
  #' remove snps from cache
  #' 
  #' A function that will crawl all uniqueIDs in ~/data and remove specific
  #' snps from the cached file. This is only useful in the case of frequent 
  #' of snp-sets and should not be necessary unless testing or actively developing.
  #' Note that the function have not been updated to handle named-cache files
  #' and should be if ever used for this (e.g. cache.gwas, cache.nonsenser etc)
  #'  
  #' 
  #' @param snps a character vector of SNPs to remove
  #' 

  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  if(class(snps)!="character")stop("snps must be class character")
  if(any(duplicated(snps)))stop("snps must not have duplications")
  if(class(verbose)!="numeric")stop("verbose must be class numeric")
  if(length(verbose)!=1)stop("verbose must be length 1")
  
  
  uniqueIDs<-list.files("/home/ubuntu/data/")
  
  for(uniqueID in uniqueIDs){
    cacheFile<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.gz",sep="")
    if(file.exists(cacheFile)){
      cache<-read.table(cacheFile,header=T,stringsAsFactors=F,row.names=1)
    }else{
      if(verbose>0)print(paste("Didn't find a cache file for",uniqueID))	
      next
    }
    if(any(snps%in%rownames(cache))){
      snpsFound<-snps[snps%in%rownames(cache)]
      if(verbose>0){
        print(paste("removing",	length(snpsFound),"snps from",uniqueID,"- they are:",paste(snpsFound,collapse=","),"and have values",paste(cache[snpsFound,"genotype"],collapse=", ")))
      }
      
      cache<-cache[!rownames(cache)%in%snps,,drop=F]
      
      f<-gzfile(cacheFile,"w")
      write.table(cache,file=f,sep="\t",col.names=NA)
      close(f)
      
    }
    
  }
}







remove_all_temp_folders<-function(
  uniqueIDs=NULL
){
  #' remove all temp folders
  #' 
  #' A function that will crawl all data directories and remove any lingering temp folders - only use with manual execution
  #' 
  #' @param uniqueIDs Optional vector of uniqueIDs to check for temp-folders. Otherwise all uniqueIDs in ~/data are checked.
  
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



remove_all_empty_data_folders<-function(
  uniqueIDs=NULL
){
  #' remove all empty data folders
  #' 
  #' A function that will crawl all data directories and remove any that 
  #' are empty. These can happen on submission errors. Best to just execute 
  #' manually
  #' 
  #' @param uniqueIDs Optional vector of uniqueIDs to check for emptiness. Otherwise all uniqueIDs in ~/data are checked.
  
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






generate_report<-function(
  uniqueIDs=NULL, 
  filename=NULL, 
  updateProgress = NULL
){
  #' generate report
  #' 
  #' A function that will crawl all data directories and generate report with 
  #' various historical data charts for each module.
  #' 
  #' @param uniqueIDs optional selection of which uniqueIDs to investigate (otherwise will just loop over all)
  #' @param filename optional pre-specified filename for output. Otherwise will just have a time-stamped name. Returned by the function as relative_webpath
  #' @param updateProgress a function for tracking progress when running from shiny. If not set, a default function will be used.
  #'   
  #' @return a webpath where the pdf-format of the report can be downloaded
  
  if(class(updateProgress)!="function")stop(paste("updateProgress must be function, not",class(updateProgress)))
  
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files("/home/ubuntu/data/")
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  if(is.null(updateProgress))updateProgress<-function(detail,value,max){return(NULL)}
  if(class(updateProgress)!="function")stop(paste("updateProgress must be function, not",class(updateProgress)))
  
  
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
  
  
  
  #for progressupdates
  stepSize <- 20
  steps<-round(seq(from=1,to=length(uniqueIDs),length.out=stepSize))
  
  for(uniqueID in uniqueIDs){
    
    #updating progress
    # if(which(uniqueIDs%in%uniqueID)%in% steps){
    #   count<-which(uniqueIDs%in%uniqueID)
    #   percentage <- round(count / length(uniqueIDs)*100)
    #   text <- paste0("reading reports:", percentage,"%")
    #   updateProgress(detail = text,value=percentage,max=120)
    # }
    # 
    
    #getting pData file
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
  
  updateProgress(detail = "Plotting values",value=110,max=120)
  
  for(module in sort(unique(user_log[,"modules"]))){
    #only grab for this module
    u1<-user_log[user_log[,"modules"]%in%module,]
    #omit the special-users (e.g. myself)
    u1<-u1[!u1[,"uniqueIDs"]%in%special_ids,]
    if(nrow(u1)==0)next
    
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
  if(any(special_ids%in%u1[,"uniqueIDs"])){
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
  }
  
  
  
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






run_export_script<-function(
  uniqueIDs=NULL,
  modules=NULL, 
  delay=0
  ){
  #' run export script
  #' 
  #' The function that will crawl all module directories and execute the export script if present. This is the linker that makes sure that all modules are calculated
  #' after pre-processing (imputation) of each genome, but it may also be activated
  #' on specific uniqueIDs or modules for testing purposes
  #' 
  #' 
  #' @param uniqueID Indicates if specific sets should be processed
  #' @param modules Indicates if specific modules should be procssed
  #' @param delay An integer, in seconds, giving an optional delay to insert after each uniqueID
  #' 
  #' @return a status message. In addition a json-file with calculated information will be available (or expanded) in the ~/data/<uniqueID> folder as requested
  
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  #load packages (more quietly)
  suppressWarnings(library("jsonlite",warn.conflicts = FALSE)) 
  suppressWarnings(library("shiny",warn.conflicts = FALSE)) 
  
  #check variables ok
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
    if(!all(file.exists(paste("/home/ubuntu/srv/impute-me/",modules,sep=""))))stop("Not all modules given were found")
  }
  
  
  
  for(uniqueID in uniqueIDs){
    if(verbose>0)print(paste0(Sys.time(),": Running export script for ",uniqueID))
    
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
    for(imp in c("uniqueID","first_timeStamp")){
      if(!imp %in%colnames(pData))stop(paste("pData lacked this column:",imp))  
      outputList[[imp]] <-pData[1,imp]
    }
    #outputList[["current_timeStamp"]] <- as.character(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"))
    outputList[["documentation"]] <- list(
      coderepository="https://github.com/lassefolkersen/impute-me",
      mainarticle="https://doi.org/10.3389/fgene.2020.00578",
      releaseupdates="https://twitter.com/imputeme",
      version="Winter 2021 (v1.0.3)"
    )
    
    
    #check if ethnicity is in pData, and if not save it there (because it is needed elsewhere)
    if(!"ethnicity"%in%colnames(pData)){
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
        if(verbose>0)print(paste0(Sys.time(),": Determined and saved ethnicity as: ",ethnicity))
      }else{
        print(paste("Couldn't save ethnicity in pData:",ethnicity))
      }
    }
    
    
    
    #get remaining non-ethnicity modules
    for(module in modules){
      if(!file.info(paste0("/home/ubuntu/srv/impute-me/",module))["isdir"])next
      if("export_script.R" %in% list.files(paste0("/home/ubuntu/srv/impute-me/",module))){
        
        print(paste0(Sys.time(),": Running module ",module," for ",uniqueID))
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
      if(length(previous_unique)>0){
        if(verbose>0)print(paste0(Sys.time(),": Inserting ",length(previous_unique)," modules from existing json: ",paste(names(previous_unique),collapse=", ")))
        outputList<-c(outputList,previous_unique)  
      }
    }
    
    #save new JSON
    JSON<-toJSON(outputList,digits=NA)
    f<-file(filename,"w")
    writeLines(JSON,f)
    close(f)
    
    
    if(delay > 0){
      Sys.sleep(delay)
      
    }
  }
  
  m<-paste0(Sys.time(),": The run_export function was run for ",length(uniqueIDs)," samples on ",length(modules)," modules")
  
  
  return(m)
}







re_check_md5sums<-function(){
  #' re check md5sums
  #' 
  #' Function for the special use case that the md5sum quick-look-up file has
  #' been lost but the stored genomes are not. The function will generate a
  #' a new md5sums.txt file in ~/misc_files
  #' 
  
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











reset_runs_from_node<-function(
  uniqueIDs,
  check_is_running=T
){
  #' reset runs from node
  #' 
  #' function to reset a bulk-run from Node. Useful if there was a crash and 
  #' we need to re-run
  #' 
  #' @param uniqueIDs List of uniqueIDs to reset (can be conveniently grapped from top of each log file in the ~/logs/cron_logs folder)
  #' @check_is_running Check at hub if the jobs are actually running and fail if not.
  #' 
  #' @return No return value, but all specified uniqueIDs will be deleted from this node, and their job-status will have been set to 'Job is ready' at the Hub node.
  
  #define server-role
  serverRole <- get_conf("serverRole")
  hubAddress <- get_conf("hubAddress")
  if(serverRole!="Node")stop("This function only works when running on nodes")
  
  
  
  if(class(uniqueIDs)!="character")stop("uniqueIDs must be a character")
  if(!all(nchar(uniqueIDs)==12))stop("uniqueIDs must be of length 12")
  if(length(grep("^id_",uniqueIDs)) != length(uniqueIDs))stop("all uniqueIDs must start with id_")
  

  #check what exists locally  
  folders_imputation<-sub("imputation_folder_","",list.files("~/imputations/"))
  folders_data<-list.files("~/data/")

  #check if we are requesting uniqueIDs to be deleted even when they are not found on this node. Stop if so.  
  if(!all(uniqueIDs %in% c(folders_data,folders_imputation))){
    missing<-uniqueIDs[!uniqueIDs %in% c(folders_data,folders_imputation)]
    stop(paste0("Aborting with no change. These ",length(missing)," uniqueIDs were not found in local ~/imputations or ~/data folder: c('",paste(missing,collapse="','"),"')"))
  }
  
  #check if there are uniqueIDs that are not requested to be deleted, but still exist in data or imputations. Give a warning (but dont delete them)  
  if(any(!c(folders_data,folders_imputation)%in%uniqueIDs)){
    missing <- c(folders_data,folders_imputation)[!c(folders_data,folders_imputation)%in%uniqueIDs]
    stop(paste0("Aborting with no change. These ",length(missing)," uniqueIDs were found locally in ~/imputations or ~/data folder and, assuming, you are on a node, should probably be deleted: c('",paste(missing,collapse="','"),"')"))
  }
  
  
  if(check_is_running){
    for(uniqueID in uniqueIDs){
      cmd1 <- paste0("ssh ubuntu@",hubAddress," 'cat /home/ubuntu/imputations/imputation_folder_",uniqueID,"/job_status.txt'")
      status<-system(cmd1,intern=T)
      if(status!="Job is remote-running")stop(paste("Status for",uniqueID,"was not remote-running. This must be the case for a reset. Aborting with no change. Status was",status))
    }
  }
  
  
  #check if there are overlaps
  if(length(intersect(folders_imputation,folders_data))>0)stop("There are folders both in ~/imputation and ~/data we haven't seen that before, but should probably be checked manually")
  
  
  if(length(folders_data)>0){
    print(paste("Note that there was",length(folders_data),"folders in ~/data which will also be deleted and reset:",paste(folders_data,collapse=", ")))
  }else{
    print(paste("Deleting",length(uniqueIDs),"uniqueIDs from local ~/imputation folder."))
  }
  
  
  
  
  #check the bulk_imputations folder (can always be deleted)
  bulk_to_delete<-list.files("~/bulk_imputations/")
  if(length(bulk_to_delete)==1){
    print("Also deleting one folder in ~/bulk_imputations")
  }else{
    print(paste("Deleting",length(bulk_to_delete),"folders in ~/bulk_imputations:",paste(bulk_to_delete,collapse=", ")))
  }
  unlink(paste0("~/bulk_imputations/",bulk_to_delete),recursive=T)  
  
  #set job ready tag
  print(paste("Setting Job ready tag for",length(uniqueIDs),"uniqueIDs on hub at:",hubAddress))
  for(uniqueID in uniqueIDs){
    cmd2 <- paste0("ssh ubuntu@",hubAddress," 'echo Job is ready > /home/ubuntu/imputations/imputation_folder_",uniqueID,"/job_status.txt'")
    system(cmd2)
  }

  
  #doing deletion
  if(length(folders_imputation)>0){
    unlink(paste0("~/imputations/imputation_folder_",folders_imputation),recursive=T)  
  }
  if(length(folders_data)>0){
    unlink(paste0("~/data/",folders_data),recursive=T)
  }
  
}









summarize_imputemany_json<-function(
  uniqueIDs, 
  name
  ){
  #' function to check if a given uniqueID is the last in a batch upload, 
  #' and if so summarize all of the uniqueIDs in that batch and send it off
  #' 
  #' @param uniqueIDs A list of uniqueIDs
  #' @param name The name of the output file
  #' 
  #' @return A web-path for an xlsx-file containing the summarized information from the requested uniqueIDs
  
  
  library("jsonlite")  
  library("openxlsx")
  library("tools")
  
  #check uniqueIDs are ok
  if(class(uniqueIDs)!="character")stop(paste("uniqueIDs must be character, not",class(uniqueIDs)))
  if(length(uniqueIDs)<=1)stop(paste("uniqueIDs must be lengh more than 1, not",length(uniqueIDs)))
  missing_files <- vector()
  for(uniqueID in uniqueIDs){
    if(!file.exists(paste0("/home/ubuntu/data/",uniqueID)))missing_files<-c(missing_files,uniqueID)
  }
  if(length(missing_files)>0)  stop(paste("These",length(missing_files),"uniqueIDs were missing:",paste(missing_files,collapse=", ")))
  missing_json <- vector()
  for(uniqueID in uniqueIDs){
    json_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json")
    if(!file.exists(json_path))missing_json<-c(missing_json,uniqueID)
  }
  if(length(missing_json)>0)  stop(paste("JSON files were missing for these",length(missing_json),"uniqueIDs:",paste(missing_json,collapse=", ")))
  
  
  
  
  #in this for-loop we read in all the json data.
  #the strategy is to start with a data.frame with uniqueIDs as rows (since that is constant), and then just add on
  o1 <- data.frame(row.names=uniqueIDs)
  for(uniqueID in uniqueIDs){
    json_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json")
    data<-fromJSON(json_path)
    o1[uniqueID,"ethnicity"]<-data[["ethnicity"]][["guessed_super_pop"]]
    

    #get the original filename from pData
    pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"))
    if(class(pData)!="try-error" && "ethnicity" %in% colnames(pData)){
      o1[uniqueID,"sampleName"]<-pData[1,"filename"]
    }else{
      o1[uniqueID,"sampleName"]<-uniqueID
    }
    
    
    #all GWAS
    for(study_id in names(data[["AllDiseases"]])){
      if(study_id == "documentation")next
      #hack to handle the GRS-ending part (issue #25)
      if(!"GRS"%in%names(data[["AllDiseases"]][[study_id]])){
        o1[uniqueID,study_id]<-data[["AllDiseases"]][[study_id]]
      }else{
        #when fixed, just use this
        o1[uniqueID,study_id]<-data[["AllDiseases"]][[study_id]][["GRS"]]
      }
    }
    
    
    #all ukbiobank
    for(study_id in names(data[["ukbiobank"]])){
      if(study_id == "documentation")next
      o1[uniqueID,study_id]<-data[["ukbiobank"]][[study_id]][["GRS"]]
    }
    
    
    #intelligence
    for(study_id in names(data[["intelligence"]])){
      if(study_id == "documentation")next
      o1[uniqueID,study_id]<-data[["intelligence"]][[study_id]][["GRS"]]
    }
    
    
    #prs
    for(study_id in names(data[["prs"]])){
      if(study_id %in% c("documentation","timing"))next
      o1[uniqueID,study_id]<-data[["prs"]][[study_id]][["GRS"]]
    }
    
    
    #ethnicity
    if("ethnicity"%in%names(data)){
      if("PCA_coordinates"%in%names(data[["ethnicity"]])){
        for(pc in names(data[["ethnicity"]][["PCA_coordinates"]])){
          o1[uniqueID,paste0("ethnicity_",pc)]<-data[["ethnicity"]][["PCA_coordinates"]][[pc]]
        }
      }
    }

    
    #appearance
    for(entry_id in names(data[["guessMyHeight"]])){
      o1[uniqueID,paste0("appearance_",entry_id)]<-data[["guessMyHeight"]][[entry_id]]
    }
  }
  
  o2 <- t(o1)
  
  #add in phenodata for AllDisease
  trait_path1 <- "/home/ubuntu/srv/impute-me/AllDiseases/2021-01-28_trait_overview.xlsx"
  library(openxlsx)
  traits <- read.xlsx(trait_path1,rowNames=T)
  insert_block1 <- traits[rownames(o2),]
  colnames(insert_block1) <- paste0("AllDiseases_",colnames(insert_block1))
  
  #add in phenodata for UKbiobank
  trait_path2 <- "/home/ubuntu/srv/impute-me/ukbiobank/2017-09-28_trait_overoverview.rdata"
  load(trait_path2)
  insert_block2 <- traits[rownames(o2),]
  colnames(insert_block2) <- paste0("ukbiobank_",colnames(insert_block2))
  
  #merge
  o3<-cbind(o2, insert_block1, insert_block2)
  
  #output summary file - the grapper preventer is to prevent fishing for names (since they are just time-stamps)
  grapper_preventer <- paste(sample(LETTERS,8),collapse="")  
  dir_out <- paste0("www/summary_",grapper_preventer,"/")
  file_out <- paste0(dir_out,name,"_summary.xlsx")
  file_out_long <- paste0("/home/ubuntu/srv/impute-me/",file_out)
  if(length(grep("/$",hubAddress))){
    file_out_web <- paste0(hubAddress,file_out)  
  }else{
    file_out_web <- paste0(hubAddress,"/",file_out)
  }
  if(file.exists(dirname(file_out_long)))stop("The grapper preventer generated a random number that already exists")
  dir.create(dirname(file_out_long))
  write.xlsx(o3,file=file_out_long,rowNames=T)
  
  
  Sys.sleep(1)
  
  return(file_out_web)
}






check_for_rare_nonbiallic_snps<-function(
  uniqueID
  ){
  #' check for rare nonbiallic snps
  #' 
  #' There's a rare error type that we've seen maybe 10-11 times now
  #' where a few, like less than 20, rows of the input data have mismatch in
  #' allele-types. Not strand-flip - impossible mis-match. If those rows
  #' are removed it'll process fine, and if audited it typically turns out
  #' it's either updated dbsnp (very rare) or else just seems like typos
  #' in the input. 
  #' This is so rare and so bad that it shouldn't be allowed to run 
  #' by auto-pilot, but the function here should be able to give a better
  #' error output so it can be easily fixed if need be
  
  #verbose doesn't matter, this should always be printed.
  print(paste0(Sys.time(),": ERROR. Starting check_for_rare_nonbiallic_snps for ",uniqueID," - this will result in failure, but hopefully will give informative output."))
  
  #define program paths
  shapeit="/home/ubuntu/programs/shapeit.v2.904.3.10.0-693.11.6.el7.x86_64/bin/shapeit"
  plink="/home/ubuntu/programs/plink"
  impute2="/home/ubuntu/programs/impute_v2.3.2_x86_64_static/impute2"
  sample_ref="/home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"
  
  
  #load libraries
  library("tools")
  library("mailR")
  library("rJava")
  
  #check input
  if(class(uniqueID)!="character")stop(paste("uniqueID must be class character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  basefolder <- paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID)
  if(!file.exists(basefolder))stop(paste("Didn't find imputation folder at",basefolder))
  inputfile_path <- paste0(basefolder,"/",uniqueID,"_raw_data.txt")
  if(!file.exists(inputfile_path))stop(paste("Didn't find inputfile_path at",inputfile_path))
  
  #test that impute2 static can run (or switch to dynamic)
  impute2_blank_run_out <- suppressWarnings(system(impute2,intern=T,ignore.stderr = T) )
  if(attr(impute2_blank_run_out,"status")==139){
    impute2 <- "/home/ubuntu/programs/impute_v2.3.2_x86_64_dynamic/impute2"
  }
  
  #loop over chromosomes
  messages <- vector()
  for(chr in c("X",as.character(1:22))){
    #This is a copy of the real impute run - but with detailed error collection
    #First in loop - extract only one specific chromosome
    cmd2<-paste(plink," --file step_1 --chr ",chr," --recode --out step_2_chr",chr," --exclude step_2_exclusions",sep="")
    out2<-system(cmd2,ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    #if X chromosome is missing it is allowed to skip forward
    if(out2 %in% c(12,13) & chr == "X"){
      print("Didn't find X-chr data, so skipping that")
      next
    }
    
    #Then check for strand flips etc. 
    cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
    system(cmd3,ignore.stdout = TRUE, ignore.stderr = TRUE)
    
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
    
    #This removes any cases where there are more than two alleles involved
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
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    #running the shapeit command (with two people, the right one and a placeholder heterozygote
    cmd4<-paste(shapeit," -check --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/programs/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions",sep="")
    system(cmd4,ignore.stdout = T, ignore.stderr = T)

    #This is the error collection part
    log<-readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
    log_end<-log[[length(log)]]
    if(length(grep("ERROR",log_end))>0){
      messages<-c(messages,paste0("CHR",chr,": ",log_end,"\n"))
    }
    
  }
  
  messages<-c(messages,paste0("\n\n",Sys.time(),"\nError collection run from check_for_rare_nonbiallic_snps for ",uniqueID,". Errors are shown above. Note there's a possibility of multiple errors on one chromosome not being shown, because shapeit only reports the first. If that's the case, probably best to discard the sample.\n\n"))
  
  
  
  #printing to log
  for(message in messages){
    cat(message)
  }
  
  #Then sending as error mail (if possible)
  if(get_conf("from_email_address") != "" & get_conf("from_email_password") != "" & get_conf("error_report_mail")!= ""){
    if(verbose>=0)print(paste0(Sys.time(),": Sending error mail."))
    mailingResult<-try(send.mail(from = get_conf("from_email_address"),
                                 to = get_conf("error_report_mail"),
                                 subject = "An impute-me run has problem",
                                 body = paste(messages,collapse="<br>"),
                                 html=T,
                                 smtp = list(
                                   host.name = "smtp.gmail.com", 
                                   port = 465, 
                                   user.name = get_conf("from_email_address"), 
                                   passwd = get_conf("from_email_password"), 
                                   ssl = TRUE),
                                 authenticate = TRUE,
                                 send = TRUE),silent=T)
    if(class(mailingResult)=="try-error" & verbose > 2)print(paste0(Sys.time(),": Mailing failed."))
    
  }
  
  stop("Ended check_for_rare_nonbiallic_snps run")
  
}











count_x_chr_entries<-function(
  chr
  ){
  #' count x chr entries
  #' 
  #' This is the error where a few of the bulk-imputation run samples
  #' have so few SNPs in a chromosome - typically X-chromosome -
  #' that a specific window fails. It's not big problem, they should
  #' just be run as single-run runs instead. But this function will help
  #' identifying which sample it should be
  #' Note - it'll always result in a fail in the end
  #' 
  #' @param chr the chromosome to check. Will almost always be the X-chromosome, "X", but can be others as well.
  
  library("mailR")
  library("rJava")
  
  #verbose doesn't matter, this should always be printed.
  print(paste0(Sys.time(),": ERROR. Starting count_x_chr_entries for chr",chr," - this will result in failure, but hopefully will give informative output."))

  if(class(chr)!="character")stop(paste("chr must be class character, not ",class(chr)))
  if(length(chr)!=1)stop(paste("chr must be length 1, not ",length(chr)))

  bulk_folder<-list.files("/home/ubuntu/bulk_imputations",full.names=T)
  if(length(bulk_folder)!=1)stop(paste("Found",length(bulk_folder),"bulk folders. The count_x_chr_entries script can only run if there is exactly one."))
  
  ped_path <- paste0(bulk_folder,"/step_2m_chr",chr,".ped")
  if(!file.exists(ped_path))stop(paste("Did not find ped at",ped_path," The count_x_chr_entries script must have a ped file."))
  
  d<-read.table(ped_path,sep=" ",stringsAsFactors = F)
  result<-apply(d[,6:ncol(d)]==0,1,sum)
  names(result) <-d[,1]
  result<-sort(result,decreasing=T)
  
  #Creating output message
  messages<-paste0("The findings from the counting-scan was that chr",chr," had ",ncol(d)-6," entries. The number of missing in each samples is shown here, sorted. Often the cause of the problems is that the (top-most) samples, those with the most missing variants, needs to be run individually instead - not in a bulk-processing run.\n\n")
  for(uniqueID in names(result)){
    messages<-c(messages,paste0(uniqueID,"\n",result[uniqueID],"\n"))
  }
  
  
  #Writing to log
  for(message in messages){
    cat(message)
  }
  
  #Then sending as error mail (if possible)
  if(get_conf("from_email_address") != "" & get_conf("from_email_password") != "" & get_conf("error_report_mail")!= ""){
    if(verbose>=0)print(paste0(Sys.time(),": Sending error mail."))
    mailingResult<-try(send.mail(from = get_conf("from_email_address"),
                                 to = get_conf("error_report_mail"),
                                 subject = "An impute-me run has problem",
                                 body = paste(messages,collapse="<br>"),
                                 html=T,
                                 smtp = list(
                                   host.name = "smtp.gmail.com", 
                                   port = 465, 
                                   user.name = get_conf("from_email_address"), 
                                   passwd = get_conf("from_email_password"), 
                                   ssl = TRUE),
                                 authenticate = TRUE,
                                 send = TRUE),silent=T)
    
    if(class(mailingResult)=="try-error" & verbose > 2)print(paste0(Sys.time(),": Mailing failed."))
    
    
  }
  stop("count_x_chr_entries")
}










check_genome_build<-function(map_file){
  #' check genome build
  #' 
  #' A function that quickly checks if a map file (plink format) is in the hg19 build, using a
  #' small set of SNPs with hg19-position, known to almost always be included in input files
  #' 
  #' @param map_file The file that needs to be checked (plink format mainly, but also checks for and accepts many raw DTC formats)
  
  #checking map file
  if(class(map_file)!="character")stop(paste("map_file must be character, not",class(map_file)))
  if(length(map_file)!=1)stop(paste("map_file must be lengh 1, not",length(map_file)))
  if(!file.exists(map_file))stop(paste("Did not find an map_file at ",map_file))
  
  
  #defining canaries
  canaries<-rbind(
    c("rs3762954","662955"),
    c("rs390560","2601689"),
    # c("rs10043332","3128346"), #often gets double location in 23andme data
    # c("rs10070917","4955950"), #often gets double location in 23andme data
    c("rs11740668","404623"),
    c("rs999292","93218958"),
    c("rs13147822","107960572"),
    c("rs62574625","101218552"),
    c("rs11023374","14903636"),
    c("rs35241590","904752"),
    c("rs114037461","15362682"),
    c("rs189127468","5322361"),
    c("rs2160077","92428410"),
    c("rs7119167","73228685"),
    c("rs7952067","100421472"),
    c("rs2503099","100610101"),
    c("rs7869279","114454544"),
    c("rs6548616","79399575"),
    c("rs868351238","74703054"),
    c("rs2071699","49254504"),
    c("rs817771","98225036"),
    c("rs61200250","120608075"),
    c("rs1020770","85595645")
    
  )
  canaries<-data.frame(canaries,stringsAsFactors = F)
  colnames(canaries)<-c("rsid","1kg_pos_hg19")
  canaries[,"1kg_pos_hg19"] <- as.numeric(canaries[,"1kg_pos_hg19"])
  
  
  
  #extract lines by rs-id from map file
  cmd1 <- paste0("grep -E '",paste(paste(canaries[,"rsid"],"\t",sep=""),collapse="|"),"' ",map_file)
  d<-suppressWarnings(system(cmd1,intern=T))
  if(length(d)==0){
    d1<-data.frame(chr=vector(),rsid=vector(),cm=vector(),pos=vector(),stringsAsFactors = F)
  }else{
    d1<-data.frame(do.call(rbind,strsplit(d,"\t")),stringsAsFactors = F)
    
    #now its a little tricky because we have to determine the format
    if(length(grep("^rs",d1[,2]))>0){ #then the second column is rsid, like in plink
      colnames(d1)<-c("chr","rsid","cm","pos")
    }else if(length(grep("^rs",d1[,1]))>0){ #then the first column is rsid, like in 23andme/ancestrycom etc
      colnames(d1)<-c("rsid","chr","pos","genotype")
    }else{
      stop("Problem with assigning format in check_genome_build")
    }
  }
  
  
  #compare with canaries
  d2<-merge(d1,canaries,by="rsid")
  
  
  if(nrow(d2)==0){
    message<-"no snps for built check, so not performed. May want to add more canaries in the future."
  }else{
    if(all(d2[,"pos"]==d2[,"1kg_pos_hg19"])){
      message<-paste("passed built check with",nrow(d2),"snps")
    }else{
      message<-paste("failed built check with",sum(d2[,"pos"]==d2[,"1kg_pos_hg19"]),"of",nrow(d2),"snps matching hg19-positions")
    }
  }
  return(message)
}




