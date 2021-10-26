


set_conf<-function(
  request,
  value=NULL
){
  #' set configuration parameters
  #' 
  #' Function that sets the environment variables, either from defaults
  #' individually or from a file ()
  #' 
  #' @param request the name of the requested variable to set. Special values are 'defaults' and 'set_from_file'.
  #' @param value the value to assign to the requested variable. For set_from_file it is the path of the file to read in.
  #' 
  
  default_configuration<-list(
    "max_imputations_per_node"=1,           #the max number of parallel imputations to run
    "max_imputations_in_queue" =200,           #the max number of imputations allowed waiting in a queue
    "server_role"='Hub',           #the role of this computer, can be either Hub or Node
    "hub_address"='',           #if server_role is Node, then the IP-address of the Hub is required
    "from_email_password"='',           #optional password for sending out emails
    "from_email_address"='',           #optional email-address/username for sending out emails
    "routinely_delete_this"='',           #delete these parts in routine 14-day data deletion. May put in 'link' and/or 'data', which is the default online. But docker-running defaults to not deleting anything.
    "paypal"='https://www.paypal.me/lfolkersenimputeme/5',           #suggest to donate to this address in emails
    "bulk_node_count"=1,           #count of bulk-nodes, used only for calculating timings in receipt mail
    "error_report_mail"='',           #optional email-address to send (major) errors to
    "seconds_wait_before_start"=0,           #a delay that is useful only with CPU-credit systems
    "running_as_docker"=TRUE,           #adapt to docker running
    "autostart_supercronic"=TRUE,            #Attempt to auto-start cron-jobs with the prepare_individual_genome function (makes it easier for casual docker users, but may confuse things in pipeline running)
    "max_imputation_chunk_size"=1000,           #how much stuff to put into the memory in each chunk. Lower values results in slower running with less memory-requirements.
    "minimum_required_variant_in_vcf_count"=200000,    #The least amount of matching variants acceptable for passing as a whole-genome sequencing vcf
    "block_double_uploads_by_md5sum"=FALSE,           #If the upload interface should give an error when the exact same file is being uploaded twice (by md5sum).
    "plink_algorithms_to_run"='SCORESUM_PLINK_2_0_DOSAGE_MATRIX',           #What types of plink algorithms to run (see prs module export_script.R for details)
    "modules_to_compute"='AllDiseases,autoimmuneDiseases,BRCA,drugResponse,ethnicity,rareDiseases,ukbiobank,prs',           #Select the modules to pre-run (defaults to all folders in the repo if set to NULL).
    "cron_logs_path"='/home/ubuntu/logs/cron_logs/',           #Path to write cron-logs to. Can be empty.
    "submission_logs_path"='/home/ubuntu/logs/submission/',           #Path to write submission logs to (information about user errors, etc). Can be empty.
    "shiny_logs_path"='/home/ubuntu/logs/shiny/',           #Path to write shiny-logs to (the shiny-server system's logs). Can be empty.
    "misc_files_path"='/home/ubuntu/misc_files/',           #Path to misc-files to (e.g. md5sums, height-scores, imputemany-registry etc).  Can be empty, but then the default configuration.R will be written.
    "data_path"='/home/ubuntu/data/',           #Path to write processed final data (imputed bulk data, prs-jsons, etc)
    "programs_path"='/imputeme/programs/',           #Path where to look for programs and imputation-reference. Is not changed during processing and rarely in updates, and must be pre-loaded.
    "prs_dir_path"='/imputeme/prs_dir/',           #Path where to look for prs-weights and frequency data. Is not changed during processing but often in updates, and must be pre-loaded.
    "imputations_path"='/home/ubuntu/imputations/',           #Landing folder for microarray data to be imputed. Can be empty, must be writeable.
    "vcfs_path"='/home/ubuntu/vcfs/',           #Landing folder for sequencing data to be processed without imputation. Can be empty, must be writeable.
    "code_path"='/imputeme/code/impute-me/',           #Path of the entire impute-me github code repository. Cannot be empty. Must be writeable for the ./www/ folder exposition-system through the shiny-server to work, otherwise immutable.
    "uploads_for_imputemany_path"='/home/ubuntu/uploads_for_imputemany/',           #Path to where uploads to imputemany are saved before being split
    "bulk_imputations_path"='/home/ubuntu/bulk_imputations/',           #Working folder for bulk imputation
    "verbose"=1           #how much info to put into logs (min 0, max 10)
  )

  #set all according to list above  
  if(request == "defaults"){
    if(!is.null(value))stop("When setting defaults, value must be given as NULL")

    do.call(Sys.setenv, default_configuration)
    
    if(default_configuration[["verbose"]]>1){
      print(paste0(Sys.time(),": Setting ",length(default_configuration)," environment variables. Any previous values were overwritten."))
    }
    
  #set all according to file given as value  
  }else if(request == "set_from_file"){
    if(!file.exists(value))stop("Setting defaults from file, but can't find the file at the path given by value")
    source(value)    
    missing<-vector()
    #loop over read in parameters, register which is missing and change the rest in default list from above 
    for(i in 1:length(default_configuration)){
      if(exists(names(default_configuration)[i])){
        default_configuration[[i]] <- get(names(default_configuration)[i])  
      }else{
        missing<-c(missing,names(default_configuration)[i])    
      }
    }
    do.call(Sys.setenv, default_configuration)
    
    #report changes
    if(default_configuration[["verbose"]]>3){
      if(length(missing)>0){
        print(paste0(Sys.time(),": Setting ",length(default_configuration)-length(missing)," environment variables from file ",value,". Setting ",length(missing)," values to hard-coded defaults because they were missing: ",paste(missing,collapse=", ")))  
      }else{
        print(paste0(Sys.time(),": Setting ",length(default_configuration)-length(missing)," environment variables from file ",value))
      }
    }
    
  }else{
    if(is.null(value))stop("Can't set_conf a value of NULL")
    if(!request %in% names(default_configuration))stop(paste0("Didn't recognize ",request," as an imputeme environment variable. Please select only one of: ", paste(sort(names(default_configuration)),collapse=", ")))
    previous_value<-Sys.getenv(request)
    names(value)<-request
    do.call(Sys.setenv, as.list(value))
    print(paste0(Sys.time(),": Setting environment variable ",request," to ",value,". Previous value was ",previous_value))
  }
}

get_conf<-function(
  request
){
  #' get configuration parameters
  #' 
  #' Function that reads the environment variables, reformats to R-classes, and
  #' makes choices wether to stop for missing values 
  #' 
  #' 
  #' @param request the name of the requested variable
  #' 
  #' @return The value of the requested variable, either from the configuration_file or as a default value if not available in configuration file.


  #always import verbose
  verbose<-as.numeric(Sys.getenv("verbose"))
  
  #reform class as necessary
  #numeric
  if(request %in% c("max_imputations_per_node","max_imputations_in_queue","bulk_node_count","seconds_wait_before_start","max_imputation_chunk_size","verbose","minimum_required_variant_in_vcf_count")){
    assign(request,as.numeric(Sys.getenv(request)))
  #logical
    }else if(request %in% c("running_as_docker","block_double_uploads_by_md5sum","autostart_supercronic")){
    assign(request,as.logical(Sys.getenv(request)))
  }else if(request %in% c("modules_to_compute","plink_algorithms_to_run","routinely_delete_this")){
  #comma-separated lists to vectors
    assign(request,strsplit(Sys.getenv(request),",")[[1]])
  #character
  }else(
    assign(request,Sys.getenv(request))
  )
  
    
  #then do checkups for each of the variables (could probably be skipped later, but checks were already
  #written)
  if(request == "verbose"){
    #already done
  }else if(request == "max_imputations_per_node"){
    if(!is.numeric(max_imputations_per_node))stop("max_imputations_per_node not numeric")
    if(length(max_imputations_per_node)!=1)stop("max_imputations_per_node not length 1")
    if(max_imputations_per_node <= 0 | max_imputations_per_node > 1000)stop("max_imputations_per_node must be between 1 and 1000")
  }else if(request == "max_imputations_in_queue"){
    if(!is.numeric(max_imputations_in_queue))stop("max_imputations_in_queue not numeric")
    if(length(max_imputations_in_queue)!=1)stop("max_imputations_in_queue not length 1")
    if(max_imputations_in_queue <= 0 | max_imputations_in_queue > 100000)stop("max_imputations_in_queue must be between 1 and 100000")
  }else if(request == "server_role"){
    if(!is.character(server_role))stop("server_role not character")
    if(length(server_role)!=1)stop("server_role not length 1")
    if(!server_role%in%c("Hub","Node"))stop("server_role not Hub or Node")
  }else if(request == "hub_address"){
    if(!is.character(hub_address))stop("hub_address not character")
    if(length(hub_address)!=1)stop("hub_address not length 1")
  }else if(request == "from_email_password"){
    if(!is.character(from_email_password ))stop("from_email_password  not character")
    if(length(from_email_password )!=1)stop("from_email_password  not length 1")
  }else if(request == "from_email_address"){
    if(!is.character(from_email_address))stop("from_email_address not character")
    if(length(from_email_address)!=1)stop("from_email_address not length 1")
  }else if(request == "routinely_delete_this"){
    if(!is.character(routinely_delete_this))stop("routinely_delete_this not character")
    if(!all(routinely_delete_this%in%c("link","data")))stop("routinely_delete_this not equal to link, data or both")
  }else if(request == "paypal"){
    if(!is.character(paypal))stop("paypal not character")
    if(length(paypal)!=1)stop("paypal not length 1")
  }else if(request == "bulk_node_count"){
    if(!is.numeric(bulk_node_count ))stop("bulk_node_count not numeric")
    if(length(bulk_node_count)!=1)stop("bulk_node_count not length 1")
    if(bulk_node_count <= 0 | bulk_node_count > 100)stop("bulk_node_count must be between 1 and 100")
  }else if(request == "error_report_mail"){
    if(!is.character(error_report_mail))stop("error_report_mail not character")
    if(length(error_report_mail)!=1)stop("error_report_mail not length 1")
  }else if(request == "seconds_wait_before_start"){
    if(!is.numeric(seconds_wait_before_start))stop("seconds_wait_before_start not numeric")
    if(length(seconds_wait_before_start)!=1)stop("seconds_wait_before_start not length 1")
  }else if(request == "running_as_docker"){
    if(!is.logical(running_as_docker))stop("running_as_docker not logical")
    if(length(running_as_docker)!=1)stop("running_as_docker not length 1")
  }else if(request == "autostart_supercronic"){
    if(!is.logical(autostart_supercronic))stop("autostart_supercronic not logical")
    if(length(autostart_supercronic)!=1)stop("autostart_supercronic not length 1")
  }else if(request == "max_imputation_chunk_size"){
    if(!is.numeric(max_imputation_chunk_size))stop("max_imputation_chunk_size not numeric")
    if(length(max_imputation_chunk_size)!=1)stop("max_imputation_chunk_size not length 1")
  }else if(request == "minimum_required_variant_in_vcf_count"){
    if(!is.numeric(minimum_required_variant_in_vcf_count))stop("minimum_required_variant_in_vcf_count not numeric")
    if(length(minimum_required_variant_in_vcf_count)!=1)stop("minimum_required_variant_in_vcf_count not length 1")
    if(minimum_required_variant_in_vcf_count < 1000)stop("minimum_required_variant_in_vcf_count must be at least 1000")
  }else if(request == "block_double_uploads_by_md5sum"){
    if(!is.logical(block_double_uploads_by_md5sum))stop("block_double_uploads_by_md5sum not logical")
    if(length(block_double_uploads_by_md5sum)!=1)stop("block_double_uploads_by_md5sum not length 1")
  }else if(request == "modules_to_compute"){
    if(!is.character(modules_to_compute))stop("modules_to_compute not character")
    if(!all(modules_to_compute%in%list.files(get_conf("code_path"))))stop(paste("A request was made for modules_to_compute that were not found in ",get_conf("code_path"),":",paste(modules_to_compute,collapse=", ")))
  }else if(request == "plink_algorithms_to_run"){
    if(!is.character(plink_algorithms_to_run))stop("plink_algorithms_to_run not character")
  }else if(request == "cron_logs_path"){
    if(!is.character(cron_logs_path))stop("cron_logs_path not character")
    if(length(cron_logs_path)!=1)stop("cron_logs_path not length 1")
  }else if(request == "submission_logs_path"){
    if(!is.character(submission_logs_path))stop("submission_logs_path not character")
    if(length(submission_logs_path)!=1)stop("submission_logs_path not length 1")
  }else if(request == "shiny_logs_path"){
    if(!is.character(shiny_logs_path))stop("shiny_logs_path not character")
    if(length(shiny_logs_path)!=1)stop("shiny_logs_path not length 1")
  }else if(request == "misc_files_path"){
    if(!is.character(misc_files_path))stop("misc_files_path not character")
    if(length(misc_files_path)!=1)stop("misc_files_path not length 1")
  }else if(request == "data_path"){
    if(!is.character(data_path))stop("data_path not character")
    if(length(data_path)!=1)stop("data_path not length 1")
  }else if(request == "programs_path"){
    if(!is.character(programs_path))stop("programs_path not character")
    if(length(programs_path)!=1)stop("programs_path not length 1")
  }else if(request == "prs_dir_path"){
    if(!is.character(prs_dir_path))stop("prs_dir_path not character")
    if(length(prs_dir_path)!=1)stop("prs_dir_path not length 1")
  }else if(request == "imputations_path"){
    if(!is.character(imputations_path))stop("imputations_path not character")
    if(length(imputations_path)!=1)stop("imputations_path not length 1")
  }else if(request == "vcfs_path"){
    if(!is.character(vcfs_path))stop("vcfs_path not character")
    if(length(vcfs_path)!=1)stop("vcfs_path not length 1")
  }else if(request == "code_path"){
    if(!is.character(code_path))stop("code_path not character")
    if(length(code_path)!=1)stop("code_path not length 1")
  }else if(request == "uploads_for_imputemany_path"){
    if(!is.character(uploads_for_imputemany_path))stop("uploads_for_imputemany_path not character")
    if(length(uploads_for_imputemany_path)!=1)stop("uploads_for_imputemany_path not length 1")
  }else if(request == "bulk_imputations_path"){
    if(!is.character(bulk_imputations_path))stop("bulk_imputations_path not character")
    if(length(bulk_imputations_path)!=1)stop("bulk_imputations_path not length 1")
  }else if(request == "version"){
    #note this is special - since it is *not* taken from configuration file, but hard-coded
    version <- "v1.0.6"
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
  library("tools")
  suppressWarnings(library("shiny"))
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  #check that input path is ok and accesible
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be length 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  if(substr(path,1,1)!="/")path<-normalizePath(path)
  
  #Check filename is ok and set to basename(path) if not given
  #the actual file will anyway be renamed as <uniqueID>_raw_data.txt so it is not an 
  #essential variable (except the file-ending), but it does need to be cleaned for special characters 
  #for storing and mailing back to users
  if(is.null(filename))filename<-basename(path)
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be length 1, not",length(filename)))
  filename<-gsub("\\ ","_",filename)
  filename<-gsub("[\\$\\&\\+\\,\\:\\;\\=\\?\\@\\#\\\"\\\']","",filename)
  
  #check if this sample should be protected_from_deletion
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be length 1, not",length(protect_from_deletion)))
  
  #check the updateProgress object - set to a NULL-returning function if not given.
  if(is.null(updateProgress))updateProgress<-function(detail,value,max){return(NULL)}
  if(class(updateProgress)!="function")stop(paste("updateProgress must be function, not",class(updateProgress)))
  
  #check the user-inputted email, set to the default error_report_mail if not given
  if(is.null(email))email<-get_conf("error_report_mail")
  if(class(email)!="character")stop(paste("email must be character, not",class(email)))
  if(length(email)!=1)stop(paste("email must be length 1, not",length(email)))
  if(!get_conf("running_as_docker")){
    if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
      stop(safeError(paste("a real email adress is needed:",email)))
    }
  }  
  
  
  #updating progress
  updateProgress(detail = "Check mail, check queue, check md5sum",value=1,max=4)
  
  
  #check for too many ongoing imputations
  if(verbose>0)print(paste0(Sys.time(),": Checking for too many ongoing imputations"))
  s<-list.files(get_conf("imputations_path"))
  if(length(grep("^imputation_folder",s)) >= get_conf("max_imputations_in_queue")){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_many_jobs",email,length(grep("^imputation_folder",s)))
    m<-paste(m,collapse="\t")
    write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
    
    stop(safeError(paste("Too many imputations are already in progress. Cannot start a new one. The only solution to this is to wait a few days until the queues are shorter.")))
  }
  
  #check for vcf file
  
  if(length(grep("\\.vcf\\.gz$",filename))==1 | length(grep("\\.vcf$",filename))==1 | length(grep("fileformat=VCF",readLines(path,n=1)))>0){
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
  if(uniqueID%in%list.files(get_conf("data_path"))){  #Also check for pre-existing uniqueIDs and stop if so (never happened though)
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
    stop(safeError("Problem with unique ID generation. Please re-load and try again."))
  }
  if(verbose>0)print(paste0(Sys.time(),": Created uniqueID ",uniqueID))
  
  
  
  
  #This blocks prepares a microarray-based sample directly for imputation
  #the next if-block will instead prepare a vcf-based sample in a separate vcf-area
  if(!is_vcf_file){
    
    #create imputation folder.
    if(verbose>0)print(paste0(Sys.time(),": Create imputation folder for ",uniqueID))
    homeFolderShort<-paste("imputation_folder",uniqueID,sep="_")
    homeFolder<-paste0(get_conf("imputations_path"),homeFolderShort,"/")
    dir.create(homeFolder,recursive=T)
    write.table("Job is not ready yet",file=paste0(homeFolder,"job_status.txt"),col.names=F,row.names=F,quote=F)
    
    
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
            write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
            stop(safeError("This file looked like a FTDNA file but could not accurately be handled as such. This is a rare error, but we unfortunately cannot proceed."))
          }
          
          #if it's a non FTDNA gz file we fail it
        }else{
          if(!protect_from_deletion)unlink(homeFolder,recursive=T)
          m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"gzip_file",email,uniqueID,filename)
          m<-paste(m,collapse="\t")
          write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
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
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)	
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't seem like genomic data at all. It must contain many rows, one per SNP, with information about your genotype. Please write an email if you think this is a mistake and that this file format should be supported."))
    }
    
    
    #updating progress
    updateProgress(detail = "Consistency checks",value=2,max=4)
    
    
    #checking if there is at least 100k lines (otherwise imputation wouldn't be possible anyway)
    cmd1 <- paste0("wc -l ",path)
    lines<- as.numeric(sub(" .+$","",system(cmd1,intern=T)))
    if(lines < 100000){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_few_lines_error",email,uniqueID)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("Your file only had ",lines," lines. That doesn't look like a genome-wide microarray input file. Genome-wide microarray files have many formats and come from many places (23andme, myheritage, ancestrycom, etc), but they always have hundreds of thousands of measurements")))
    }
    
    #running the alternative format converters
    if(ncol(testRead)==5){
      #This could be an ancestry.com file. Check that first
      testRead2<-read.table(path,nrow=10,stringsAsFactors=F,header=T)
      if(unique(sub("[0-9]+$","",testRead2[,1])[1])!="rs"){
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"ancestry_problem",email,uniqueID)
        m<-paste(m,collapse="\t")
        write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)	
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
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't seem to match any of our import algorithms. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
    }
    
    
    #after reformat attempts, perform one more test read and consider
    testRead2<-read.table(path,nrow=10,stringsAsFactors=F)
    if(ncol(testRead2)!=4){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_4_columns",email,uniqueID)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't have 4 columns (or 5 for ancestry.com data). If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
    }
    if(unique(sub("[0-9]+$","",testRead2[,1])[2])!="rs"){
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"test_read_no_rs_id",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't have rs IDs in column 1, line 2. If you think this data type should be supported, then you are welcome to write an email and attach a snippet of the data for our inspection."))
    }
    
    
    
    
    
  #This block prepares a vcf file in a separate vcf-file holding area for later processing
  }else if(is_vcf_file){
    #create vcf folder
    if(verbose>0)print(paste0(Sys.time(),": create vcf folder for ",uniqueID))
    homeFolderShort<-paste("vcf_folder",uniqueID,sep="_")
    homeFolder<-paste0(get_conf("vcfs_path"),homeFolderShort,"/")
    dir.create(homeFolder,recursive=T)
    write.table("Job is not ready yet",file=paste0(homeFolder,"job_status.txt"),col.names=F,row.names=F,quote=F)
    
    
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
        write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
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
    if(verbose>0)print(paste0(Sys.time(),": checking if it is a consistent file"))
    testRead<-try(read.table(path,nrow=100,stringsAsFactors=F))
    if(class(testRead)=="try-error"){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"general_data_file_problem_vcf",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("Your file didn't seem like genomic data at all. The filename ended in vcf, so we expected a vcf file, but this did not seem to be the case."))
    }
    
    
    #read header
    testReadHeader<-try(readLines(path,n=250))

    
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
        write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      }
    }
    if(overrule_vcf_checks)escape_checks<-TRUE
    
    
    
    #check header
    if(!testReadHeader[1] %in% c("##fileformat=VCFv4.2","##fileformat=VCFv4.1","##fileformat=VCFv4.0") && !escape_checks){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"wrong_starting_vcf_header",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("The first header line of your VCF didn't have exactly '##fileformat=VCFv4.2'. It had ",testReadHeader[1],". This is required, not because these exact columns are important, but because the importer strives to avoid any custom-format imports at all. See the github issue #32 for further discussion (https://github.com/lassefolkersen/impute-me/issues/32). You may also try to follow the cookbook-recipe in this discussion in order to convert your file into microarray-like data, and then retry upload.")))
    }
    
    
    
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
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("Your vcf-file was too small to be accepted. Currently, we only accept vcf files that are larger than ",size_requirement/(1024*1024)," GB (gzipped). Your submitted data was ",signif(size/(1024*1024),3)," GB.  This is done to avoid accidentally processing exon-sequencing data, which would be a problem for most subsequent algorithms. They all require genome-wide data, because the vcf-handling is done without imputation. You can read more about why that is, at http://doi.org/10.13140/RG.2.2.34644.42883. You may also find some tips on converting exon-sequencing data to microarray-like data-format in github issue #32 https://github.com/lassefolkersen/impute-me/issues/32. After doing this, you may resubmit your file and it will be run through the imputation-algorithm. It is possible, although un-tested, that some useful information can be extraxted when using this approach.")))
    }
    
    
    
    
    #Check and block the word 'indels' in the filename (common error from Dante labs uploads - usually caught by size-requirement but not always )
    if(length(grep("indel",filename,ignore.case=T))>0  && !escape_checks){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"vcf_indel_name_error",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("The vcf reading algorithm found the word 'indel' in the file name. Typically vcf files should contain both single nucleotide variants and indels, but if they only contain indels the calculations will fail. If this data is from Dante labs, check that you use the file with 'SNPs' or 'combined' in the name instead. If you are sure this file is correct, you may simply rename it to not include the word indel, and retry upload."))
    }
    
    
    
    
    #Try to establish genome build from header
    grch37_hits<-grep("grch37|hg19",testReadHeader,ignore.case = TRUE,value=TRUE)
    grch38_hits<-grep("grch38|hg38",testReadHeader,ignore.case = TRUE,value=TRUE)
    if(length(grch37_hits)>0 & length(grch38_hits)>0 ){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"vcf_both_builds_error",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("The vcf reading algorithm found references to both genome build grch37 and grch38 in the vcf header and was unsure how to proceed. Your data have been deleted. If you want to re-submit the data and try again, we recommend that you edit the header of your vcf file such that there are only references to either grch37/hg19 or grch38/hg38, as the case may be."))
    }
    
    #Can be inserted if you don't want to have grch38 blocks (but it's not a very good filter, only scans header)
    # if(length(grch38_hits)>0){
    #   m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"vcf_grch38_not_allowed",email,uniqueID,filename)
    #   m<-paste(m,collapse="\t")
    #   write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
    #   if(!protect_from_deletion)unlink(homeFolder,recursive=T)
    #   stop(safeError("The vcf reading algorithm determined this data to be from the GRCh38 genome build. We are sorry, but this genome build is not supported yet. In the meantime, you may look into doing a liftover to GRCh37 yourself using tools such as Picard LiftoverVcf. Just remember to delete any references to GRCh38 in the vcf-header if you do so."))
    # }
    
    
    
    #check that first line column 9 is consistent
    #There is a lot of fine-tuning going into this demand, here's some preliminary observations.
    # First: we dislike formats that don't have DP, since it prevents filtering out low-pass sequencing samples
    # That's ok for companies like Dante lab (GT:AD:AF:DP:F1R2:F2R1:GQ:PL:GP:PRI:SB:MB) and Nebula (GT:AD:DP:GQ:PGT:PID:PL:PS)
    # That's a problem for the following companies that only report the GT field:
    # genotek.ru, estonian biobank and sequencing.com ("ultimate DNA test kit"). This is a pity, since
    # it seems the data is of ok quality otherwise. Possibly one could write a GT-only
    # catching mechanism for them. For now we just fail them. At the benefit of not majorly
    # messing up some low-pass or exon-seq submission.
    allowed_formats <- c("GT:AD:AF:DP:F1R2:F2R1:GQ:PL:GP:PRI:SB:MB","GT:AD:DP:GQ:PGT:PID:PL:PS","GT:AD:DP:GQ:PL","GT:AD:DP:GQ:PGT:PID:PL")
    if(!testRead[1,9] %in% allowed_formats  && !escape_checks){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"wrong_starting_vcf_line",email,uniqueID,filename,testRead[1,9])
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("The first line of the 'format'-column of your VCF  had ",testRead[1,9],". We currently only allow vcf-files with very specific formatting-requirements, corresponding to Dante labs and Nebula. This is necessary in order to avoid grave mistakes with custom-formatted e.g. exon sequencings and also to be able to test the read-depth (the DP format entry). See github issue #32 for further discussion. In this discussion you may also find suggestions on alternative approaches to submitting your file for processing. The currently allowed format-column entries include: ",paste(allowed_formats,collapse=", "))))
    }
    
    
    #check read-depth
    minimum_mean_depth_of_first_hundred_reads <- 10 #would be nice to have genome-wide average depth, since the first chunk of reads in testRead could be hard to sequence regions. But it's not possible to read the entire vcf in browsing-time, which is required for these format rejection-checks
    format<-try(strsplit(testRead[1,9],":")[[1]],silent=T)
    depth_index <- try(which(format%in%"DP"),silent=T)
    depths<-try(as.numeric(sapply(strsplit(testRead[,10],":"),function(x,depth_index){x[depth_index]},depth_index)),silent=T)
    if((any(is.na(depths)) || class(depths) == "try-error" || mean(depths,na.rm=T) < minimum_mean_depth_of_first_hundred_reads)  && !escape_checks){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_low_vcf_depth",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError(paste0("The average depth (DP) entry of the first hundred lines of your vcf file was not at least ",minimum_mean_depth_of_first_hundred_reads," which is required. Low-coverage sequencing will not work in the down-stream algorithms.")))
    }
  }
  
  
  ##checking if this job has not actually been run before
  if(get_conf("block_double_uploads_by_md5sum")){
    if(verbose>0)print(paste0(Sys.time(),": checking if this job has not actually been run before"))
    this_person_md5sum <- md5sum(path)
    all_md5sums_path<-paste0(get_conf("misc_files_path"),"md5sums.txt")
    if(!file.exists(all_md5sums_path)){write("md5sums",file=paste0(get_conf("misc_files_path"),"md5sums.txt"))}
    all_md5sums<-read.table(all_md5sums_path,sep="\t",stringsAsFactors = F)[,1]
    if(this_person_md5sum %in% all_md5sums){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"md5sum_match",email,this_person_md5sum,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
      if(!protect_from_deletion)unlink(homeFolder,recursive=T)
      stop(safeError("A person with this genome was already analyzed by the system. If this is an error, you can try to re-upload a new version of your DNA-data, e.g. you can go to your data provider (23andme, ancestry.com) and re-download the data you are interested. Then try that file. There will be no block flag for such new file, because any edits to the file will make the system unable to recognize it as a previously analyzed genome."))
    }
    write(this_person_md5sum,file=paste0(get_conf("misc_files_path"),"md5sums.txt"),append=TRUE)			
  }
  
  #finalizing: 
  #1) saving the small job_status.txt that just shows a status for the node/hub setup to read quickly over ssh
  #2) saving the variables.rdata which is a file of processing-specific parameters that is needed in first process, but afterwards deleted
  if(verbose>0)print(paste0(Sys.time(),": Finalize prepare_individual_genome"))
  imputemany_upload <- FALSE
  should_be_imputed <- TRUE
  upload_time<-format(Sys.time(),"%Y-%m-%d-%H-%M-%S")
  save(uniqueID,email,filename,protect_from_deletion,is_vcf_file,imputemany_upload,should_be_imputed,upload_time,file=paste0(homeFolder,"variables.rdata"))
  unlink(paste0(homeFolder,"job_status.txt"))
  write.table("Job is ready",file=paste0(homeFolder,"job_status.txt"),col.names=F,row.names=F,quote=F)
  
  
  #updating progress
  updateProgress(detail = "Unzipping, sending receipt mail",value=3,max=4)
  
  
  
  #If possible, send off a mail as a receipt of data
  if(get_conf("from_email_address") != "" & get_conf("from_email_password") != "" & get_conf("error_report_mail")!= ""){
    queue_length <- length(list.files(get_conf("imputations_path")))
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
    
    
    #Send receipt mail
    suppressWarnings(library("gmailr",warn.conflicts = FALSE))
    gm_auth_configure( path =paste0(get_conf("misc_files_path"),"mailchecker.json"))
    gm_auth(email=get_conf("from_email_address"),cache=paste0(get_conf("misc_files_path"),"mail_secret"))
    prepared_email <- try(gm_mime() %>%
                            gm_to(email) %>%
                            gm_from(get_conf("from_email_address")) %>%
                            gm_subject("Imputation is queued") %>%
                            gm_html_body(message))
    mailingResult<-try(gm_send_message(prepared_email))
    
    if(class(mailingResult)=="try-error"){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"mailing_error",email,uniqueID,filename)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
    }
  }
  
  #if autostart_supercronic, then kill previous supercronic jobs and restart
  if(get_conf("autostart_supercronic")){
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
    if(verbose>=0 & !file.exists(paste0(get_conf("programs_path"),"supercronic.txt")))print(paste0(Sys.time(),": Code was found to be running as docker container, but with no supercronic.txt file ready for launch. The job needs to be executed manually."))
    #it's possible that the ending ampersand is not a good idea. The problem is that when the
    #docker is running as a web-interface it works well. But when the function is called from outside of
    #the docker, with docker exec - then supercronic deletes itself after a few seconds. It can be
    #kept with e.g. a Sys.sleep() argument. But that would destroy the feedback on the website (port 3838-based)
    #and there's no easy way to tell which place invoked the command. So right now the web
    #interface "wins" because that's for more casual users. Then super users can
    #separately call and start the supercronic
    supercronic_out<-try(system(paste0("supercronic ",get_conf("programs_path"),"supercronic.txt &")))
    if(supercronic_out != 0)stop(safeError("Code was found to be running as docker container, but gave an error when trying to start supercronic. The job must be started manually."))
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
  
  
  #set programs
  library("tools")
  suppressWarnings(library("shiny"))
  plink=paste0(get_conf("programs_path"),"plink")
  
  #set logging level
  verbose <- get_conf("verbose")
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be length 1, not",length(verbose)))
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
  
  #check path
  if(class(path)!="character")stop(paste("path must be character, not",class(path)))
  if(length(path)!=1)stop(paste("path must be length 1, not",length(path)))
  if(!file.exists(path))stop(paste("Did not find file at path:",path))
  
  #Check filename is ok - set to basename(path) if not given
  if(is.null(filename))filename<-basename(path)
  if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
  if(length(filename)!=1)stop(paste("filename must be length 1, not",length(filename)))
  if(length(grep(" ",filename))>0)stop(safeError("Please don't use spaces in filenames"))
  
  #check if this sample should be protected_from_deletion
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be length 1, not",length(protect_from_deletion)))
  
  
  #check the updateProgress object - set to a NULL-returning function if not given.
  if(is.null(updateProgress))updateProgress<-function(detail,value,max){return(NULL)}
  if(class(updateProgress)!="function")stop(paste("updateProgress must be function, not",class(updateProgress)))
  
  
  #check the user-inputted email, set to the default error_report_mail if not given
  if(is.null(email))email<-get_conf("error_report_mail")
  if(class(email)!="character")stop(paste("email must be character, not",class(email)))
  if(length(email)!=1)stop(paste("email must be length 1, not",length(email)))
  if(!get_conf("running_as_docker")){
    if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
    stop(safeError(paste("a real email adress is needed:",email)))
    }
  }
  
  #check if should_be_imputed is ok
  if(class(should_be_imputed)!="logical")stop(paste("should_be_imputed must be logical, not",class(should_be_imputed)))
  if(length(should_be_imputed)!=1)stop(paste("should_be_imputed must be length 1, not",length(should_be_imputed)))
  

  #check if mail address is in positive list for bulk upload (when running as docker it is ok to auto-create this)
  acceptedMails_path<-paste0(get_conf("misc_files_path"),"accepted_emails.txt")
  if(!file.exists(acceptedMails_path)){
    if(get_conf("running_as_docker")){
      default_accepted_emails_all<-c(
      "email   imputeok",
      "any   TRUE"
      )
      f<-file(acceptedMails_path,"w")
      writeLines(default_accepted_emails_all,f)
      close(f)
      if(verbose>0)print(paste0(Sys.time(),": Didn't find an accepted_emails.txt, so wrote a default one, accepting all emails."))
      
    }else{
      stop(safeError("Configuration error: Email accepted-emails list not found"))
    }
  }
  
  #read accepted emails
  acceptedMails<-read.table(acceptedMails_path,stringsAsFactors=F,header=T)
  if(!email%in%acceptedMails[,"email"] & !"any" %in% acceptedMails[,"email"]){ #bulk-upload must adhere to upload criteria
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"not_accepted_email",email,path)
    m<-paste(m,collapse="\t")
    write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
    stop(safeError(paste0("Email ",email," was not in the accepted-emails list, and/or the entry 'any' was not found in the accepted emails list. Your data will not be processed and have already been deleted.")))
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
  if(verbose>0)print(paste0(Sys.time(),": Check for too many ongoing imputations"))
  s<-list.files(get_conf("imputations_path"))
  if(length(grep("^imputation_folder",s)) >= get_conf("max_imputations_in_queue")){
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_many_jobs",email,length(grep("^imputation_folder",s)))
    m<-paste(m,collapse="\t")
    write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
    
    stop(safeError(paste("Too many imputations are already in progress. Cannot start a new one.")))
  }
  
  
  ##checking if this job has not actually been run before
  if(get_conf("block_double_uploads_by_md5sum")){
    if(verbose>0)print(paste0(Sys.time(),": checking if this job has not actually been run before"))
    this_person_md5sum <- md5sum(path)
    all_md5sums_path<-paste0(get_conf("misc_files_path"),"md5sums.txt")
    if(!file.exists(all_md5sums_path)){write("md5sums",file=paste0(get_conf("misc_files_path"),"md5sums.txt"))}
    all_md5sums<-read.table(all_md5sums_path,sep="\t",stringsAsFactors = F)[,1]
    if(this_person_md5sum %in% all_md5sums){
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"md5sum_match",email,this_person_md5sum)
      m<-paste(m,collapse="\t")
      write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
      stop(safeError(paste0("This file was already analyzed by the system. Write an email if you wish to clear this flag (or make a small change in your file so it doesn't have the same md5sum, i.e. ",this_person_md5sum,").")))
    }
    write(this_person_md5sum,file=paste0(get_conf("misc_files_path"),"md5sums.txt"),append=TRUE)			
  }
  
  #Start handling logic. There's many different paths in and out of this interface
  #because it both has to handle various input formats, but also be able to either
  #send the data to the main imputation algorithm, or elsewhere for later
  #manual handling - depending on user choice
  
  #if the 'should be imputed" switch is not on, we just leave them in a folder for 
  #later (manual) inspection and downstream processing. This is the most 'robust'
  #choice in the sense that virtually nothing is done automatically.
  if(!should_be_imputed){
    outfolder <- get_conf("uploads_for_imputemany_path")
    if(!file.exists(outfolder))dir.create(outfolder)
    file.copy(path,paste0(outfolder,upload_time,".zip") )
  }
    
  #if the 'should be imputed" switch is on, a lot more overhead code is required to
  #handle different file-types - because this will be done at (slow) web-speed, hence the progress-tracker
  if(should_be_imputed){
    #unpacking and file-submission logic
    uploads_for_imputemany_path <- get_conf("uploads_for_imputemany_path")
    if(!file.exists(uploads_for_imputemany_path))dir.create(uploads_for_imputemany_path)
    newUnzippedPath <- paste0(uploads_for_imputemany_path,upload_time,"_input.txt")
    gunzipResults<-unzip(path,exdir="~/uploads_for_imputemany/")
    if(length(grep(" ",gunzipResults))>0)stop(safeError("Please don't use spaces in filenames, also not inside zip-file contents."))
    gunzipResults<-grep("_MACOSX",gunzipResults,invert=T,value=T)
    if(length(gunzipResults)==1){ #then its a zip file
      file.rename(gunzipResults, newUnzippedPath)		
      submission_type <- "illumina"
    }else if(length(gunzipResults)==2){
      if(!all(c("ped","map")%in%gsub("^.+\\.","",gunzipResults))){
        stop(safeError("If submitting zip files with two files in them, their endings must be .ped and .map"))
      }else{
        submission_type <- "plink-ped"
      }
    }else if(length(gunzipResults)==3){
      if(!all(c("bed","fam","bim")%in%gsub("^.+\\.","",gunzipResults))){
        stop(safeError("If submitting zip files with their files in them, their endings must be bed and fam and bim"))
      }else{
        submission_type <- "plink-bed"
      }
    }else if(length(gunzipResults)>3){
      stop(safeError("Don't submit zip files with more than two files in them"))
    }else{ #then it's probably not
      #check if it is a gz file
      filetype<-system(paste("file ", path),intern=T)
      if(length(grep("gzip compressed",filetype))==1){
        stop(safeError("Don't submit gz-files. Only uncompressed text or zip-files. If you already know what a gz file is, this should be easy for you. Please format as tab separated text files."))
      }else{
        #otherwise just rename without unzipping - and assume it's the illumina-format
        file.rename(path, newUnzippedPath)		
        submission_type <- "plink-ped"
      }
    }
    path <- newUnzippedPath
    
    #updating progress
    updateProgress(detail = "Checking formatting of input")
    
    
    if(submission_type == "plink-bed"){
      stop(safeError("Not implemented yet, used plink ped-files instead - also, very easy to do, just put in and test a converter")) 
    }

    
    if(submission_type == "plink-ped"){
      pedfile<-gunzipResults[gsub("^.+\\.","",gunzipResults)=="ped"]
      mapfile<-gunzipResults[gsub("^.+\\.","",gunzipResults)=="map"]
      runDir <- get_conf("uploads_for_imputemany_path")
      outfile<-sub("\\.ped$","",basename(pedfile))
      
      
      
      #convert to ped (faster)
      cmd1 <- paste0(plink," --ped ",pedfile," --map ",mapfile," --make-bed --out ",runDir,"/",outfile)
      system(cmd1, ignore.stdout= ignore.stdout, ignore.stderr=ignore.stderr)
      
      bedfile<-paste0(runDir,outfile,".bed")
      famfile<-paste0(runDir,outfile,".fam")
      bimfile<-paste0(runDir,outfile,".bim")
      
      # #check build
      build_check<-check_genome_build(mapfile)
      
      
      #if build check fails, the following 20 lines is a specialized rescue mechanism for Infinium GSA
      #arrays, since that's what we've seen so far. It'll fail for other hg38-formats however and needs
      #to be generalized if this is required.
      if(length(grep("failed built check",build_check))>0){
        #patch to cover hg38 for a set type of files
        if(verbose>0)print(paste0(Sys.time(),": handling liftover from hg38. This is a special liftover that only works on one tested-array type (a bigger, more generalizable one is too computationally expensive"))
        updateProgress(detail = "handling lift over")
        ped_convert_path<-paste0(get_conf("misc_files_path"),"2021-08-31_ped_converter.txt")
        
        #get subset of variants to extract (needs lot of sleeps here otherwise it fails)
        cmd2 <- paste0("cut -f 1 ",ped_convert_path," > ",runDir,outfile,".temp_extract.txt")
        system(cmd2)

        #get original bimfile length for matches
        old_bim_length<-as.numeric(sub(" .+$","",system(paste0("wc -l ",bimfile), intern=T)))
        
        #get extract
        updateProgress(detail = paste0("Doing variant extraction"))
        cmd3 <- paste0(plink," --bed ",bedfile," --fam ",famfile," --bim ",bimfile," --extract ",runDir,"/",outfile,".temp_extract.txt --make-bed --out ",runDir,"/",outfile)
        system(cmd3, ignore.stdout= ignore.stdout, ignore.stderr=ignore.stderr)

        
        #check rsids are the same (just use top-1000 otherwise too slow)
        old_bim<-read.table(bimfile,stringsAsFactors = F,nrows=1000)
        new_bim<-read.table(ped_convert_path,header=T,stringsAsFactors = F,nrows=1000)
        problems<-which(old_bim[,"V2"] != new_bim[,"originalid"])
        if(length(problems)>0)stop
        
        #replace bimfile with the updated and lifted-over version
        cmd4 <- paste0("cut -f 2-7 ",ped_convert_path," | tail -n +2 > ",bimfile)
        system(cmd4)

        #check lengths are the same (fail if not - this is the GSA specific part)
        new_bim_length<-as.numeric(sub(" .+$","",system(paste0("wc -l ",bimfile), intern=T)))
        expect_length<-as.numeric(sub(" .+$","",system(paste0("wc -l ",ped_convert_path), intern=T)))-1
        if(length(new_bim_length)==0 || length(old_bim_length)==0 ||  new_bim_length != expect_length)stop(safeError(paste0("HG38 builds are not fully implemented, only specific arrays are tested. This input had ",old_bim_length," variants before conversion, ",new_bim_length," after, and ",expect_length," were expected, so it's probably not the same array.")))
        
        updateProgress(detail = paste0("Updating bim file - new variant count: ",new_bim_length))
        
        unlink(paste0(runDir,"/",outfile,".temp_extract.txt"))
        unlink(paste0(runDir,"/",outfile,".bed~"))
        unlink(paste0(runDir,"/",outfile,".bim~"))
        unlink(paste0(runDir,"/",outfile,".fam~"))
      }
      
      
      
      
      #get sampleNames
      fam<-read.table(famfile,stringsAsFactors = F)
      sampleNames <- fam[,2]
      
      
      # Create uniqueID for each sample
      existing_uniqueIDs<-list.files(get_conf("data_path"))
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
        if(verbose>2)print(paste0(Sys.time(),": created uniqueID for ",sampleName," which is now called ",uniqueID))
        names(sampleNames)[sampleNames%in%sampleName] <- uniqueID
        
        #check that the ID doesn't already exists (never happened though)
        if(uniqueID%in%existing_uniqueIDs){
          m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
          m<-paste(m,collapse="\t")
          write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
          stop(safeError("Problem with unique ID generation. Please re-load and try again."))
        }
      }
      
      #check and export each sample to the main impute-me queue
      for(uniqueID in names(sampleNames)){
        sampleName <- sampleNames[uniqueID]
        
        #updating progress
        text <- paste0("Sample: ",sampleName," - ",which(names(sampleNames)%in%uniqueID)," of ",length(sampleNames))
        updateProgress(detail = text,max=length(sampleNames))
        
        #create imputation folder and output data folder
        if(verbose>2)print(paste0(Sys.time(),": create imputation folder and output data folder for ",uniqueID," sample ",sampleName))
        if(uniqueID%in%existing_uniqueIDs){
          m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
          m<-paste(m,collapse="\t")
          write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
          stop(safeError("Problem with unique ID generation. Please re-load and try again."))
        }
        
        homeFolder<-paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID,"/")
        dir.create(homeFolder,recursive=T)
        write.table("Job is not ready yet",file=paste0(homeFolder,"job_status.txt"),col.names=F,row.names=F,quote=F)
        fileout_name <- paste0(homeFolder,uniqueID,"_raw_data.txt" )
        
        #Extract this particular individual
        w<-which(names(sampleNames)%in%uniqueID)
        
        cmd2 <- paste0("sed -n '",w," p' ",famfile," > ",runDir,"temp_",uniqueID,"_file.txt")
        system(cmd2)
        
        cmd3 <- paste0(plink," --bed ",bedfile," --fam ",famfile," --bim ",bimfile," --keep ",runDir,"/temp_",uniqueID,"_file.txt --recode --transpose --tab --out ",runDir,"temp_",uniqueID)
        system(cmd3, ignore.stdout= ignore.stdout, ignore.stderr=ignore.stderr)
        
        cmd4 <- paste0("sed 's/ //g' ",runDir,"/temp_",uniqueID,".tped | cut -f 1,4,5 > ",runDir,"/temp_",uniqueID,".temp1.txt")
        system(cmd4)
        
        cmd5 <- paste0("cut -f 2 ",runDir,"/temp_",uniqueID,".tped > ",runDir,"/temp_",uniqueID,".temp2.txt")
        system(cmd5)
        
        cmd6 <- paste0("paste ",runDir,"/temp_",uniqueID,".temp2.txt ",runDir,"/temp_",uniqueID,".temp1.txt  > ",fileout_name)
        system(cmd6)
        
        should_be_imputed<-TRUE
        filename <- sampleName
        imputemany_upload <- TRUE
        save(uniqueID,email,filename,protect_from_deletion,sampleNames,upload_time,should_be_imputed,imputemany_upload,file=paste(homeFolder,"variables.rdata",sep=""))
        
        write.table("Job is ready",file=paste0(homeFolder,"job_status.txt"),col.names=F,row.names=F,quote=F)
        
        unlink(paste0(runDir,"/temp_",uniqueID,".temp1.txt"))
        unlink(paste0(runDir,"/temp_",uniqueID,".temp2.txt"))
        unlink(paste0(runDir,"/temp_",uniqueID,".hh"))
        unlink(paste0(runDir,"/temp_",uniqueID,".log"))
        unlink(paste0(runDir,"/temp_",uniqueID,".map"))
        unlink(paste0(runDir,"/temp_",uniqueID,".tfam"))
        unlink(paste0(runDir,"/temp_",uniqueID,".tped"))
        unlink(paste0(runDir,"/temp_",uniqueID,"_file.txt"))
      }
      
      unlink(paste0(runDir,"/",outfile,".bed"))
      unlink(paste0(runDir,"/",outfile,".bim"))
      unlink(paste0(runDir,"/",outfile,".fam"))
      unlink(paste0(runDir,"/",outfile,".hh"))
      unlink(paste0(runDir,"/",outfile,".log"))
      # unlink(paste0(runDir,"/",outfile,".ped"))
      # unlink(paste0(runDir,"/",outfile,".map"))
    }
    
    
    
    # This is the Illumina (quite a lot of upfront checks required, because it's esssentially
    # just a text file - and needs to be quite sharp on what format it has)
    if(submission_type == "illumina"){
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
      
      
      #check header and max-size
      if(!all(colnames(d)[1:3] == c("RsID","Chr","Position")))stop(safeError("First three column headers must be RsID, Chr, and Position"))
      if(ncol(d)-3 > 100){
        stop(safeError("The file contained more than 100 individuals. This is currently not allowed. It is very easy to lift the ceiling, however, it's just a precaution to not overload servers. Write an email if you need to upload more."))
      }
      
      
      #check sample names
      sampleNames<-colnames(d)[4:ncol(d)]
      if(length(sampleNames)<2)stop(safeError("Have to upload at least data with two samples"))
      
      #removing the empty RsIDs (have happened in several upload types and they are not used in this format anyway)
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
      
      #check pos chr and sort (we want the chromosomes to be in a particular order)
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
      existing_uniqueIDs<-list.files(get_conf("data_path"))
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
        if(verbose>2)print(paste0(Sys.time(),": created uniqueID for ",sampleName," which is now called ",uniqueID))
        names(sampleNames)[sampleNames%in%sampleName] <- uniqueID
        
        #check that the ID doesn't already exists (never happened though)
        if(uniqueID%in%existing_uniqueIDs){
          m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
          m<-paste(m,collapse="\t")
          write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)			
          stop(safeError("Problem with unique ID generation. Please re-load and try again."))
        }
      }
      
      #check and export each sample to the main impute-me queue
      for(uniqueID in names(sampleNames)){
        sampleName <- sampleNames[uniqueID]
        
        #updating progress
        text <- paste0("Sample: ",sampleName," - ",which(names(sampleNames)%in%uniqueID)," of ",length(sampleNames))
        updateProgress(detail = text,max=length(sampleNames))
        
        #create imputation folder and output data folder
        if(verbose>2)print(paste0(Sys.time(),": create imputation folder and output data folder for ",uniqueID," sample ",sampleName))
        if(uniqueID%in%existing_uniqueIDs){
          m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
          m<-paste(m,collapse="\t")
          write(m,file=paste0(get_conf("submission_logs_path"),"submission_log.txt"),append=TRUE)
          stop(safeError("Problem with unique ID generation. Please re-load and try again."))
        }
        
        homeFolder<-paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID,"/")
        dir.create(homeFolder,recursive=T)
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
    }
    
    
    #prepare write-out to bulk-impute register file 
    imputemany_registry_path <- paste0(get_conf("misc_files_path"),"imputemany_registry.txt")
    if(!file.exists(imputemany_registry_path)){
      f<-file(imputemany_registry_path,"w")
      writeLines(paste(c("upload_time","has_been_sent","error_sent","length","email","uniqueIDs"),collapse="\t"),f)
      close(f)
    }
    #headers are: upload time, has-been-sent,length,  error-sent, email, uniqueIDs
    registry_entry <-paste(c(upload_time,FALSE, FALSE, length(sampleNames),email, paste(names(sampleNames),collapse=",")),collapse="\t")
    write(registry_entry,file=imputemany_registry_path,append=TRUE)			
    
    
  }
  
  
  #if possible, admin-mail a notification that an imputemany upload has happened
  #doesn't matter too much if an admin-email is actually sent or not, it'll run either way
  if(get_conf("from_email_address") != "" & get_conf("from_email_password") != "" & get_conf("error_report_mail")!= ""){
    message<-paste0("<html><body>A data set with name ",upload_time," was uploaded to the server by ",email," (imputation was set to ",should_be_imputed,")</body></html>")
    suppressWarnings(library("gmailr",warn.conflicts = FALSE))
    gm_auth_configure( path =paste0(get_conf("misc_files_path"),"mailchecker.json"))
    gm_auth(email=get_conf("from_email_address"),cache=paste0(get_conf("misc_files_path"),"mail_secret"))
    prepared_email <- try(gm_mime() %>%
                            gm_to(get_conf("error_report_mail")) %>%
                            gm_from(get_conf("from_email_address")) %>%
                            gm_subject("Impute-many data set uploaded") %>%
                            gm_html_body(message))
    mailingResult<-try(gm_send_message(prepared_email))
  }
  
  
  
  #if running as docker, then kill previous supercronic jobs and restart
  if(get_conf("autostart_supercronic")){
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
    if(!file.exists(paste0(get_conf("code_path"),"supercronic.txt")))stop(safeError(paste0("Code was found to be running as docker container, but with no ",get_conf("code_path"),"/supercronic.txt file ready for launch. The job will likely never execute.")))
    supercronic_out<-try(system(paste0("supercronic ",get_conf("code_path"),"supercronic.txt &")))
    if(supercronic_out != 0)stop(safeError("Code was found to be running as docker container, but gave an error when trying to start supercronic. The job will likely not start"))
  }
  
  
  
  
  #then post in web-interface 
  return(paste("<b>Genome files succesfully submitted</b>. The processing of your genome will take some time to run. We will email you at",email,"with download-instructions during the next days."))
  
  
}







check_for_cron_ready_jobs<-function(
  job_type
){
  #' check for cron ready jobs
  #'
  #' Checks what jobs to run. In the simplest form, this function just returns 
  #' the uniqueID of the next job in line, sorted from all in queue, by submission-time 
  #' and mark it as 'Job is running' in the job_status.txt. However, with further 
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
  server_role <- get_conf("server_role")
  hub_address <- get_conf("hub_address")
  verbose <- get_conf("verbose")
  seconds_wait_before_start<-get_conf("seconds_wait_before_start")
  
  
  
  #This block serves to space the runs some time apart. It is mostly relevant on e.g. t2.medium AWS instances
  #that can build up computing power. But it may also be useful in debugging situations.
  if(seconds_wait_before_start>0){
    #First checking if node is already at max load (max_imputations_per_node, set in configuration.R)
    foldersToCheck<-grep("^imputation_folder",list.files(get_conf("imputations_path")),value=T)
    runningJobCount<-0
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste(get_conf("imputations_path"),folderToCheck,"/job_status.txt",sep="")
      if(file.exists(job_status_file)){
        job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
        if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
      }
    }
    #then stop job if runs are already running
    if(runningJobCount>(get_conf("max_imputations_per_node")-1)){
      stop(paste("Found",runningJobCount,"running jobs, which is more than max_imputations_per_node so doing nothing"))
    }else{
      if(verbose>0)print(paste0(Sys.time(),": Waiting ",seconds_wait_before_start/(60)," minutes before re-checking. Set seconds_wait_before_start variable in ~/configuration/configuration.R if this is not ok."))
      Sys.sleep(seconds_wait_before_start)
    }
    #After sleeping period, wake up and re-check (equal to the seconds_wait_before_start=0 setting)
  }
  
  
  
  
  
  
  
  #This block checks if enough stuff is already running, and abort if so
  foldersToCheck<-grep("^imputation_folder",list.files(get_conf("imputations_path")),value=T)
  runningImputationCount<-0
  for(folderToCheck in foldersToCheck){
    job_status_file<-paste(get_conf("imputations_path"),folderToCheck,"/job_status.txt",sep="")
    if(file.exists(job_status_file)){
      job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
      if(job_status=="Job is running"){runningImputationCount<-runningImputationCount+1}
    }
  }
  foldersToCheck<-grep("^vcf_folder",list.files(get_conf("vcfs_path")),value=T)
  runningVcfCount<-0
  for(folderToCheck in foldersToCheck){
    job_status_file<-paste(get_conf("vcfs_path"),folderToCheck,"/job_status.txt",sep="")
    if(file.exists(job_status_file)){
      job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
      if(job_status=="Job is running"){runningVcfCount<-runningVcfCount+1}
    }
  }
  if(runningVcfCount + runningImputationCount >(get_conf("max_imputations_per_node")-1)){
    stop(paste0("Found ",runningVcfCount + runningImputationCount," running jobs (",runningImputationCount," imputations, ",runningVcfCount," vcfs), which is more than the max_imputations_per_node setting. Aborting and doing nothing"))
  }
  
  
  
  
  if(job_type=="single"){
    
    #If the computer is not too busy and the server_role is node - we fetch ONE job (if it is hub, the jobs are already there)
    if(server_role== "Node"){
      #sort checking order by time entered
      cmd1 <- paste0("ssh ubuntu@",hub_address," ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' ",get_conf("imputations_path")," | tail -n +2")
      remotedata<-system(cmd1,intern=T)
      Sys.sleep(0.2)
      remotedata_df<-as.data.frame(do.call(rbind,strsplit(remotedata,"\\s+")),stringsAsFactors=F)
      if(ncol(remotedata_df)==0)stop("Nothing found at hub server. Exit, stop and wait.")
      remotedata_df<-remotedata_df[order(remotedata_df[,6]),]
      remoteFoldersToCheck<-remotedata_df[,7]
      
      
      #check if there's any fast-queue jobs to put up-front. The fast-queue jobs is just a file with uniqueID
      #and then TRUE or FALSE. The TRUE or FALSE means if a bulk impute is allowed to take it or not
      #which is not relevant here in single-running.
      cmd0 <- paste0("ssh ubuntu@",hub_address," cat ",get_conf("misc_files_path"),"fast_queue_emails.txt
                ")
      f1<-system(cmd0,intern=T)
      Sys.sleep(0.2)
      if(length(f1)>0){ #if there is a fast-queue file, we handle it
        f2<-do.call(rbind,strsplit(f1,"\t"))
        f3<-f2[,1]
        remoteFoldersToCheck<-c(remoteFoldersToCheck[remoteFoldersToCheck%in%f3],remoteFoldersToCheck[!remoteFoldersToCheck%in%f3])
      }  
      
      #then loop over all remote folders
      for(remoteFolderToCheck in remoteFoldersToCheck){
        cmd2 <- paste0("ssh ubuntu@",hub_address," cat ",get_conf("imputations_path"),remoteFolderToCheck,"/job_status.txt")
        job_status<-system(cmd2,intern=T)
        #Check if the job is ready
        if(job_status=="Job is ready"){
          if(verbose>0)print(paste0(Sys.time(),": Found remote job-status file and job is ready ",remoteFolderToCheck," - will copy to local Node"))
          
          #First write to job-status that now the job is off to a remote server
          cmd3 <- paste("ssh ubuntu@",hub_address," 'echo Job is remote-running > ",get_conf("imputations_path"),remoteFolderToCheck,"/job_status.txt'",sep="")
          system(cmd3)
          
          #then copy all the files to here
          cmd4 <- paste("scp -r ubuntu@",hub_address,":",get_conf("imputations_path"),remoteFolderToCheck," ",get_conf("imputations_path"),remoteFolderToCheck,sep="")
          system(cmd4)
          
          #Then write locally that job is ready (and break, because we only need one)
          job_status_file<-paste(get_conf("imputations_path"),remoteFolderToCheck,"/job_status.txt",sep="")
          unlink(job_status_file)
          write.table("Job is ready",file=job_status_file,col.names=F,row.names=F,quote=F)
          break
        }
      }
    }
    
    
    
    #Then - no matter the role - we check locally which, if any, folders are ready to run
    imputeThisFolder<-NA
    foldersToCheck<-grep("^imputation_folder",list.files(get_conf("imputations_path")),value=T)
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste(get_conf("imputations_path"),folderToCheck,"/job_status.txt",sep="")
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
    
    #If the computer is not too busy (per above checks) and the server_role is node - we fetch 10 jobs, 
    #copying them to local and marking them as running on hub.
    if(server_role== "Node"){
      #sort checking order by time entered
      cmd1 <- paste("ssh ubuntu@",hub_address," ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' ",get_conf("imputations_path"),"  | tail -n +2",sep="")
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
      cmd0 <- paste0("ssh ubuntu@",hub_address," cat ",get_conf("misc_files_path"),"fast_queue_emails.txt
                ")
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
        cmd2 <- paste("ssh ubuntu@",hub_address," cat ",get_conf("imputations_path"),remoteFolderToCheck,"/job_status.txt",sep="")
        job_status<-system(cmd2,intern=T)
        
        #Check if the job is ready
        if(job_status=="Job is ready"){
          if(verbose > 1)print(paste0(Sys.time(),": Found remote job-status file and job is ready ",remoteFolderToCheck," - will copy to local"))
          remoteFoldersToRun <- c(remoteFoldersToRun,remoteFolderToCheck)
        }
      }
      
      #if exactly 'length_requested' - then we copy the files from hub to node 
      #(because if not we don't want any hub-side modifications)
      if(length(remoteFoldersToRun)==length_requested){
        #Then write to job-status that now the job is off to a remote server
        for(remoteFolderToRun in remoteFoldersToRun){
          cmd3 <- paste("ssh ubuntu@",hub_address," 'echo Job is remote-running > ",get_conf("imputations_path"),remoteFolderToRun,"/job_status.txt'",sep="")
          system(cmd3)
        }
        
        #then copy all the files to here
        for(remoteFolderToRun in remoteFoldersToRun){
          cmd4 <- paste("scp -r ubuntu@",hub_address,":",get_conf("imputations_path"),remoteFolderToRun," ",get_conf("imputations_path"),remoteFolderToRun,sep="")
          system(cmd4)
          
          #And write locally that job is ready
          job_status_file<-paste(get_conf("imputations_path"),remoteFolderToRun,"/job_status.txt",sep="")
          unlink(job_status_file)
          write.table("Job is ready",file=job_status_file,col.names=F,row.names=F,quote=F)
        }
        
        #Update the local folder file, but keeping the original order to reflect priority runs
        foldersToCheck<-remoteFoldersToRun
      }
    }else{
      
      #on hub-running we just grep the folders and take 10
      #checking priority queue is not implemented yet for this run type, too rare
      foldersToCheck<-grep("^imputation_folder",list.files(get_conf("imputations_path")),value=T)
      if(length(foldersToCheck)>10){
        foldersToCheck <- foldersToCheck[1:10]
      }
    }
    
    
    
    #Then - no matter the role - we check locally which, if any, 
    #folders are ready to run. Should be 10 copied from hub, if running as node, 
    #or else just check what is present locally (since it's hub-running)
    imputeThisFolder<-vector()
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste(get_conf("imputations_path"),folderToCheck,"/job_status.txt",sep="")
      
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
      stop(paste0("The impute-queue folders did not contain enough jobs ready for imputation. The requested number is ",length_requested,". The found number is: ",length(imputeThisFolder))) #here we could easily have a mechanism for proceeding if the found number is <10, but for now we just fail so we can sort it out later. It's not applicable in node-running-setups anyway, because they always take 10.
    }else{
      print(paste0(Sys.time(),": Found ",length_requested," ready sets to impute"))
    }
    
    
    uniqueIDs<-sub("imputation_folder_","",imputeThisFolder)
    
    
  }else if(job_type =="vcf"){
    
    #First handle the Node running code - which will later merge into local/hub running after a scp step
    if(get_conf("server_role")== "Node"){
      
      #sort checking order by time entered
      cmd1 <- paste0("ssh ubuntu@",hub_address," ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' ",get_conf("vcfs_path")," | tail -n +2")
      remotedata<-system(cmd1,intern=T)
      Sys.sleep(0.2)
      remotedata_df<-as.data.frame(do.call(rbind,strsplit(remotedata,"\\s+")),stringsAsFactors=F)
      if(ncol(remotedata_df)==0)stop("Nothing found at hub server. Exit, stop and wait.")
      remotedata_df<-remotedata_df[order(remotedata_df[,6]),]
      remoteFoldersToCheck<-remotedata_df[,7]
      
      
      #check if there's any fast-queue jobs to put up-front. The fast-queue jobs is just a file with uniqueID
      #and then TRUE or FALSE. The TRUE or FALSE means if a bulk impute is allowed to take it or not
      #which is not relevant here in single-running.
      cmd0 <- paste0("ssh ubuntu@",hub_address," cat ",get_conf("misc_files_path"),"fast_queue_emails.txt
                ")
      f1<-system(cmd0,intern=T)
      Sys.sleep(0.2)
      if(length(f1)>0){ #if there is a fast-queue file, we handle it
        f2<-do.call(rbind,strsplit(f1,"\t"))
        f3<-f2[,1]
        remoteFoldersToCheck<-c(remoteFoldersToCheck[remoteFoldersToCheck%in%f3],remoteFoldersToCheck[!remoteFoldersToCheck%in%f3])
      }  
      
      #then loop over all remote folders
      for(remoteFolderToCheck in remoteFoldersToCheck){
        cmd2 <- paste0("ssh ubuntu@",hub_address," cat ",get_conf("vcfs_path"),remoteFolderToCheck,"/job_status.txt")
        job_status<-system(cmd2,intern=T)
        #Check if the job is ready
        if(job_status=="Job is ready"){
          if(verbose>0)print(paste0(Sys.time(),": Found remote job-status file and job is ready ",remoteFolderToCheck," - will copy to local Node"))
          
          #First write to job-status that now the job is off to a remote server
          cmd3 <- paste0("ssh ubuntu@",hub_address," 'echo Job is remote-running > ",get_conf("vcfs_path"),remoteFolderToCheck,"/job_status.txt'")
          system(cmd3)
          
          #then copy all the files to here
          cmd4 <- paste0("scp -r ubuntu@",hub_address,":",get_conf("vcfs_path"),remoteFolderToCheck," ",get_conf("vcfs_path"),remoteFolderToCheck)
          system(cmd4)
          
          #Then write locally that job is ready
          job_status_file<-paste(get_conf("vcfs_path"),remoteFolderToCheck,"/job_status.txt",sep="")
          unlink(job_status_file)
          write.table("Job is ready",file=job_status_file,col.names=F,row.names=F,quote=F)
          break
        }
      }
      #Update the local foldersToCheck to reflect new arrivals
      foldersToCheck<-grep("^vcf_folder",list.files(get_conf("vcfs_path")),value=T)
    }
    
    
    #From here on local processing is assumed, in the sense that regardless of the
    #function being executed on a node or hub it will be in the same state.
    cmd1 <- paste0("ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' ",get_conf("vcfs_path")," | tail -n +2")
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
      job_status_file<-paste(get_conf("vcfs_path"),folderToCheck,"/job_status.txt",sep="")
      if(file.exists(job_status_file)){
        job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
        if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
      }
    }
    if(runningJobCount>(get_conf("max_imputations_per_node")-1)){
      stop(paste("Found",runningJobCount,"running jobs, which is more than max_imputations_per_node so doing nothing"))
    }
    
    
    
    
    #Then - no matter the role - we check locally which, if any, folders are ready to run
    imputeThisFolder<-NA
    for(folderToCheck in foldersToCheck){
      job_status_file<-paste(get_conf("vcfs_path"),folderToCheck,"/job_status.txt",sep="")
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
  
  if(server_role=="Hub"){
    if(verbose>0)print(paste0(Sys.time(),": Found ",length(uniqueIDs)," jobs of type ",job_type," and marked them as job_running."))
  }else if(server_role=="Node"){
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
  shapeit=paste0(get_conf("programs_path"),"shapeit.v2.904.3.10.0-693.11.6.el7.x86_64/bin/shapeit")
  plink=paste0(get_conf("programs_path"),"plink")
  impute2=paste0(get_conf("programs_path"),"impute_v2.3.2_x86_64_static/impute2")
  sample_ref=paste0(get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample")
  
  
  #check uniqueID is ok
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  
  
  #set runDir and check that it exists
  start_wd<-getwd()
  runDir <- paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID)
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be length 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)runDir <- sub("/$","",runDir) #remove trailing slash
  # setwd(runDir)
  load(paste(runDir,"/variables.rdata",sep=""))
  rawdata<-paste(runDir,"/",uniqueID,"_raw_data.txt",sep="")
  
  #test that impute2 static can run (or switch to dynamic)
  impute2_blank_run_out <- suppressWarnings(system(impute2,intern=T,ignore.stderr = T) )
  if(attr(impute2_blank_run_out,"status")==139){
    impute2 <- paste0(get_conf("programs_path"),"impute_v2.3.2_x86_64_dynamic/impute2")
    if(verbose>2)print(paste0(Sys.time(),": Problem with impute_v2.3.2_x86_64_static detected. Switching to impute_v2.3.2_x86_64_dynamic."))
  }
  
  #check other arguments 
  if(class(rawdata)!="character")stop(paste("rawdata must be character, not",class(rawdata)))
  if(length(rawdata)!=1)stop(paste("rawdata must be length 1, not",length(rawdata)))
  if(!file.exists(rawdata))stop(paste("Did not find rawdata at path:",rawdata))
  
  if(class(shapeit)!="character")stop(paste("shapeit must be character, not",class(shapeit)))
  if(length(shapeit)!=1)stop(paste("shapeit must be length 1, not",length(shapeit)))
  if(!file.exists(shapeit))stop(paste("Did not find shapeit at path:",shapeit))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be length 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(class(impute2)!="character")stop(paste("impute2 must be character, not",class(impute2)))
  if(length(impute2)!=1)stop(paste("impute2 must be length 1, not",length(impute2)))
  if(!file.exists(impute2))stop(paste("Did not find impute2 at path:",impute2))
  
  if(class(sample_ref)!="character")stop(paste("sample_ref must be character, not",class(sample_ref)))
  if(length(sample_ref)!=1)stop(paste("sample_ref must be length 1, not",length(sample_ref)))
  if(!file.exists(sample_ref))stop(paste("Did not find sample_ref at path:",sample_ref))
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be length 1, not",length(verbose)))
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
  cmd1<-paste0(plink," --23file ",rawdata," ",uniqueID," ",uniqueID," --recode --out ",runDir,"/step_1")
  out1<-system(cmd1,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
  
  
  #If the standard command fails, we run an extensive error rescue. Hopefully shouldn't be used too often, but is nice for when people submit weird custom-setup data
  if(out1 == 3){
    special_error_check(uniqueID,runDir)
  }  
  
  #Rscript to omit duplicates
  map<-read.table(paste0(runDir,'/step_1.map'),sep='\t',stringsAsFactors=F,comment.char="")
  exclude<-map[duplicated(map[,4]),2]
  if(verbose>1)print(paste0(Sys.time(),': Removed ',length(exclude),' SNPs that were duplicated'))
  write.table(exclude,file=paste0(runDir,'/step_2_exclusions'),sep='\t',row.names=FALSE,col.names=F,quote=F)
  
  
  #loop over chromosomes
  for(chr in c("X",as.character(1:22))){
    if(verbose>0)print(paste0(Sys.time(),": Starting run on chromosome ",chr))  
    
    
    #First in loop - extract only one specific chromosome
    cmd2<-paste(plink," --file ",runDir,"/step_1 --chr ",chr," --recode --out ",runDir,"/step_2_chr",chr," --exclude ",runDir,"/step_2_exclusions",sep="")
    out2<-system(cmd2,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    #if X chromosome is missing it is allowed to skip forward
    if(out2 %in% c(12,13) & chr == "X"){
      if(verbose>0)print(paste0(Sys.time(),": Didn't find X-chr data, so skipping that"))
      next
    }
    
    #Then check for strand flips etc. 
    cmd3<-paste0(shapeit," -check --input-ped ",runDir,"/step_2_chr",chr,".ped ",runDir,"/step_2_chr",chr,".map -M ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log ",runDir,"/step_2_chr",chr,"_shapeit_log")
    system(cmd3,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    
    
    #Many homozygote SNPs will fail the check, because, well - of course, they don't have the ref-allele. So we make more detailed R script for sorting them
    logFile<-read.table(paste(runDir,"/step_2_chr",chr,"_shapeit_log.snp.strand",sep=""),sep='\t',stringsAsFactors=FALSE,header=F,skip=1)
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
    map<-read.table(paste(runDir,"/step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F,comment.char = "")
    
    #This loads the ped file, and doubles it
    ped2<-ped1<-strsplit(readLines(paste(runDir,"/step_2_chr",chr,".ped",sep=""))," ")[[1]]
    
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
    write.table(ped,paste(runDir,"/step_3_chr",chr,".ped",sep=""),sep=" ",col.names=F,row.names=F,quote=F)
    omitRemaining<-logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
    if(verbose>1)print(paste0(Sys.time(),': Strand-flip handling. Omitting ',length(omitMissing),' because of missing, ',length(omitBlank),' because they are blank, and ',length(omitNonIdentical),' true strand flips'))
    write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste(runDir,"/step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
    
    
    #running the shapeit command (with two people, the right one and a placeholder heterozygote
    cmd4<-paste0(shapeit," --input-ped ",runDir,"/step_3_chr",chr,".ped ",runDir,"/step_2_chr",chr,".map -M ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log ",runDir,"/step_4_chr",chr,"_shapeit_log --exclude-snp ",runDir,"/step_3_chr",chr,"_exclusions -O ",runDir,"/step_4_chr",chr)
    system(cmd4,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
    
    
    #checking for errors and stopping if there are any. 
    #No point to continue otherwise
    #There's a few specialized bug-hunter scripts that may be activated at this point
    log<-readLines(paste(runDir,"/step_4_chr",chr,"_shapeit_log.log",sep=""))
    if(substr(log[length(log)],1,5)=="ERROR"){
      if(length(grep("Non biallelic site",log[length(log)]))>0){
        check_for_rare_nonbiallic_snps(uniqueID)
      }
      stop(paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep=""))
    }
    
    #removing the placeholder person again
    cmd5_1<-paste("cut --delimiter=' ' -f 1-7 ",runDir,"/step_4_chr",chr,".haps > ",runDir,"/step_5_chr",chr,".haps",sep="")
    system(cmd5_1)
    cmd5_2<-paste("head -n 3 ",runDir,"/step_4_chr",chr,".sample > ",runDir,"/step_5_chr",chr,".sample",sep="")
    system(cmd5_2)
    
    
    
    #detect max length of each chromosome
    cmd6<-paste0("zcat ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2")
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
      cmd_special_1 <- paste0("awk '$3>",start," && $3<",end,"' ",runDir,"/step_5_chr",chr,".haps")
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
        cmd7<-paste0(impute2," -m ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g ",runDir,"/step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o ",runDir,"/step_7_chr",chr,"_",i)
        step_7_log<-system(cmd7,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
        
        #test for memory-lack bug (step_7_log will be 137 if killed, otherwise 0)
        if(step_7_log == 137){
          re_run_chunk <- TRUE
          divisions<-3
          print(paste0(Sys.time(),": restart impute2 run at ",i," with new subset to avoid memory-lack bug. This was done because of impute2-error-137. Chunk_lines_length was ",chunk_lines_length, ". If this error is observed often or around the time of known pipeline failures, it may be smart to reduce the max_imputation_chunk_size in the ~/configuration/configuration.R to a lower number." ))
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
          
          cmd7<-paste0(impute2," -m ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g ",runDir,"/step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o ",runDir,"/step_7_chr",chr,"_",i,"-",j)
          step_7_log_2<-system(cmd7,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          if(step_7_log_2 == 137)stop("the memory problem was still active after second round. It may be smart to reduce the max_imputation_chunk_size in the ~/configuration/configuration.R to a lower number.")
          
        }
      }
    }
  }
  # setwd(start_wd)
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
  shapeit=paste0(get_conf("programs_path"),"shapeit.v2.904.3.10.0-693.11.6.el7.x86_64/bin/shapeit")
  plink=paste0(get_conf("programs_path"),"plink")
  impute2=paste0(get_conf("programs_path"),"impute_v2.3.2_x86_64_static/impute2")
  sample_ref=paste0(get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample")
  
  
  
  #check uniqueID
  if(class(uniqueIDs)!="character")stop(paste("uniqueIDs must be character, not",class(uniqueIDs)))
  if(length(uniqueIDs)!=10)stop(paste("rawdata must be length 10, not",length(uniqueIDs)))
  
  #Check and set runDir
  start_wd <- getwd()
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be length 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  setwd(runDir)
  
  
  #test that impute2 static can run (or switch to dynamic)
  impute2_blank_run_out <- suppressWarnings(system(impute2,intern=T,ignore.stderr = T) )
  if(attr(impute2_blank_run_out,"status")==139){
    impute2 <- paste0(get_conf("programs_path"),"impute_v2.3.2_x86_64_dynamic/impute2")
    if(verbose>2)print(paste0(Sys.time(),": Problem with impute_v2.3.2_x86_64_static detected. Switching to impute_v2.3.2_x86_64_dynamic."))
  }
  
  #check other arguments
  if(class(shapeit)!="character")stop(paste("shapeit must be character, not",class(shapeit)))
  if(length(shapeit)!=1)stop(paste("shapeit must be length 1, not",length(shapeit)))
  if(!file.exists(shapeit))stop(paste("Did not find shapeit at path:",shapeit))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be length 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(class(impute2)!="character")stop(paste("impute2 must be character, not",class(impute2)))
  if(length(impute2)!=1)stop(paste("impute2 must be length 1, not",length(impute2)))
  if(!file.exists(impute2))stop(paste("Did not find impute2 at path:",impute2))
  
  if(class(sample_ref)!="character")stop(paste("sample_ref must be character, not",class(sample_ref)))
  if(length(sample_ref)!=1)stop(paste("sample_ref must be length 1, not",length(sample_ref)))
  if(!file.exists(sample_ref))stop(paste("Did not find sample_ref at path:",sample_ref))
  
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be length 1, not",length(verbose)))
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
  
  
  
  rawdata_files<-paste(get_conf("imputations_path"),"imputation_folder_",uniqueIDs,"/",uniqueIDs,"_raw_data.txt",sep="")
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
          messages<-paste0(Sys.time(),": ERROR The missnp>500 error was triggered. Outputting debug info. To fix this error it is usually sufficient to find the sample with odd-out set of alleles and queue that for single-running.")
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
            
            
            
            message<-paste0("<html><body>",paste(messages,collapse="<br>"),"</body></html>")
            suppressWarnings(library("gmailr",warn.conflicts = FALSE))
            gm_auth_configure( path =paste0(get_conf("misc_files_path"),"mailchecker.json"))
            gm_auth(email=get_conf("from_email_address"),cache=paste0(get_conf("misc_files_path"),"mail_secret"))
            prepared_email <- try(gm_mime() %>%
                                    gm_to(get_conf("error_report_mail")) %>%
                                    gm_from(get_conf("from_email_address")) %>%
                                    gm_subject("An impute-me run has problem") %>%
                                    gm_html_body(message))
            mailingResult<-try(gm_send_message(prepared_email))
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
        stop(paste0("Found ",duplicates_found," duplicate positions at chr",chr,", and the current threshold is 10000. Suggested fix is to open ",runDir,"/step_2m_chr",chr,".map and look for odd variant-IDs found at similar positions as rs-IDs. Then search for these variant names and run the containing sample outside of bulk-run (e.g. grep <odd-variant-id> imputations/imputation_folder_id_*/id*)"))
      }
      exclude<-unique(map[duplicated(map[,4]),2])
      write.table(exclude,file=paste0('step_2_overall_exclusions_chr',chr),sep='\t',row.names=FALSE,col.names=F,quote=F)
      
      
      cmd8<-paste(plink," --file step_2m_chr",chr," --exclude step_2_overall_exclusions_chr",chr," --recode --out step_2_chr",chr,sep="")
      system(cmd8,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
    }    
    
    #Then check for strand flips etc. 
    cmd9<-paste0(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log")
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
    cmd10<-paste0(shapeit," --force --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr)
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
    cmd13<-paste0("zcat ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2")
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
        cmd14<-paste0(impute2," -m ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i)
        step_7_log<-system(cmd14,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
        
        
        #test for memory-lack bug (step_7_log will be 137 if killed, otherwise 0)
        if(step_7_log == 137){
          re_run_chunk <- TRUE
          divisions<-3
          if(verbose>=0)print(paste0(Sys.time(),": restart impute2 run at ",i," with new subset to avoid memory-lack bug. This was done because of impute2-error-137. Chunk_lines_length was ",chunk_lines_length, ". If this error is observed often or around the time of known pipeline failures, it may be smart to reduce the max_imputation_chunk_size in the ~/configuration/configuration.R to a lower number." ))
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
          
          cmd15<-paste0(impute2," -m ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start_2," ",end_2," -Ne 20000 -o step_7_chr",chr,"_",i,"-",j)
          step_7_log_2<-system(cmd15,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          if(step_7_log_2 == 137)stop("the memory problem was still active after second round. It may be smart to reduce the max_imputation_chunk_size in the ~/configuration/configuration.R to a lower number.")
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
      outfolder <- paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID,"/")
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
    
    #clean up files afterwards (or else we break the 17G  limit)
    unlink(step7Files) 
  }
  setwd(start_wd)
}





convert_vcfs_to_simple_format<-function(
  uniqueID,
  protect_from_deletion=FALSE
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
  #' @param protect_from_deletion A switch carried down the analysis, that can be set as TRUE for debugging purposes
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
  vcf_folder <- paste0(get_conf("vcfs_path"),"vcf_folder_",uniqueID,"/")
  bed_path<-paste0(get_conf("code_path"),"imputeme/2021-08-13_common_snps.txt.gz")
  applied_bed_path <- paste0(vcf_folder,"temporary_bed.txt")
  vcf_path<-paste0(vcf_folder,uniqueID,"_raw_data.vcf.gz")
  variables_path <- paste0(vcf_folder,"variables.rdata")
  out_folder<-paste0(get_conf("data_path"),uniqueID)
  out_pdata_path<-paste0(out_folder,"/pData.txt")
  out_temp_path<-paste0(out_folder,"/temp")
  out_input_path<-paste0(out_folder,"/",uniqueID,".input_data.zip") #Odd naming, "out_input_path", I know, but it's because it should be a copy of the input-file saved in the ~/data folder. For VCFs however, it's the post-subsetting input file. They are too big otherwise.
  out_input_temp_path<-paste0(out_temp_path,"/",uniqueID,"_raw_data.txt")
  out_temp_gen_per_chr_path<-paste0(out_temp_path,"/",uniqueID,"_chr__CHR__.gen")
  out_gen_path<-paste0(out_folder,"/",uniqueID,".gen.zip")
  out_temp_simple_per_chr_path<-paste0(out_temp_path,"/",uniqueID,"_chr__CHR__.simple_format.txt")
  out_simple_path<-paste0(out_folder,"/",uniqueID,".simple_format.zip")
  minimum_required_variant_in_vcf_count <- get_conf("minimum_required_variant_in_vcf_count")
  start_wd<-getwd()
  
  #checks of input
  if(!file.exists(vcf_folder))stop(paste0("For ",uniqueID," did not find a vcf_folder at: ",vcf_folder))
  if(class(protect_from_deletion)!="logical")stop(paste("protect_from_deletion must be logical, not",class(protect_from_deletion)))
  if(length(protect_from_deletion)!=1)stop(paste("protect_from_deletion must be length 1, not",length(protect_from_deletion)))
  
  
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
  if(!file.exists(paste0(vcf_folder,"extracted.tped"))){
    if(verbose>0)print(paste0(Sys.time()," Merging input VCF with requested set of locations produced no file ('extracted.tped'). This means that not a single input position was among the common variants needed to produce polygenic risk scores."))
    tped <- data.frame()
  }else{
    tped<-read.table(paste0(vcf_folder,"extracted.tped"),stringsAsFactors=F,sep="\t")  
  }
  
  if(nrow(tped) < minimum_required_variant_in_vcf_count){
    if(verbose>0)print(paste0(Sys.time(),": Observed less than minimum_required_variant_in_vcf_count (",nrow(tped),"). Will try to re-run extraction step assuming GRCh38."))
    
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
    if(!file.exists(paste0(vcf_folder,"extracted.tped"))){
      tped<-read.table(paste0(vcf_folder,"extracted.tped"),stringsAsFactors=F,sep="\t")
    }else{
      tped <- data.frame()
    }
    if(nrow(tped) < minimum_required_variant_in_vcf_count){
      stop(paste("This VCF-file only had",nrow(tped),"recognized SNPs from the >1M requested common-SNP set. This must be at least ",minimum_required_variant_in_vcf_count," or else we suspect a problem with the DNA sequencing"))
    }
    
    #But in the (best) success case, we continue
  }else{
    if(verbose>0)print(paste0(Sys.time(),": ",uniqueID," had ",nrow(tped)," variants matched with the request-bed file. The minimum required amount was ",minimum_required_variant_in_vcf_count,"."))
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
  if(length(grep("^rs",tped[,"V2"])) > minimum_required_variant_in_vcf_count){
    rownames(tped) <- tped[,"V2"]
    rownames(bed) <- bed[,"rsid"]
    
    #Or else revert to positional matching (be careful here - as explained above!)
  }else if(length(grep("[0-9]+:[0-9]+",tped[,"V2"])) > minimum_required_variant_in_vcf_count){
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
  
  #double check that a reasonable number of variants are matched (we still use the minimum_required_variant_in_vcf_count variable, although
  #it now lost meaning a little because it's a lot of different counts. Fact is
  #that these values are still up for tuning, but a fairly wide canyon - all the
  #way down to exon seq is accepted)
  if(length(intersect(rownames(tped), rownames(bed))) < minimum_required_variant_in_vcf_count){
    stop("Too few vcf-name fields could be matched in the reference bed file")
  }
  
  
  #if the build is hg38 we to flip a few of the ref/alts as indicated in the common-snps bed file (these are the
  #variants where ref and alt flips on liftover.)
  if(build_guess == "grch38"){
    bed[,"temp_ref"]<-bed[,"ref"]
    bed[,"temp_alt"]<-bed[,"alt"]
    w0<-which(bed[,"ref_switched_in_hg38"])
    bed[w0,"temp_ref"]<-bed[w0,"alt"]
    bed[w0,"temp_alt"]<-bed[w0,"ref"]
    bed[,"ref"]<-bed[,"temp_ref"]
    bed[,"alt"]<-bed[,"temp_alt"]
    bed[,"temp_alt"]<-bed[,"temp_ref"]<-NULL
    if(verbose>1)print(paste0(Sys.time(),": Flipped ",length(w0)," ref and alt alleles known to differ between hg19 and hg38"))
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
    ref=bed[w1,"ref"],
    alt=bed[w1,"alt"],
    stringsAsFactors=F)
  
  
  #create data.frame with vcf-extracted data (i.e. those that are not homozygote ref)
  w2 <- which(rownames(bed) %in% rownames(tped))
  non_homozygote_refs<-data.frame(
    rsid=bed[w2,"rsid"],
    chr=bed[w2,"chr"],
    pos=bed[w2,"grch37_start"],
    genotype=apply(tped[rownames(bed)[w2],c(5,6)],1,paste,collapse=""),
    ref=bed[w2,"ref"],
    alt=bed[w2,"alt"],
    stringsAsFactors=F)
  
  
  #clean up for memory reasons (this is peak)
  rm("tped","bed","w1","w2")
  gc(verbose=verbose>4)
  
  
  #switch allele order (alphabetical, we don't know the phase anyway)
  switch_alleles <- function(x){
    x<-sub("/","",x)
    corrector <- c("AA","AC","AG","AT","AC","CC","CG","CT","AG","CG","GG","GT","AT","CT","GT","TT")
    names(corrector) <- c("AA","AC","AG","AT","CA","CC","CG","CT","GA","GC","GG","GT","TA","TC","TG","TT")
    as.character(corrector[x])
  }
  non_homozygote_refs[,"genotype"] <- switch_alleles(non_homozygote_refs[,"genotype"])
  
  
  
  #merge and sort by pos (and clean!)
  output <- rbind(non_homozygote_refs,homozygote_refs)
  rm("non_homozygote_refs","homozygote_refs")
  gc(verbose=verbose>4)
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
    dir.create(out_folder,recursive=T)
  }
  #also write a temp folder already
  dir.create(out_temp_path,recursive=T)
  
  
  #write an input-type file. This is going to contain the same data as the simple.format file.,
  #because we don't impute anything. But it has to be there to avoid crashing dependencies
  write.table(output, file=out_input_temp_path,sep="\t",row.names=F,col.names=F,quote=F)
  zip::zip(out_input_path,out_input_temp_path, mode = "cherry-pick", include_directories=F, recurse=F)

  
  #Write a pData file
  load(variables_path)
  timeStamp<-format(Sys.time(),"%Y-%m-%d-%H-%M")
  md5sum <- md5sum(vcf_path)
  imputation_type<-"vcf"
  f<-file(out_pdata_path,"w")
  writeLines(paste(c("uniqueID","filename","email","first_timeStamp","md5sum","gender","protect_from_deletion","should_be_imputed","imputemany_upload","upload_time","imputation_type"),collapse="\t"),f)
  # writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,sex,protect_from_deletion,should_be_imputed,imputemany_upload,upload_time,imputation_type),collapse="\t"),f)
  writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,sex,protect_from_deletion,should_be_imputed,imputemany_upload,upload_time,imputation_type),collapse="\t"),f)
  
  close(f)
  
  
  
  
  #write a simple-format-type file.
  #Disabled - it's optional now even for imputation, and for vcf it just doesn't make sense
  #since it's essentially going to be a copy of the "input"-data (zip, not vcf), only split by chromosome 
  # files_for_zipping <- vector()
  # for(chr in chromosomes){
  #   filename <- sub("__CHR__",chr,out_temp_simple_per_chr_path)
  #   o <- output[output[,"chr"]%in%chr,]
  #   write.table(o, file=filename,sep="\t",row.names=F,col.names=F,quote=F)
  #   files_for_zipping <- c(files_for_zipping, filename)
  # }
  # setwd(out_temp_path)
  # zip(out_simple_path, basename(files_for_zipping), flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
  
  
  
  
  #write a gen-format-type file.
  #e.g.
  # --- rs149201999 16050408 T C 0.947 0.053 0
  # --- rs146752890 16050612 C G 0.946 0.054 0
  #(note, in a sense this is a silly excercise - we shouldn't convert vcf-measurements back to 
  #a probabilistic imputation-output format - but we have to, to make sure everything don't crash
  #downstream)
  files_for_zipping <- vector()
  for(chr in chromosomes){
    if(verbose>3)print(paste0(Sys.time(),": Generating pseudo-.gen file from the vcf-data, at chr ",chr))
    
    filename <- sub("__CHR__",chr,out_temp_gen_per_chr_path)
    files_for_zipping <- c(files_for_zipping, filename)
    w1<-which(output[,"chr"]%in%chr)
    
    #generate new output format emulating gen format
    o<-data.frame(
      chr=rep("---",length(w1)),
      rsid=output[w1,"rsid"],
      pos=output[w1,"pos"],
      ref=output[w1,"ref"],
      alt=output[w1,"alt"],
      prob1=NA,
      prob2=NA,
      prob3=NA,
      genotype=output[w1,"genotype"],
      stringsAsFactors = F
    )
    
    
    
    #insert probabilities - homozygote refs
    w2<-which(o[,"genotype"] == paste0(o[,"ref"],o[,"ref"]))
    o[w2,"prob1"] <- 1
    o[w2,"prob2"] <- 0
    o[w2,"prob3"] <- 0
    
    
    #insert probabilities - heterozygotes
    w3<-which(o[,"genotype"] == paste0(o[,"alt"],o[,"ref"]) | o[,"genotype"] == paste0(o[,"ref"],o[,"alt"]))
    o[w3,"prob1"] <- 0
    o[w3,"prob2"] <- 1
    o[w3,"prob3"] <- 0
    
    
    
    #insert probabilities - homozygote alts
    w4<-which(o[,"genotype"] == paste0(o[,"alt"],o[,"alt"]))
    o[w4,"prob1"] <- 0
    o[w4,"prob2"] <- 0
    o[w4,"prob3"] <- 1
    
    
    #check how many are missing still (typically just a few, due to odd alt-notation)
    w5<-which(apply(is.na(o[,c("prob1","prob2","prob3")]),1,sum)>0)
    o[w5,"prob1"] <- 0
    o[w5,"prob2"] <- 0
    o[w5,"prob3"] <- 0
    if(verbose>2)print(paste0(Sys.time(),": When generating pseudo-.gen file from the vcf-data, there was ",length(w5)," missing variants assigned a 0-0-0 score at chr ",chr))
    
    #then write.out (and clean)
    write.table(o, file=filename,sep=" ",row.names=F,col.names=F,quote=F)
    rm("o")
    gc(verbose=verbose>8)
  }
  zip::zip(out_input_path,files_for_zipping, mode = "cherry-pick", include_directories=F, recurse=F)

  
  #deleting working folders and reset wd folder
  unlink(out_temp_path,recursive=T)
  
  
  
  
}








summarize_imputation<-function(
  uniqueID,
  runDir=paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID),
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
  #' @param export_simple_format A logical indicating whether the simple format script should be run (23andme-like format, based on hard-calls on .gen file)
  #' 
  #' @return The two paths to the downloadble .gen and simple-format zip-files. In addition these will be available in the ~/data/<uniqueID> folder
  
  #Set libraries
  library(tools)
  
  #set logging level
  verbose <- get_conf("verbose")
  
  #define programs
  gtools=paste0(get_conf("programs_path"),"gtool")
  plink=paste0(get_conf("programs_path"),"plink") 
  
  start_wd<-getwd()
  if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be length 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
  if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
  setwd(runDir)
  
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  
  if(class(export_simple_format)!="logical")stop(paste("export_simple_format must be logical, not",class(export_simple_format)))
  if(length(export_simple_format)!=1)stop(paste("export_simple_format must be length 1, not",length(export_simple_format)))
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be length 1, not",length(verbose)))
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
  if(length(gtools)!=1)stop(paste("gtools must be length 1, not",length(gtools)))
  if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
  
  if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
  if(length(plink)!=1)stop(paste("plink must be length 1, not",length(plink)))
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(file.exists(paste0(get_conf("data_path"),"/",uniqueID))){
    if(length(list.files(paste0(get_conf("data_path"),"/",uniqueID)))>0){
      stop(paste0("The destinationDir '",paste0(get_conf("data_path"),"/",uniqueID),"' already exists and has files in it. This is a major unforeseen error")  )
    }else{
      dir.create(paste0(get_conf("data_path"),"/",uniqueID),recursive=T)
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
    crontabs<-sub(" .+$","",sub(paste0("^.+Rscript ",get_conf("code_path"),"imputeme/"),"",crontabs))
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
  prepDestinationDir<-paste(get_conf("data_path"),"/",uniqueID,sep="")
  if(!file.exists(prepDestinationDir))dir.create(prepDestinationDir,recursive=T)
  
  
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
  
  
  #get logging level and other global variables
  verbose <- get_conf("verbose")
  server_role <- get_conf("server_role")
  hub_address <- get_conf("hub_address")
  
  
  #check arguments
  if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  
  #get sample specific variables
  pDataFile<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
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
    summary_folder<-paste0(get_conf("vcfs_path"),"vcf_folder_",uniqueID)
  }else{
    summary_folder<-paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID)  
  }
  
  
  #check if a simple-format file is available
  if(file.exists(paste0(get_conf("data_path"),uniqueID,"/",uniqueID,".simple_format.zip"))){
    export_simple_format<-TRUE
  }else{
    export_simple_format<-FALSE
    if(verbose>1)print(paste0(Sys.time(),": simple.format file not found - writing message accordingly"))
  }
  
  #If this is running as a node, we need to copy it back
  if(server_role== "Node"){
    cmd5 <- paste("scp -r ",get_conf("data_path"),uniqueID," ubuntu@",hub_address,":",get_conf("data_path"),sep="")
    out5<-system(cmd5)
    if(out5!=0)stop(paste0("Problem with transferring the data for ",uniqueID,". Possible connection error. Aborting. Nothing was changed, so after solving connectivity the function can be re-run as transfer_cleanup_and_mailout('",uniqueID,"') without problems"))
  }
  
  
  #making a link out to where the data can be retrieved	(different on hub and node, and also depends on vcf-file or not)
  if(server_role== "Node" ){
    
    if(!is_vcf_file){
      if(export_simple_format){
        cmd6 <- paste0("ssh ubuntu@",hub_address," 'ln -s ",get_conf("data_path"),uniqueID,"/",uniqueID,".simple_format.zip ",get_conf("code_path"),"www/",uniqueID,".simple_format.zip'")
        out6<-system(cmd6)
      }else{
        out6<-0
      }
      
      cmd7 <- paste0("ssh ubuntu@",hub_address," 'ln -s ",get_conf("data_path"),uniqueID,"/",uniqueID,".gen.zip ",get_conf("code_path"),"www/",uniqueID,".gen.zip'")
      out7<-system(cmd7)
      
    }else{
      out6<-out7<-0
    }
    
    cmd8 <- paste("ssh ubuntu@",hub_address," 'ln -s ",get_conf("data_path"),uniqueID,"/",uniqueID,"_data.json ",get_conf("code_path"),"www/",uniqueID,"_data.json'",sep="")
    out8<-system(cmd8)
    
    if(out6+out7+out8 > 0)stop(paste0("Problem with symlink-creation on Hub for ",uniqueID,". Possible connection error. Aborting."))
    
  }else if(server_role== "Hub" & is_vcf_file){
    file.symlink(
      from=paste(get_conf("data_path"),uniqueID,"/",uniqueID,"_data.json",sep=""),
      to=paste0(get_conf("code_path"),"/www/",uniqueID,"_data.json")
    )
    
  }else if(server_role== "Hub" & !is_vcf_file){
    
    if(export_simple_format){
      file.symlink(
        from=paste(get_conf("data_path"),uniqueID,"/",uniqueID,".simple_format.zip",sep=""),
        to=paste0(get_conf("code_path"),"/www/",uniqueID,".simple_format.zip")
      )
    }
    file.symlink(
      from=paste(get_conf("data_path"),uniqueID,"/",uniqueID,".gen.zip",sep=""),
      to=paste0(get_conf("code_path"),"www/",uniqueID,".gen.zip")
    )
    file.symlink(
      from=paste(get_conf("data_path"),uniqueID,"/",uniqueID,"_data.json",sep=""),
      to=paste0(get_conf("code_path"),"www/",uniqueID,"_data.json")
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
        suppressWarnings(library("gmailr",warn.conflicts = FALSE))
        gm_auth_configure( path =paste0(get_conf("misc_files_path"),"mailchecker.json"))
        gm_auth(email=get_conf("from_email_address"),cache=paste0(get_conf("misc_files_path"),"mail_secret"))
        prepared_email <- try(gm_mime() %>%
                                gm_to(email) %>%
                                gm_from(get_conf("from_email_address")) %>%
                                gm_subject("Imputation is ready") %>%
                                gm_html_body(message))
        mailingResult<-try(gm_send_message(prepared_email))
        
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
  if(server_role== "Node"){
    cmd9 <- paste("ssh ubuntu@",hub_address," 'rm -r ",summary_folder,"'",sep="")
    out9<-system(cmd9)
    
    #also don't leave the finished data here, if running as Node
    if(out9==0){
      unlink(paste(get_conf("data_path"),uniqueID,sep=""),recursive=TRUE)
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
  if(length(path)!=1)stop(paste("path must be length 1, not",length(path)))
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
  if(length(path)!=1)stop(paste("path must be length 1, not",length(path)))
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
  
  plink=paste0(get_conf("programs_path"),"plink")
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  if(class(uniqueID)!="character")stop(paste("uniqueID must be a character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  
  if(is.null(runDir)){
    runDir<-paste0(get_conf("imputations_path"),"/imputation_folder_",uniqueID,"/")
  }
  if(class(runDir)!="character")stop(paste("runDir must be a character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be length 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at",runDir))
  
  if(verbose>=0)print(paste0(Sys.time(),": The genes_for_good_cleaner was activated"))
  
  rawdata_file<-paste(get_conf("imputations_path"),"imputation_folder_",uniqueID,"/",uniqueID,"_raw_data.txt",sep="")
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
  plink=paste0(get_conf("programs_path"),"plink")
  
  #set logging level
  verbose <- get_conf("verbose")
  
  #other checks
  if(class(uniqueID)!="character")stop(paste("uniqueID must be a character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  
  if(is.null(runDir)){
    runDir<-paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID,"/")
  }
  if(class(runDir)!="character")stop(paste("runDir must be a character, not",class(runDir)))
  if(length(runDir)!=1)stop(paste("runDir must be length 1, not",length(runDir)))
  if(!file.exists(runDir))stop(paste("Did not find runDir at",runDir))
  
  rawdata_file<-paste(runDir,"/",uniqueID,"_raw_data.txt",sep="")
  if(!file.exists(rawdata_file))stop(paste("error in special-error-check: didn't find file at",rawdata_file))
  
  if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
  
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be length 1, not",length(verbose)))
  
  if(verbose>=0)print(paste0(Sys.time(),": The special_error_check was activated"))
  
  
  special_error_status<-vector()
  line_count_cmd<-paste0("wc -l ",rawdata_file)
  line_count_0<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
  
  #Common problem 1: mitochondrial SNPs (not used in any analysis anyway -)
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
  if(line_count_1-line_count_3>1)special_error_status <- c(special_error_status, paste0("INDEL removals (",line_count_1-line_count_3,")"))
  
  
  
  
  #Common problem 4: lack of sorting
  cmd_special_3<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out ",runDir,"/step_1_",uniqueID,"_sorting_check")
  system(cmd_special_3,intern=T,ignore.stdout = T, ignore.stderr = T)
  sorting_check<-readLines(paste0(runDir,"/step_1_",uniqueID,"_sorting_check.log"))
  if(length(grep("are out of order",sorting_check))>0){
    #replace X with 23
    cmd_sort_1<-paste0("sed 's/\\tX\\t/\\t23\t/' ",rawdata_file," > ",runDir,"/temp01.txt")
    system(cmd_sort_1)
    
    #remove non-numeric chromosomes
    cmd_sort_2<-paste0("awk '$2 == ($2+0)' ",runDir,"/temp01.txt > ",runDir,"/temp02.txt")
    system(cmd_sort_2)
    
    
    #sorting by chr then pos
    cmd_sort_3<-paste0("sort -k2 -k3 -g -o ",runDir,"/temp03.txt ",runDir,"/temp02.txt")
    system(cmd_sort_3)
    
    #then copy back
    file.copy(paste0(runDir,"/temp03.txt"),rawdata_file,overwrite=T)
    
    #re-check
    suppressWarnings(system(cmd_special_3,intern=T,ignore.stdout = T, ignore.stderr = T))
    sorting_check_2<-readLines(paste0(runDir,"/step_1_",uniqueID,"_sorting_check.log"))
    if(length(grep("are out of order",sorting_check_2))==0){
      line_count_4<-as.integer(sub(" .+$","",system(line_count_cmd,intern=T)))
      special_error_status<- c(special_error_status,paste0("sorting required (",line_count_3-line_count_4," lines removed)"))
    }else{
      special_error_status<- c(special_error_status,paste0("sorting check failed"))
    }
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
  cmd_special_3_out<-system(cmd_special_3,intern=T)
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
  
  
  #then re-check and decide future action (note, it's a little silly, but we have to run it twice because 
  #bulk running has step_1_[uniqueID] naming and single running just has step_1 naming. And we don't know 
  #which this is and there absolutely needs to be a map file, and it's more cumbersome to set up a check for 
  #it. In the future, maybe just have all runs do step_1_[uniqueID] naming. At least silence it so it don't 
  #clutter logs... it takes just a half second)
  cmd_final1<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1_",uniqueID)  
  cmd_final2<-paste0(plink," --noweb --23file ",rawdata_file," ",uniqueID," ",uniqueID," --recode --out step_1")  
  out_final1<-system(cmd_final1,ignore.stdout = T, ignore.stderr = T)
  out_final2<-system(cmd_final2,ignore.stdout = T, ignore.stderr = T)
  
  if(out_final1 == 0 & length(grep("failed built check",special_error_status))==0){
    if(verbose>0)print(paste0(Sys.time(),": Somehow the special errors section actually cleared up this one. These were the error messages: ",paste(special_error_status,collapse=", ")))
    #and move on
  }else{
    #however if still failing, we have to send a mail
    error1 <- system(cmd_final1,intern=T)
    error1 <- gsub("\b","",error1)
    message <- paste0("<HTML><body>",uniqueID," failed all attempts at starting imputation. It came with special error status:<br><b>", paste(special_error_status,collapse="<br>"),".</b><br><br>The last error message was this: <br><br><small>",paste(error1,collapse="<br>"),"</small><br></body></HTML>")
    if(verbose>=0)print(paste0(Sys.time(),": Sending error mail and giving up. This is the error:"))
    print(special_error_status)
    
    if(get_conf("from_email_address") != "" & get_conf("from_email_password") != ""){
      suppressWarnings(library("gmailr",warn.conflicts = FALSE))  
      gm_auth_configure( path =paste0(get_conf("misc_files_path"),"mailchecker.json"))
      gm_auth(email=get_conf("from_email_address"),cache=paste0(get_conf("misc_files_path"),"mail_secret"))
      prepared_email <- try(gm_mime() %>%
                              gm_to(get_conf("error_report_mail")) %>%
                              gm_from(get_conf("from_email_address")) %>%
                              gm_subject("An impute-me run has problem") %>%
                              gm_html_body(message))
      mailingResult<-try(gm_send_message(prepared_email))
      if(class(mailingResult)=="try-error" & verbose > 2)print(paste0(Sys.time(),": Mailing failed for special error check, but output has been logged."))
      stop("Special error check failed")
    }
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
  
  gtools=paste0(get_conf("programs_path"),"gtool")
  
  #set logging level
  verbose <- get_conf("verbose")
  suppressWarnings(library("shiny"))
  
  #checking namingLabel
  if(class(namingLabel)!="character")stop(paste("namingLabel must be character, not",class(namingLabel)))
  if(length(namingLabel)!=1)stop(paste("namingLabel must be length 1, not",length(namingLabel)))
  
  #checking verbosity
  if(class(verbose)!="numeric")stop(paste("verbose must be numeric, not",class(verbose)))
  if(length(verbose)!=1)stop(paste("verbose must be length 1, not",length(verbose)))
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
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  idFolder<-paste0(get_conf("data_path"),uniqueID)
  if(!file.exists(idFolder))stop(paste0(Sys.time(),": Did not find an idFolder at ",idFolder))
  genZipFile<-paste(idFolder,"/",uniqueID,".gen.zip",sep="")
  inputZipFile<-paste(idFolder,"/",uniqueID,".input_data.zip",sep="")
  cachedGenotypeFile<-paste(idFolder,"/",uniqueID,".",namingLabel,".gz",sep="")
  
  #start message
  if(file.exists(cachedGenotypeFile) & verbose>2)print(paste0(Sys.time(),": Found quick-access cache-file for label ",namingLabel,", getting required variants."))  
  if(!file.exists(cachedGenotypeFile) & verbose>0)print(paste0(Sys.time(),": Extracting variants in label ",namingLabel," directly from genome-wide data and saving them."))  
  
  
  
  #creating a temp folder to use
  idTempFolder<-paste(get_conf("data_path"),uniqueID,"temp",sep="/")
  if(file.exists(idTempFolder))stop(safeError(paste("Temp folder exists, this could indicate that",uniqueID,"is already worked on. Wait a little, or write administrators if you think this is a mistake")))
  
  
  #checking other variables
  if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
  if(length(gtools)!=1)stop(paste("gtools must be length 1, not",length(gtools)))
  if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
  if(class(ignore_indels)!="logical")stop(paste("ignore_indels must be ignore_indels, not",class(ignore_indels)))
  if(length(ignore_indels)!=1)stop(paste("ignore_indels must be length 1, not",length(ignore_indels)))
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
    
    
    dir.create(idTempFolder,recursive = T)
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
              include_these<-which(!duplicated(map[,2]))
              if(verbose>0)print(paste0(Sys.time(),": WARNING Found ",length(include_these)," duplicate row names errors for chr",chr,". Removing them. If this happens a lot, you should check that the correct genome-build is being used."))
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
    uniqueIDs<-list.files(get_conf("data_path"))
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste(get_conf("data_path"),uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  #getting a list of SNPs to analyze
  all_SNPs<-data.frame(SNP=vector(),chr_name=vector(),stringsAsFactors = F)		
  for(module in list.files(get_conf("code_path"),full.names=T)){
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
      if(file.exists(paste(get_conf("data_path"),uniqueID,"/temp",sep=""))){
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
  e<-try(load(paste0(get_conf("code_path"),"nonsenser/2015-12-16_all_coding_SNPs.rdata")))
  if(class(e)!="try-error"){
    for(uniqueID in uniqueIDs){
      genotypes<-try(get_genotypes(uniqueID,coding_snps,namingLabel="cached.nonsenser"))
    }
  }
  
  
  
  #getting the AllDiseases + ukbiobank SNPs if possible
  load(paste0(get_conf("code_path"),"AllDiseases/2021-01-28_all_gwas_snps.rdata"))
  e1<-gwas_snps
  load(paste0(get_conf("code_path"),"ukbiobank/2017-09-28_all_ukbiobank_snps.rdata"))
  e2<-gwas_snps
  e2<-e2[!rownames(e2)%in%rownames(e1),]
  e<-rbind(e1,e2)
  if(class(e)!="try-error"){
    for(uniqueID in uniqueIDs){
      genotypes<-try(get_genotypes(uniqueID,e,namingLabel="cached.all_gwas"))
    }
  }
  
  
  #getting the ethnicity SNPs if possible
  e<-try(load(paste0(get_conf("code_path"),"ethnicity/2017-04-03_ethnicity_snps.rdata")))
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
  
  
  uniqueIDs<-list.files(get_conf("data_path"))
  if(length(verbose)!=1)stop("verbose must be length 1")
  if(length(type)!=1)stop("type must be length 1")
  if(class(verbose)!="numeric")stop("verbose must be class numeric")
  if(class(type)!="character")stop("type must be class character")
  allowedTypes <- c("bash","R")
  if(!type%in%allowedTypes)stop("Type must be one of",paste(allowedTypes,collapse=", "))
  t0 <- Sys.time()
  
  if(type == "bash"){
    cmd1 <- paste(c(paste0("echo -n > ",get_conf("misc_files_path"),"temporary_file_for_overview.txt;"),
                    paste0("for filename in ",get_conf("data_path"),"id_*;"),
                    "do",
                    "file=$filename/pData.txt;",
                    paste0("cat $file >> ",get_conf("misc_files_path"),"temporary_file_for_overview.txt;"),
                    "done"),collapse=" ")
    system(cmd1)
    d<-readLines(paste0(get_conf("misc_files_path"),"temporary_file_for_overview.txt"))
    unlink(paste0(get_conf("misc_files_path"),"temporary_file_for_overview.txt"))
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
      pDataFile<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
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
  snps,
  uniqueIDs=NULL,
  namingLabel="cached"
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
  #' @param uniqueIDs Optional vector of uniqueIDs to check. Otherwise all uniqueIDs in ~/data are checked.
  #' @param namingLabel should default to cached, but it's a way of separately saving larger cached sets in a different file
  #' 
  
  
  #set logging level
  verbose <- get_conf("verbose")
  
  
  if(class(snps)!="character")stop("snps must be class character")
  if(any(duplicated(snps)))stop("snps must not have duplications")
  if(class(verbose)!="numeric")stop("verbose must be class numeric")
  if(length(verbose)!=1)stop("verbose must be length 1")
  
  if(!is.null(uniqueIDs)){
    if(!all(uniqueIDs%in%list.files(get_conf("data_path"))))stop("Did not find all uniqueIDs in ~/data")
  }else{
    uniqueIDs<-list.files(get_conf("data_path"))  
  }
  
  if(length(uniqueIDs)>1000){
    if(verbose>=0)print(paste0(Sys.time(),": This will remove ",length(snps)," variants from ",length(uniqueIDs)," samples - abort now if you are unsure of this. You have three seconds.")	)
    Sys.sleep(3)
  }
  
  
  
  for(uniqueID in uniqueIDs){
    cacheFile<-paste(get_conf("data_path"),uniqueID,"/",uniqueID,".",namingLabel,".gz",sep="")
    if(file.exists(cacheFile)){
      cache<-read.table(cacheFile,header=T,stringsAsFactors=F,row.names=1)
    }else{
      if(verbose>0)print(paste0(Sys.time(),": Didn't find a cache file for ",uniqueID," with namingLabel ",namingLabel))	
      next
    }
    if(any(snps%in%rownames(cache))){
      snpsFound<-snps[snps%in%rownames(cache)]
      if(verbose>0){
        print(paste0(Sys.time(),": removing ",	length(snpsFound)," snps from ",uniqueID,"- they are: ",paste(snpsFound,collapse=",")," and have values ",paste(cache[snpsFound,"genotype"],collapse=", ")))
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
    uniqueIDs<-list.files(get_conf("data_path"))
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste(get_conf("data_path"),uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  for(uniqueID in uniqueIDs){
    tempFolder<-paste(get_conf("data_path"),uniqueID,"/temp",sep="")
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
    uniqueIDs<-list.files(get_conf("data_path"))
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste(get_conf("data_path"),uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  for(uniqueID in uniqueIDs){
    dataFolder<-paste(get_conf("data_path"),uniqueID,sep="")
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
    uniqueIDs<-list.files(get_conf("data_path"))
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste(get_conf("data_path"),uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  
  if(is.null(updateProgress))updateProgress<-function(detail,value,max){return(NULL)}
  if(class(updateProgress)!="function")stop(paste("updateProgress must be function, not",class(updateProgress)))
  
  
  if(is.null(filename)){
    filename <- paste0(sample(1000:9999,1),sample(1000:9999,1),"_report.pdf")	
  }else{
    if(class(filename)!="character")stop("filename must be of class character")
    if(length(filename)!=1)stop("filename must be of length 1")
    
  }
  filepath <- paste0(get_conf("code_path"),"www/",filename)
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
    pData_file<-paste(get_conf("data_path"),uniqueID,"pData.txt",sep="/")
    if(!file.exists(pData_file))next
    pData<-read.table(pData_file,sep="\t",header=T,stringsAsFactors=F)
    first_timeStamps<-c(first_timeStamps,pData[1,"first_timeStamp"])
    
    user_log_file<-paste(get_conf("data_path"),uniqueID,"user_log_file.txt",sep="/")
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
  # for(w1 in list.files(get_conf("imputations_path"),full.names=T)){
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
  if(length(delay)!=1)stop(paste("delay must be length 1, not",length(delay)))
  
  
  if(is.null(uniqueIDs)){
    uniqueIDs<-list.files(get_conf("data_path"))
  }else{
    if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
    if(!all(file.exists(paste(get_conf("data_path"),uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
  }
  
  if(is.null(modules)){
    modules<-get_conf("modules_to_compute")
    if(verbose>2)print(paste0(Sys.time(),": modules wasn't explictly specified in the run_export_script, using the default from configuration.R instead: ",paste(modules,collapse=", ")))
  }else{
    if(class(modules)!="character")stop("modules must be of class character")
    if(!all(file.exists(paste0(get_conf("code_path"),modules))))stop("Not all modules given were found")
  }
  
  
  
  for(uniqueID in uniqueIDs){
    if(verbose>0)print(paste0(Sys.time(),": Running export script for ",uniqueID))
    
    outputList <- list()
    #importing standard pData stuff
    pDataFile<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
    pData<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t"),silent=T)
    if(class(pData)=="try-error"){
      print(paste("uniqueID",uniqueID,"was skipped due to inavailability of pData file"))
      next
    }
    if(nrow(pData)!=1)stop("pData file must have 1 row")
    
    #check existence of cached file
    cachedFile<-paste(get_conf("data_path"),uniqueID,"/",uniqueID,".cached.gz",sep="")
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
      version=get_conf("version")
    )
    
    
    #check if ethnicity is in pData, and if not save it there (because it is needed elsewhere)
    if(!"ethnicity"%in%colnames(pData)){
      source(paste(paste0(get_conf("code_path"),"ethnicity"  ,"/export_script.R")))
      ethnicity <- try(export_function(uniqueID))
      if(class(ethnicity)=="try-error"){
        ethnicity<-NA
      }else{
        ethnicity<-ethnicity[["guessed_super_pop"]]
      }
      pDataFile <- paste0(get_conf("data_path"),uniqueID,"/pData.txt")
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
      if(!file.info(paste0(get_conf("code_path"),module))["isdir"])next
      if("export_script.R" %in% list.files(paste0(get_conf("code_path"),module))){
        
        print(paste0(Sys.time(),": Running module ",module," for ",uniqueID))
        if(exists("export_function"))suppressWarnings(rm("export_function"))
        source(paste(paste0(get_conf("code_path"),module,"/export_script.R")))
        if(!exists("export_function"))stop(paste("In module",module,"there was an export_script.R without an export_function"))
        exp <- try(export_function(uniqueID))
        if(class(exp)=="try-error"){next}
        outputList[[module]] <-exp
      }
    }
    
    
    filename <- paste0(get_conf("data_path"),uniqueID,"/",paste(uniqueID,"data.json",sep="_"))
    
    
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
  all_md5sums<-read.table(paste0(get_conf("misc_files_path"),"md5sums.txt"),sep="\t",stringsAsFactors = F)[,1]
  
  
  otherPersons<-list.files(get_conf("data_path"),full.names=T)
  for(otherPerson in otherPersons){
    if(!file.info(otherPerson)[["isdir"]])next
    if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
    other_person_md5sum<-try(read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")[1,"md5sum"],silent=T)
    if(class(other_person_md5sum)=="try-error")next
    if(is.null(other_person_md5sum))next
    
    all_md5sums<-c(all_md5sums,other_person_md5sum)
  }
  #checking if this job is not already in queue
  for(otherPerson in paste0(list.files(get_conf("imputations_path"),full.names=T),"/")){
    if(!file.info(otherPerson)[["isdir"]])next
    
    raw_data_file<-grep("raw_data\\.txt",list.files(otherPerson,full.names=T),value=T)
    if(length(raw_data_file)!=1)stop("odd")
    other_person_md5sum<-md5sum(raw_data_file)
    all_md5sums<-c(all_md5sums,other_person_md5sum)
    
  }
  print(paste(sum(duplicated(all_md5sums)),"of",length(all_md5sums),"were duplicated"))
  all_md5sums <- unique(all_md5sums)
  writeLines(all_md5sums,paste0(get_conf("misc_files_path"),"md5sums.txt"))
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
  server_role <- get_conf("server_role")
  hub_address <- get_conf("hub_address")
  verbose <- get_conf("verbose")
  if(server_role!="Node")stop("This function only works when running on nodes")
  
  
  
  if(class(uniqueIDs)!="character")stop("uniqueIDs must be a character")
  if(!all(nchar(uniqueIDs)==12))stop("uniqueIDs must be of length 12")
  if(length(grep("^id_",uniqueIDs)) != length(uniqueIDs))stop("all uniqueIDs must start with id_")
  
  
  #check what exists locally  
  folders_imputation<-sub("imputation_folder_","",list.files(get_conf("imputations_path")))
  folders_data<-list.files(get_conf("data_path"))
  
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
      cmd1 <- paste0("ssh ubuntu@",hub_address," 'cat ",get_conf("imputations_path"),"imputation_folder_",uniqueID,"/job_status.txt'")
      status<-system(cmd1,intern=T)
      if(status!="Job is remote-running")stop(paste("Status for",uniqueID,"was not remote-running. This must be the case for a reset. Aborting with no change. Status was",status))
    }
  }
  
  
  #check if there are overlaps
  if(length(intersect(folders_imputation,folders_data))>0)stop("There are folders both in ~/imputation and ~/data we haven't seen that before, but should probably be checked manually")
  
  
  if(length(folders_data)>0){
    if(verbose>0)print(paste0(Sys.time(),": Note that there was ",length(folders_data)," folders in ~/data which will also be deleted and reset: ",paste(folders_data,collapse=", ")))
  }else{
    if(verbose>0)print(paste0(Sys.time(),": Deleting ",length(uniqueIDs)," uniqueIDs from local ~/imputation folder."))
  }
  
  
  
  
  #check the bulk_imputations folder (can always be deleted)
  bulk_to_delete<-list.files(get_conf("bulk_imputations_path"))
  if(length(bulk_to_delete)==1){
    if(verbose>0)print(paste0(Sys.time(),": Also deleting one folder in ",get_conf("bulk_imputations_path")))
  }else{
    if(verbose>0)print(paste0(Sys.time()," Deleting ",length(bulk_to_delete)," folders in ",get_conf("bulk_imputations_path"),": ",paste(bulk_to_delete,collapse=", ")))
  }
  unlink(paste0(get_conf("bulk_imputations_path"),bulk_to_delete),recursive=T)  
  
  #set job ready tag
  if(verbose>0)print(paste0(Sys.time(),": Setting Job ready tag for ",length(uniqueIDs)," uniqueIDs on hub at: ",hub_address))
  for(uniqueID in uniqueIDs){
    cmd2 <- paste0("ssh ubuntu@",hub_address," 'echo Job is ready > ",get_conf("imputations_path"),"/imputation_folder_",uniqueID,"/job_status.txt'")
    system(cmd2)
  }
  
  
  #doing deletion
  if(length(folders_imputation)>0){
    unlink(paste0(get_conf("imputations_path"),"imputation_folder_",folders_imputation),recursive=T)  
  }
  if(length(folders_data)>0){
    unlink(paste0(get_conf("data_path"),folders_data),recursive=T)
  }
  
}









summarize_imputemany_json<-function(
  uniqueIDs, 
  name
){
  #' function to check if a given uniqueID is the last in a batch upload, 
  #' and if so summarize all of the uniqueIDs in that batch and prepare for send-off
  #' 
  #' 
  #' @param uniqueIDs A list of uniqueIDs
  #' @param name The name of the output file
  #' 
  #' @return A web-path for an xlsx-file containing the summarized information from the requested uniqueIDs
  
  
  library("jsonlite")  
  library("openxlsx")
  library("tools")
  
  #check name is ok and unused
  if(class(name)!="character")stop(paste("name must be character, not",class(name)))
  if(length(name)!=1)stop(paste("name must be length 1, not",length(name)))
  
  
  #check uniqueIDs are ok
  if(class(uniqueIDs)!="character")stop(paste("uniqueIDs must be character, not",class(uniqueIDs)))
  if(length(uniqueIDs)<=1)stop(paste("uniqueIDs must be length more than 1, not",length(uniqueIDs)))
  missing_files <- vector()
  for(uniqueID in uniqueIDs){
    if(!file.exists(paste0(get_conf("data_path"),uniqueID)))missing_files<-c(missing_files,uniqueID)
  }
  if(length(missing_files)>0)  stop(paste("These",length(missing_files),"uniqueIDs were missing:",paste(missing_files,collapse=", ")))
  missing_json <- vector()
  for(uniqueID in uniqueIDs){
    json_path <- paste0(get_conf("data_path"),uniqueID,"/",uniqueID,"_data.json")
    if(!file.exists(json_path))missing_json<-c(missing_json,uniqueID)
  }
  if(length(missing_json)>0)  stop(paste("JSON files were missing for these",length(missing_json),"uniqueIDs:",paste(missing_json,collapse=", ")))
  
  
  #get hub_address
  hub_address <- get_conf("hub_address")
  
  #in this for-loop we read in all the json data.
  #the strategy is to start with a data.frame with uniqueIDs as rows (since that is constant), 
  #and then just add on columns as we go (and then transpose it, but that's for later)
  o1 <- data.frame(row.names=uniqueIDs)
  for(uniqueID in uniqueIDs){
    json_path <- paste0(get_conf("data_path"),uniqueID,"/",uniqueID,"_data.json")
    data<-fromJSON(json_path)
    o1[uniqueID,"ethnicity"]<-data[["ethnicity"]][["guessed_super_pop"]]
    
    
    #get the original filename from pData
    pDataFile<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
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
  trait_path1 <- paste0(get_conf("code_path"),"AllDiseases/2021-01-28_trait_overview.xlsx")
  library(openxlsx)
  traits1 <- read.xlsx(trait_path1,rowNames=T)
  insert_block1 <- traits1[rownames(o2),]
  colnames(insert_block1) <- paste0("AllDiseases_",colnames(insert_block1))
  
  #add in phenodata for UKbiobank
  trait_path2 <- paste0(get_conf("code_path"),"ukbiobank/2017-09-28_trait_overoverview.rdata")
  traits2_name<-load(trait_path2)
  traits2<-get(traits2_name)
  insert_block2 <- traits[rownames(o2),]
  colnames(insert_block2) <- paste0("ukbiobank_",colnames(insert_block2))
  
  
  #add in phenodata for all-SNP prs
  trait_path3 <- paste0(get_conf("code_path"),"/prs/2021-02-11_study_list.xlsx")
  traits3 <- read.xlsx(trait_path3,rowNames=F)
  traits3<-  traits3[!is.na(traits3[,"file_to_read"]),]
  rownames(traits3)<-traits3[,"file_to_read"]
  insert_block3 <- traits3[rownames(o2),]
  colnames(insert_block3) <- paste0("prs_",colnames(insert_block3))
  
  
  #merge
  o3<-cbind(o2, insert_block1, insert_block2, insert_block3)
  
  #output summary file - the grapper preventer is to prevent fishing for names (since they are just time-stamps)
  grapper_preventer <- paste(sample(LETTERS,8),collapse="")  
  dir_out <- paste0("www/summary_",grapper_preventer,"/")
  file_out <- paste0(dir_out,name,"_summary.xlsx")
  file_out_long <- paste0(get_conf("code_path"),file_out)
  if(length(grep("/$",hub_address))){
    file_out_web <- paste0(hub_address,file_out)  
  }else{
    file_out_web <- paste0(hub_address,"/",file_out)
  }
  if(file.exists(dirname(file_out_long)))stop("The grapper preventer generated a random number that already exists")
  dir.create(dirname(file_out_long),recursive=T)
  write.xlsx(o3,file=file_out_long,rowNames=T, firstRow=T, firstCol=T)
  
  
  Sys.sleep(1)
  
  return(file_out_web)
}






check_for_rare_nonbiallic_snps<-function(
  uniqueID,
  chromosomes=NULL
){
  #' check for rare nonbiallic snps
  #' 
  #' There's a rare error type that we've seen maybe 10-11 times now
  #' where a few, like less than 20, rows of the input data have mismatch in
  #' allele-types. Not just strand-flips - impossible mis-match. If those rows
  #' are removed it'll process fine, and if audited it typically turns out
  #' it's either updated dbsnp (very rare) or else just seems like typos
  #' in the input. 
  #' 
  #' This is so rare and so bad that it shouldn't be allowed to run 
  #' by auto-pilot, but the function here should be able to give a better
  #' error output so it can be easily fixed if need be. 
  #' 
  #' The function can also be run interactively, which is useful for persistent
  #' problems
  #' 
  #' @param uniqueID The uniqueID to check
  #' @param chromosomes The chromosomes to check. Optional. Defaults to 1-22 and X.
  #' 
  #' @return always end in an error, because it needs to stop the processing.
  
  
  #define constants
  verbose <- get_conf("verbose")
  
  #verbose doesn't matter, this should always be printed.
  if(verbose>=0)print(paste0(Sys.time(),": ERROR. Starting check_for_rare_nonbiallic_snps for ",uniqueID," - this will result in failure, but hopefully will give informative output."))
  
  #define program paths
  shapeit=paste0(get_conf("programs_path"),"shapeit.v2.904.3.10.0-693.11.6.el7.x86_64/bin/shapeit")
  plink=paste0(get_conf("programs_path"),"plink")
  impute2=paste0(get_conf("programs_path"),"impute_v2.3.2_x86_64_static/impute2")
  sample_ref=paste0(get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample")
  
  
  #load libraries
  library("tools")
  
  #check input
  if(class(uniqueID)!="character")stop(paste("uniqueID must be class character, not",class(uniqueID)))
  if(length(uniqueID)!=1)stop(paste("uniqueID must be length 1, not",length(uniqueID)))
  basefolder <- paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID)
  if(!file.exists(basefolder))stop(paste("Didn't find imputation folder at",basefolder))
  inputfile_path <- paste0(basefolder,"/",uniqueID,"_raw_data.txt")
  if(!file.exists(inputfile_path))stop(paste("Didn't find inputfile_path at",inputfile_path))
  
  #test that impute2 static can run (or switch to dynamic)
  impute2_blank_run_out <- suppressWarnings(system(impute2,intern=T,ignore.stderr = T) )
  if(attr(impute2_blank_run_out,"status")==139){
    impute2 <- paste0(get_conf("programs_path"),"impute_v2.3.2_x86_64_dynamic/impute2")
  }
  
  
  #set chromosomes if not given - or check
  if(is.null(chromosomes)){
    chromosomes <- c("X",as.character(1:22))  
  }else{
    if(class(chromosomes)!="character")stop(paste("chromosomes must be class character, not",class(chromosomes)))
    if(length(chromosomes)>23)stop(paste("chromosomes must be maximally length 23, not",length(chromosomes)))
    unknown<-chromosomes[!chromosomes%in%c("X",as.character(1:22))]
    if(length(unknown)>0)stop(paste("chromosomes contained unknown entries:",paste(unknown,collapse=", ")))
  }
  
  
  #this blocks allows the check_for_rare_nonbiallic_snps script to be run also on new samples, not halfway through processing
  #(useful when suspecting many non-biallelic alleles. Most often the block is skipped however.)
  required_files<-c("step_2_exclusions","step_2_chrX.ped")
  if(any(!file.exists(required_files))){
    if(verbose>=0)print(paste0(Sys.time(),": Re-running first parts of imputation script in preparation for check_for_rare_nonbiallic_snps from scratch."))
    cmd1<-paste0(plink," --23file ",inputfile_path," ",uniqueID," ",uniqueID," --recode --out step_1")
    out1<-system(cmd1,ignore.stderr=T, ignore.stdout=T)
    map<-read.table('step_1.map',sep='\t',stringsAsFactors=F,comment.char="")
    exclude<-map[duplicated(map[,4]),2]
    if(verbose>1)print(paste0(Sys.time(),': Removed ',length(exclude),' SNPs that were duplicated'))
    write.table(exclude,file='step_2_exclusions',sep='\t',row.names=FALSE,col.names=F,quote=F)
  }
  
  
  
  #loop over chromosomes
  messages <- vector()
  for(chr in chromosomes){
    if(verbose>2)print(paste0(Sys.time(),": Checking chr ",chr," for rare non-biallelic variants."))
    
    
    #This is a copy of the real impute run - but with detailed error collection
    #First in loop - extract only one specific chromosome
    cmd2<-paste(plink," --file step_1 --chr ",chr," --recode --out step_2_chr",chr," --exclude step_2_exclusions",sep="")
    out2<-system(cmd2,ignore.stdout = T, ignore.stderr = T)
    
    #if X chromosome is missing it is allowed to skip forward
    if(out2 %in% c(12,13) & chr == "X"){
      print("Didn't find X-chr data, so skipping that")
      next
    }
    
    #Then check for strand flips etc. 
    cmd3<-paste0(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log")
    system(cmd3,ignore.stdout = T, ignore.stderr = T)
    
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
    cmd4<-paste0(shapeit," -check --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz ",get_conf("programs_path"),"ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions")
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
    
    message<-paste0("<html><body>",paste(messages,collapse="<br>"),"</body></html>")
    suppressWarnings(library("gmailr",warn.conflicts = FALSE))  
    gm_auth_configure( path =paste0(get_conf("misc_files_path"),"mailchecker.json"))
    gm_auth(email=get_conf("from_email_address"),cache=paste0(get_conf("misc_files_path"),"mail_secret"))
    prepared_email <- try(gm_mime() %>%
                            gm_to(get_conf("error_report_mail")) %>%
                            gm_from(get_conf("from_email_address")) %>%
                            gm_subject("An impute-me run has problem") %>%
                            gm_html_body(message))
    mailingResult<-try(gm_send_message(prepared_email))
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
  
  #verbose doesn't matter, this should always be printed.
  print(paste0(Sys.time(),": ERROR. Starting count_x_chr_entries for chr",chr," - this will result in failure, but hopefully will give informative output."))
  
  if(class(chr)!="character")stop(paste("chr must be class character, not ",class(chr)))
  if(length(chr)!=1)stop(paste("chr must be length 1, not ",length(chr)))
  
  bulk_folder<-list.files("~/bulk_imputations",full.names=T)
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
    
    
    message<-paste0("<html><body>",paste(messages,collapse="<br>"),"</body></html>")
    suppressWarnings(library("gmailr",warn.conflicts = FALSE))
    gm_auth_configure( path =paste0(get_conf("misc_files_path"),"mailchecker.json"))
    gm_auth(email=get_conf("from_email_address"),cache=paste0(get_conf("misc_files_path"),"mail_secret"))
    prepared_email <- try(gm_mime() %>%
                            gm_to(get_conf("error_report_mail")) %>%
                            gm_from(get_conf("from_email_address")) %>%
                            gm_subject("An impute-me run has problem") %>%
                            gm_html_body(message))
    mailingResult<-try(gm_send_message(prepared_email))
    
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
  if(length(map_file)!=1)stop(paste("map_file must be length 1, not",length(map_file)))
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










convert_gen_file<-function(
  path, 
  export_type, 
  convert=TRUE, 
  quality_threshold=0.8, 
  split_by_chr=FALSE, 
  runDir="~/gens"
  ){
  #' convert gen file
  #' 
  #' A function that can convert the .gen files that are the main genomic impute-me-output
  #' into various other commonly used formats. This function is exposed to users, since they may
  #' want to convert their results
  #' 
  #' @param path The path to the file that needs conversion
  #' @param export_type The fily-type requested, e.g. vcf, simple, (plink?)
  #' @param convert logical - wheter to actually perform the conversions, or just the initial checkups
  #' @param quality_threshold The dosage threshold beneath which variants are not included in conversion
  #' @param split_by_chr A logical that specifies if the output should still be split by chromosome (standard within impute.me and lighter on memory). If FALSE, the file will be concatenated.
  #' @param runDir A path where output files are placed as well as temporary files during the conversion process
  #' 
  #' @return The path of the converted file
  

  #define programs
  gtools=paste0(get_conf("programs_path"),"gtool")
  plink=paste0(get_conf("programs_path"),"plink" )
  suppressMessages(library("data.table"))
  suppressWarnings(library("shiny"))
  
  #set verbose level
  verbose <- get_conf("verbose")
  if(class(verbose)!="numeric")stop(safeError(paste("verbose must be numeric, not",class(verbose))))
  if(length(verbose)!=1)stop(safeError(paste("verbose must be length 1, not",length(verbose))))
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
  
  
  
  #checking convert
  if(class(convert)!="logical")stop(safeError(paste("convert must be logical, not",class(convert))))
  if(length(convert)!=1)stop(safeError(paste("convert must be length 1, not",length(convert))))
  
  
  #checking quality_threshold
  if(class(quality_threshold)!="numeric")stop(safeError(paste("quality_threshold must be numeric, not",class(quality_threshold))))
  if(length(quality_threshold)!=1)stop(safeError(paste("quality_threshold must be length 1, not",length(quality_threshold))))
  if(quality_threshold < 0.3 | quality_threshold > 1)stop(safeError(paste("quality_threshold must be between 0.3 and 1, not",quality_threshold)))
  
  #checking export_type
  allowed_types <- c("vcf","simple")
  if(class(export_type)!="character")stop(safeError(paste("export_type must be character, not",class(export_type))))
  if(length(export_type)!=1)stop(safeError(paste("export_type must be length 1, not",length(export_type))))
  if(!export_type%in%c(allowed_types))stop(safeError(paste("export_type must be between among:",paste(allowed_types,collapse=", "))))
  
  #checking split_by_chr
  if(class(split_by_chr)!="logical")stop(safeError(paste("split_by_chr must be logical, not",class(split_by_chr))))
  if(length(split_by_chr)!=1)stop(safeError(paste("split_by_chr must be length 1, not",length(split_by_chr))))
  
  #checking runDir
  if(class(runDir)!="character")stop(safeError(paste("runDir must be character, not",class(runDir))))
  if(length(runDir)!=1)stop(safeError(paste("runDir must be length 1, not",length(runDir))))
  if(!file.exists(runDir))stop(safeError(paste0("Did not find the requested runDir at ",runDir)))
  if(length(grep("/$",runDir))!=0)runDir <- sub("/$","",runDir) #remove trailing slash
  
  #extensive checking of input file type and content
  if(class(path)!="character")stop(safeError(paste("path must be character, not",class(path))))
  if(length(path)!=1)stop(safeError(paste("path must be lenght 1, not",length(path))))
  if(!file.exists(path))stop(safeError(paste0("Did not find the requested path at ",path)))
  if(substr(path,nchar(path)-7,nchar(path))!=".gen.zip" & convert)stop(safeError(paste0("All accepted files must be of impute.me standard .gen format which ends in .gen.zip, not ",substr(path,nchar(path)-7,nchar(path)))))
  zip_contents_list<-unzip(path,list=T)[,"Name"]
  zip_content_problems<-vector()
  zip_file_content_by_chromosome <- vector()
  for(required_file in paste0("_chr",1:22,".gen")){
    if(length(grep(required_file,zip_contents_list))!=1){
      zip_content_problems<-c(zip_content_problems,required_file)
    }else{
      zip_file_content_by_chromosome<-c(zip_file_content_by_chromosome,grep(required_file,zip_contents_list,value=T))
    }
  }
  if(length(zip_content_problems)>0){
    stop(safeError(paste0("Found a problem with the contents of ",path,": ",length(zip_content_problems)," expected endings were not found: ",paste(zip_content_problems,collapse=", "))))
  }
  if(length(grep("_chrX.gen",zip_contents_list))==1){
    x_included<-TRUE
    zip_file_content_by_chromosome<-c(zip_file_content_by_chromosome, grep("_chrX.gen",zip_contents_list,value=T))
  }else{
    x_included<-FALSE
  }
  if(any(!zip_contents_list%in%zip_file_content_by_chromosome)){
    extra_files<-zip_contents_list[!zip_contents_list%in%zip_file_content_by_chromosome]
    stop(safeError(paste0("There were ",length(extra_files)," files in ",path," that were not recognised and does not match with standard impute.me gen.zip files: ",paste(extra_files, collapse=", "))))
  }
  
  
  #set a sample ID from the basename, but without any special characters
  sampleID <- gsub("[\\$\\&\\+\\,\\:\\;\\=\\?\\@\\#\\\"\\\']","",gsub("\\ ","_",sub(".gen.zip","",basename(path))))
  
  
  #unpack and light check of contents from each chromosome
  unzipResults<-unzip(path,exdir=runDir)
  if(!all(basename(unzipResults)%in%zip_file_content_by_chromosome) | !all(zip_file_content_by_chromosome%in%basename(unzipResults))){
    stop(safeError("Problems with unzipped content not matching expected content"))
  }
  for(chr_file in zip_file_content_by_chromosome){
    testRead<-read.table(paste0(runDir,"/",chr_file),sep=" ",nrows=10)
    if(ncol(testRead)!=8)stop(safeError(paste("Didn't find 8 columns in",chr_file)))
    if(class(testRead[,5])!="character" | !class(testRead[,6])%in%c("integer","numeric") | !class(testRead[,7])%in%c("integer","numeric") ){
      stop(safeError(paste0("Some of the columns in ",chr_file," did not have expected class. Column 5 is expected to be character, it was ",class(testRead[,5]),". Column 6 is expected to be numeric, it was ",class(testRead[,6]),". Column 7 is expected to be numeric, it was, ",class(testRead[,7]))))
    }
  }
  
  #break if not convert
  if(!convert){
    unlink(paste0(runDir,"/",zip_file_content_by_chromosome))
    return(TRUE)
  }else{
    if(verbose>2)print(paste0(Sys.time(),": completed initial checks and proceeding to real conversion"))
  }
  
  
  #Export to simple format
  if(export_type=="simple"){
    
    #make sampleFile
    sampleFile<- paste(runDir,"/",sampleID,"_samples.txt",sep="")
    f<-file(sampleFile,"w")
    writeLines("ID_1 ID_2 missing sex",f)
    writeLines("0 0 0 D",f)
    writeLines(paste(sampleID,sampleID,"0.0 2 "),f)#gender probably doesn't matter here
    close(f)
    
    for(genFile in zip_file_content_by_chromosome){
      chr <- sub("\\.gen$","",sub("^.+_chr","",genFile))
      
      #catching some odd observations of empty 'chr' handles, seen when running low on memory
      if(nchar(chr)==0){
        if(verbose>0)print(paste0(Sys.time(),": Observed an odd length-0 chromosome genFiles with filename: ",genFile,". It was skipped, but may want to investigate logs closer."))
        next
      }
      
      #Else continue
      if(verbose>1)print(paste0(Sys.time(),": Creating simple-format data from chromosome ",chr))

      #make list of indels
      cmd2<-paste0("awk -F' ' '{ if ((length($4) > 1 ) || (length($5) > 1 ) || $4 == \"-\" || $5 == \"-\") print $2 }' ",runDir,"/",genFile,"> ",runDir,"/step_8_chr",chr,"_snps_to_exclude")
      system(cmd2)
      
      #exclude indels
      cmd3 <- paste0(gtools," -S --g ",runDir,"/",genFile," --s ",sampleFile," --exclusion ",runDir,"/step_8_chr",chr,"_snps_to_exclude --og ",runDir,"/step_8_chr",chr,".gen")
      system(cmd3,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
      #Convert to ped format
      cmd4 <- paste(gtools," --threshold ",call_threshold," -G --g ",runDir,"/step_8_chr",chr,".gen --s ",sampleFile," --chr ",chr," --snp",sep="")
      system(cmd4,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
      #reform to plink fam/bim/bed file
      cmd5 <- paste(plink," --file ",runDir,"/step_8_chr",chr,".gen --recode --transpose --noweb --out ",runDir,"/step_9_chr",chr,sep="")
      system(cmd5,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
      
      #re-order to 23andme format
      cmd6<-paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' ",runDir,"/step_9_chr",chr,".tped  > ",runDir,"/step_10_chr",chr,".txt",sep="")
      system(cmd6)
      
      #The step 8 and also 9 sometime fails for no apparent reason. Probably memory. We therefore make a checkup, where
      #it is checked if the file actually exists and if not - a more complicated step splits it up in chunks.
      #It's not looking nice, but at least the split-up only needs to run in very low memory settings
      fileExists<-file.exists(paste(runDir,"/step_10_chr",chr,".txt",sep=""))
      if(fileExists){
        size<-file.info(paste(runDir,"/step_10_chr",chr,".txt",sep=""))["size"]
      }else{
        size<-0
      }
      
      #	re-run if it's less than 100 bytes (fair to assume something was wrong then)
      if(size<100 ){
        if(verbose>0)print(paste0("retrying step 8-9 command for chr",chr,". Trying to split it in pieces (non-normal low memory running)"))
        cmd7 <- paste("split --lines 5000000 ",runDir,"/step_8_chr",chr,".gen ",runDir,"/step_8_extra_chr",chr,".gen",sep="")
        system(cmd7)
        chunks<-grep(paste("step_8_extra_chr",chr,"\\.gena[a-z]$",sep=""),list.files(runDir),value=T)
        for(chunk in chunks){
          ch<-sub("^.+\\.","",chunk)
          cmd8 <- paste(gtools," --threshold ",quality_threshold," -G --g ",runDir,"/",chunk," --s ",sampleFile," --chr ",chr," --snp",sep="")
          system(cmd8,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          #reform to plink fam/bim/bed file
          cmd9 <- paste(plink," --file ",runDir,"/",chunk," --recode --transpose --noweb --out ",runDir,"/step_9_chr",chr,"_",ch,sep="")
          system(cmd9,ignore.stderr=ignore.stderr, ignore.stdout=ignore.stdout)
          #re-order to 23andme format
          cmd10<-paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' ",runDir,"/step_9_chr",chr,"_",ch,".tped  > ",runDir,"/step_9_chr",chr,"_split_",ch,".txt",sep="")
          system(cmd10)
        }
        cmd11<-paste("cat ",paste(paste(runDir,"/step_9_chr",chr,"_split_",sub("^.+\\.","",chunks),".txt",sep=""),collapse=" ")," > ",runDir,"/step_10_chr",chr,".txt",sep="")
        system(cmd11)
      }
      
      #remove NN
      cmd12 <- paste("awk '{ if($4 != \"NN\") print}' ",runDir,"/step_10_chr",chr,".txt  > ",runDir,"/step_11_chr",chr,".txt",sep="")
      system(cmd12)
      
      
      #remove duplicates
      cmd13 <- paste("awk -F',' '!seen[$1]++' ",runDir,"/step_11_chr",chr,".txt > ", runDir,"/",sub("\\.gen$","",genFile),".simple_format.txt",sep="")
      system(cmd13)
      
      
      #removing some particularly big temporary files
      # unlink(list.files(runDir,pattern=paste0("^step_8_chr",chr),full.names=T))
      # unlink(list.files(runDir,pattern=paste0("^step_9_chr",chr),full.names=T))
      # unlink(list.files(runDir,pattern=paste0("^step_10_chr",chr),full.names=T))
    }
    
    
    #removing a bunch of remaining half-way files from simple-format generation -
    #all smaller size (that were still nice to have if the above loop failed)
    # unlink(list.files(runDir,pattern="^step_5_",full.names=T))
    # unlink(list.files(runDir,pattern="^step_6_",full.names=T))
    # unlink(list.files(runDir,pattern="^step_7_",full.names=T))
    # unlink(list.files(runDir,pattern="^step_11_",full.names=T))
    
    stop("Made it to here. Decide what to do, depending on split_by_chr")
    
    #zipping and moving simple_format files
    # chromosomes<-sub(".gen","",sub("^.+_chr","",zip_file_content_by_chromosome))
    # if(verbose>0)print(paste0(Sys.time(),": Zipping files for simple-format export"))
    # zipFile_simpleformat<-paste(runDir,paste(sampleID,".simple_format.zip",sep=""),sep="/")
    # simpleformatFiles<-paste(sampleID,"_chr",chromosomes,".simple_format.txt",sep="")
    # zip(zipFile_simpleformat, simpleformatFiles, flags = "-r9Xq", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
    # zipFile_simpleformat_destination <- paste(runDir,basename(zipFile_simpleformat),sep="/")
    # move_result <- suppressWarnings(file.rename(zipFile_simpleformat, zipFile_simpleformat_destination))
    # if(!move_result){ #this would fail, for example if data is on another volume. But we can still copy
    #   file.copy(zipFile_simpleformat, zipFile_simpleformat_destination)
    #   unlink(zipFile_simpleformat)
    # }

    
  }
  
  
  #Export to vcf format
  if(export_type=="vcf"){
    #Code adapted from gen2vcf by Tokhir Dadaev (Thanks!)
    #https://rdrr.io/github/oncogenetics/oncofunco/src/R/gen2vcf.R
    
    outVCFHeader = "##fileformat=VCFv4.1"
    
    for(genFile in zip_file_content_by_chromosome){
      if(verbose > 4)print(paste0(Sys.time(),": Processing gen-file ",genFile))
      
      chr<-sub(".gen","",gsub(".+_chr","",genFile))
      out_chr_path <- paste0(runDir,"/",sampleID,"_chr",chr,".vcf")
      
      gen <- data.table::fread(genFile)
      
      MAP <- cbind(chrom = chr, gen[, 1:5 ])
      AB <- gen[, seq(7, ncol(gen), 3),drop=F]
      BB <- gen[, seq(8, ncol(gen), 3),drop=F] * 2
      AB <- gen[,7]
      BB <- gen[, 8] * 2
      AB_BB <- AB + BB
      colnames(AB_BB) <- sampleID
      dose <- cbind(MAP, AB_BB)
      d1 <- dose[, c(4, 3, 5, 6)]
      colnames(d1) <- c("POS", "ID", "REF", "ALT")
      d2 <- data.table::data.table(QUAL = 100,
                                   FILTER = "PASS",
                                   INFO = "INFO",
                                   FORMAT = "DS")
      out <- cbind("#CHROM" = chr, d1, d2, dose[, 7:ncol(dose), with = FALSE])
      
      if(split_by_chr){
        write(outVCFHeader, file = out_chr_path)
        write.table(out, file = out_chr_path,
                    append = TRUE, quote = FALSE, sep = "\t", row.names = FALSE)
      }else{
        write.table(out, file = out_chr_path,
                    append = FALSE, quote = FALSE, sep = "\t", row.names = FALSE,col.names=FALSE)
      }
    }
  }
}







