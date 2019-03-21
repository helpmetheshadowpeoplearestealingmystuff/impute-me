
# 
#Strategy - setup this to run every hour on the hour, 
#
# 50 * * * * Rscript /home/ubuntu/srv/impute-me/imputeme/imputation_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-impute-cron.log 2>&1


library("mailR")
library("rJava")
library("tools")
source("/home/ubuntu/srv/impute-me/functions.R")


#bulk running parameter
length_requested <- 10


#First checking if node is already at max load (maxImputations)
foldersToCheck<-grep("^imputation_folder",list.files("/home/ubuntu/imputations/"),value=T)
runningJobCount<-0
remoteRunningJobCount<-0
for(folderToCheck in foldersToCheck){
  job_status_file<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
  if(file.exists(job_status_file)){
    job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
    if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
  }
}

#then stop job if runs are already running
#if not, go to sleep and wait before re-checking. This is an optional thing, set in configuration.R, 
#but it's really smart because it allows the credit count of the AWS instnce to recover a bit, and 
#ultimately gives faster turn-around and more stable servers
if(runningJobCount>(maxImputations-1)){
  stop(paste("Found",runningJobCount,"running jobs, and max is",maxImputations,"so doing nothing"))
}else{
  Sys.sleep(seconds_wait_before_start)
}


#After sleeping period, wake up and re-check
foldersToCheck<-grep("^imputation_folder",list.files("/home/ubuntu/imputations/"),value=T)
runningJobCount<-0
for(folderToCheck in foldersToCheck){
  job_status_file<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
  if(file.exists(job_status_file)){
    job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
    if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
  }
}
if(runningJobCount>(maxImputations-1)){
  stop(paste("Found",runningJobCount,"running jobs, and max is",maxImputations,"so doing nothing"))
}




#If the computer is not too busy and the serverRole is node - we fetch 10 jobs
if(serverRole== "Node"){
  #sort checking order by time entered
  cmd1 <- paste("ssh ubuntu@",hubAddress," ls -l --time-style='+\\%Y-\\%m-\\%d-\\%H:\\%M:\\%S' /home/ubuntu/imputations/  | tail -n +2",sep="")
  remotedata<-system(cmd1,intern=T)
  Sys.sleep(0.2)
  remotedata_df<-as.data.frame(do.call(rbind,strsplit(remotedata,"\\s+")),stringsAsFactors=F)
  if(ncol(remotedata_df)==0)stop("Nothing found at hub server. Exit, stop and wait.")
  remotedata_df<-remotedata_df[order(remotedata_df[,6]),]
  remoteFoldersToCheck<-remotedata_df[,7]
  
  
  #check if there's any fast-queue jobs to put up-front. The fast-queue jobs is just a file with uniqueID
  #and then TRUE or FALSE. The TRUE or FALSE means if a bulk impute is allowed to 
  #take it or not
  #(they can be in priority queue either because they are paid, or because they are error-prone. Don't put error-prone in the bulk imputing line)
  cmd0 <- paste("ssh ubuntu@",hubAddress," cat /home/ubuntu/misc_files/fast_queue_emails.txt
                ",sep="")
  f1<-system(cmd0,intern=T)
  Sys.sleep(0.2)
  if(length(f1)>0){ #if there is a fast-queue file, we handle it
    f2<-do.call(rbind,strsplit(f1,"\t"))
    
    #first bump of the TRUE fast-queue entries (they are ok to run in bulk-impute)
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
      print(paste("Found job-status file and job is ready",remoteFolderToCheck))
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
    foldersToCheck<-grep("^imputation_folder",list.files("/home/ubuntu/imputations/"),value=T)
  }
}




#Then - no matter the role - we check locally which, if any, folders are ready to run
imputeThisFolder<-vector()
for(folderToCheck in foldersToCheck){
  job_status_file<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
  
  if(!file.exists(job_status_file)){
    print(paste("Didn't find a job-status file - should probably auto-delete",folderToCheck))
    next
  }
  job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
  if(job_status=="Job is not ready yet"){
    print(paste("Found job-status file - but job is not ready yet",folderToCheck))
    next
  }
  if(job_status=="Job is running"){
    print(paste("Found job-status file - but job is already running",folderToCheck))
    next
  }
  if(job_status=="Job is ready"){
    print(paste("Found job-status file and job is ready",folderToCheck))
    unlink(job_status_file)
    write.table("Job is running",file=job_status_file,col.names=F,row.names=F,quote=F)
    imputeThisFolder<-c(imputeThisFolder,folderToCheck)
    
  }
}


#Stop if none are found
if(length(imputeThisFolder)!= length_requested){
  stop(paste0("Not ",length_requested," folders were found to be ready for imputation"))
}else{
  print(paste("Found",length_requested,"ready sets to impute"))
}




#If script is still running, it means there was a job ready for imputation.

#Get the uniqueIDs that are lined up
uniqueIDs<-sub("^.+folder_","",imputeThisFolder)


#prepare a runDir (bulk_imputations-folder should already exist)
if(!file.exists("/home/ubuntu/bulk_imputations/"))dir.create("/home/ubuntu/bulk_imputations/")
runDir<-paste("/home/ubuntu/bulk_imputations/",format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"_bulk",sep="")
dir.create(runDir)
setwd(runDir)


#run the imputation
run_bulk_imputation(uniqueIDs, runDir)  



#delete the rundir afterwards (because files are now in summary_folder)
unlink(runDir,recursive=TRUE)


#loop over all samples and perform the summarization step.
for(uniqueID in uniqueIDs){
  print(paste("Looping over uniqueID",uniqueID,"for the summarization part"))
  
  #load variables specifically for this uniqueID
  summary_folder<-paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID)
  load(paste0(summary_folder,"/variables.rdata"))
  
    
  #summarizing files
  summarize_imputation(runDir=summary_folder,uniqueID=uniqueID)  
  
  
  #remove the summary_folder (tight on space when in bulk-mode)
  unlink(summary_folder,recursive = T)
  setwd("~")
  
  
  #check if all is ok
  if(!file.exists(paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip")))stop(paste("Didn't find gen file for",uniqueID))
  if(!file.exists(paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip")))stop(paste("Didn't find simple_format file for",uniqueID))
  

  #Run the genotype extraction routine
  crawl_for_snps_to_analyze(uniqueIDs=uniqueID)
  
  
  #Run the json extraction routine
  run_export_script(uniqueIDs=uniqueID)
  
  
  #If this is running as a node, we need to copy it back around here
  if(serverRole== "Node"){
    cmd5 <- paste("scp -r /home/ubuntu/data/",uniqueID," ubuntu@",hubAddress,":/home/ubuntu/data",sep="")
    system(cmd5)
  }
  
  
  

  #making a link out to where the data can be retrieved	(different on hub and node)
  if(serverRole== "Node"){
    cmd6 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip /home/ubuntu/srv/impute-me/www/",uniqueID,".simple_format.zip'",sep="")
    system(cmd6)
    
    cmd7 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip /home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip'",sep="")
    system(cmd7)
    
    cmd8 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json /home/ubuntu/srv/impute-me/www/",uniqueID,"_data.json'",sep="")
    system(cmd8)
    
    
  }else if(serverRole== "Hub"){
    file.symlink(
      from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip",sep=""),
      to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".simple_format.zip",sep="")
    )
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
    print(paste(Sys.time(),"skipping mail because it's from imputemany_upload"))
  }else{
    print(paste(Sys.time(),"sending results mail"))
    ip<-"https://www.impute.me"
    location_simple <- paste(ip,"/www/",uniqueID,".simple_format.zip",sep="")
    location_gen <- paste(ip,"/www/",uniqueID,".gen.zip",sep="")
    location_json <- paste(ip,"/www/",uniqueID,"_data.json",sep="")
    message <- paste("<HTML>We have completed imputation of your genome. You can retrieve your imputed genome at this address:<br><a href=",location_simple,">",location_simple,"</a><br><br>You can also go to <a href='www.impute.me'>www.impute.me</a> and explore the current analysis-modules using this log-in ID: <b>",uniqueID,"</b><br><br>The service is non-profit, but the computing price for an imputation is approximately 5 USD per imputation. So if you have not done so already, please make a contribution to keep the servers running (<u><a href='",paypal,"'>paypal</a></u>).<br><br>If you have any further questions, please refer to the book <u><a href='https://www.worldscientific.com/worldscibooks/10.1142/11070'>'Understand your DNA'</a></u> that serves as a guide for the underlying concepts of this analysis.<br><br>For advanced users, it is also possible to download the <a href=",location_gen,">gen-format</a> and <a href=",location_json,">json-format</a> files. These contain probabilistic information on genotype calls and calculated phenotype information, respectively.<br></HTML>",sep="")
    for(tryCount in 1:3){
      print(paste("Trying to mail to",email))
      mailingResult<-try(send.mail(from = email_address,
                                   to = email,
                                   subject = "Imputation is ready",
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
      Sys.sleep(10)
      if(class(mailingResult)!="try-error")break
      if(tryCount == 3)stop("MAILING FAILED. THIS SHOULD BE FOLLOWED UP")
    }
  }
  
  

  #also clear the hub imputation_folder if running as node
  if(serverRole== "Node"){
    cmd9 <- paste("ssh ubuntu@",hubAddress," 'rm -r /home/ubuntu/imputations/imputation_folder_",uniqueID,"'",sep="")
    system(cmd9)
    
    #also don't leave the finished data here
    unlink(paste("/home/ubuntu/data/",uniqueID,sep=""),recursive=TRUE)
    
  }
  
  
}

