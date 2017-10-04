
# 
#Strategy - setup this to run every hour on the hour, 
# 	
#Don't run it as root. THis is better
# crontab -u ubuntu -e
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
  jobStatusFile<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
  if(file.exists(jobStatusFile)){
    jobStatus<-read.table(jobStatusFile,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
    if(jobStatus=="Job is running"){
      runningJobCount<-runningJobCount+1
    }
    if(jobStatus=="Job is remote-running"){
      remoteRunningJobCount<-remoteRunningJobCount+1
    }
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
  remotedata_df<-remotedata_df[order(remotedata_df[,6]),]
  remoteFoldersToCheck<-remotedata_df[,7]
  
  
  #check if there's any fast-queue jobs to put up-front
  cmd0 <- paste("ssh ubuntu@",hubAddress," cat /home/ubuntu/misc_files/fast_queue_emails.txt
",sep="")
  f<-system(cmd0,intern=T)
  remoteFoldersToCheck<-c(remoteFoldersToCheck[remoteFoldersToCheck%in%f],remoteFoldersToCheck[!remoteFoldersToCheck%in%f])
  
  
  #then loop over all remote folders - first checking if there is 'length_requested'  ready jobs
  remoteFoldersToRun <- vector()
  for(remoteFolderToCheck in remoteFoldersToCheck){
    if(length(remoteFoldersToRun)>=length_requested)break
    cmd2 <- paste("ssh ubuntu@",hubAddress," cat /home/ubuntu/imputations/",remoteFolderToCheck,"/job_status.txt",sep="")
    jobStatus<-system(cmd2,intern=T)
    #Check if the job is ready
    if(jobStatus=="Job is ready"){
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
  jobStatus<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
  if(jobStatus=="Job is not ready yet"){
    print(paste("Found job-status file - but job is not ready yet",folderToCheck))
    next
  }
  if(jobStatus=="Job is running"){
    print(paste("Found job-status file - but job is already running",folderToCheck))
    next
  }
  if(jobStatus=="Job is ready"){
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




#If script is still running, it means there was a job ready for imputation - 
uniqueIDs<-sub("^.+folder_","",imputeThisFolder)
runDir<-paste("/home/ubuntu/bulk_imputations/",format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"_bulk",sep="")
dir.create(runDir)
setwd(runDir)


#run the imputation
run_bulk_imputation(uniqueIDs, runDir)



unlink(runDir,recursive=TRUE)


for(uniqueID in uniqueIDs){
  print(paste("Looping over uniqueID",uniqueID,"for the summarization part"))
  
  #load variables specifically for this uniqueID
  summary_folder<-paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID)
  load(paste0(summary_folder,"/variables.rdata"))
  
    
  #summarizing files and remove the summary_folder
  summarize_imputation(runDir=summary_folder,uniqueID=uniqueID,destinationDir="/home/ubuntu/data")  
  unlink(summary_folder,recursive = T)
  
  #Run the genotype extraction routine
  try(crawl_for_snps_to_analyze(uniqueIDs=uniqueID))
  
  
  #Run the json extraction routine
  try(run_export_script(uniqueIDs=uniqueID))
  
  
  #If this is running as a node, we need to copy it back around here
  if(serverRole== "Node"){
    cmd5 <- paste("scp -r /home/ubuntu/data/",uniqueID," ubuntu@",hubAddress,":/home/ubuntu/data",sep="")
    system(cmd5)
  }
  
  
  

  #making a link out to where the data can be retrieved	(different on hub and node)
  if(serverRole== "Node"){
    cmd6 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,".23andme.zip /home/ubuntu/srv/impute-me/www/",uniqueID,".23andme.zip'",sep="")
    system(cmd6)
    
    cmd7 <- paste("ssh ubuntu@",hubAddress," 'ln -s /home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip /home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip'",sep="")
    system(cmd7)
    
    
  }else if(serverRole== "Hub"){
    file.symlink(
      from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".23andme.zip",sep=""),
      to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".23andme.zip",sep="")
    )
    file.symlink(
      from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip",sep=""),
      to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip",sep="")
    )
  }else{stop("very odd")}
  
  

  print("Getting IP and sending mail")
  ip<-"https://www.impute.me"
  location_23andme <- paste(ip,"/www/",uniqueID,".23andme.zip",sep="")
  location_gen <- paste(ip,"/www/",uniqueID,".gen.zip",sep="")
  
  
  message <- paste("<HTML>We have completed imputation of your genome. You can retrieve your imputed genome at this address:<br><a href=",location_23andme,">",location_23andme,"</a><br><br>You can also go to <a href='www.impute.me'>www.impute.me</a> and explore the current analysis-modules using this log-in ID: <b>",uniqueID,"</b><br><br>The service is non-profit, but the computing price for an imputation is 5 USD per imputation. So if you have not done so already, please make a contribution to keep the servers running (<u><a href='",paypal,"'>paypal</a></u> or <u><a href='https://www.coinbase.com/checkouts/25ff9f232d64626a2acb5e8af741ade3' target='_blank'>bitcoin</a></u>).<br><br>For advanced users, it is also possible to download the <a href=",location_gen,">gen-format files</a>. These contain probabilistic information on genotype calls and may help in getting a more clear picture of the reason for specific SNP calls.<br></HTML>",sep="")
  
  
  
  
  for(tryCount in 1:3){
    print(paste("Trying to mail to",email))
    mailingResult<-try(send.mail(from = email_address,
                                 to = "lassefolkersen@gmail.com",
                                 bcc="lassefolkersen@gmail.com",
                                 subject = "BULK-Imputation is ready",
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
  }
    
  
  

  #also clear the hub imputation_folder if running as node
  if(serverRole== "Node"){
    cmd8 <- paste("ssh ubuntu@",hubAddress," 'rm -r /home/ubuntu/imputations/imputation_folder_",uniqueID,"'",sep="")
    system(cmd8)
    
    #also don't leave the finished data here
    unlink(paste("/home/ubuntu/data/",uniqueID,sep=""),recursive=TRUE)
    
  }
  
  
}

