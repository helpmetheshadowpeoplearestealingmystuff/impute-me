# 
#Setup this to run every hour.  It will check for jobs to handle in the /home/ubuntu/vcfs queueing area 
#and if present it will execute the relevant scripts, e.g. convert_vcfs_to_simple_format, and also the 
#PRS-calculation scripts run_export_scripts
#
# Suggested setup for cronjob (edit with crontab -e)
# 50 * * * * Rscript /home/ubuntu/srv/impute-me/imputeme/vcf_handling_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-impute-cron.log 2>&1





library("mailR")
library("rJava")
library("tools")
source("/home/ubuntu/srv/impute-me/functions.R")



#Currently not tested for node-running. Only hub. Mainly because there has been no need, since the requirements are lower for a seq-handling
if(serverRole== "Node"){
  stop("This cron job has not been tested for node running at all, because it has been sufficient to run it as hub so far.")
}
  
#Script starts here in case the seconds_wait_before_start delay is not used.
foldersToCheck<-grep("^vcf_folder",list.files("/home/ubuntu/vcfs/"),value=T)
runningJobCount<-0
for(folderToCheck in foldersToCheck){
  job_status_file<-paste("/home/ubuntu/vcfs/",folderToCheck,"/job_status.txt",sep="")
  if(file.exists(job_status_file)){
    job_status<-read.table(job_status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
    if(job_status=="Job is running"){runningJobCount<-runningJobCount+1}
  }
}
if(runningJobCount>(maxImputations-1)){
  stop(paste("Found",runningJobCount,"running jobs, and max is",maxImputations,"so doing nothing"))
}






#Then - no matter the role - we check locally which, if any, folders are ready to run
imputeThisFolder<-NA
for(folderToCheck in foldersToCheck){
  job_status_file<-paste("/home/ubuntu/vcfs/",folderToCheck,"/job_status.txt",sep="")
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
    # unlink(job_status_file)
    # write.table("Job is running",file=job_status_file,col.names=F,row.names=F,quote=F)
    imputeThisFolder<-folderToCheck
    break
  }
}
#Stop if none are found
if(is.na(imputeThisFolder)){
  stop("No folders were found to be ready for imputation")
}




#If script is still running, it means there was a job ready for imputation - 
runDir<-paste("/home/ubuntu/vcfs/",imputeThisFolder,sep="")
setwd(runDir) 
load(paste(runDir,"/variables.rdata",sep="")) #get the parameters, including uniqueID as written in variables.rdata (but should be the same always)


#run the imputation
convert_vcfs_to_simple_format(uniqueID)

#Run the genotype extraction routine
try(crawl_for_snps_to_analyze(uniqueIDs=uniqueID))


#Run the json extraction routine
try(run_export_script(uniqueIDs=uniqueID))


#set up json download
file.symlink(
  from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json",sep=""),
  to=paste("/home/ubuntu/srv/impute-me/www/",uniqueID,"_data.json",sep="")
)




if(exists("imputemany_upload") && imputemany_upload){
  print(paste(Sys.time(),"skipping mail because it's from imputemany_upload"))
}else{
  print(paste(Sys.time(),"sending results mail"))
  ip<-"https://www.impute.me"
  location_json <- paste(ip,"/www/",uniqueID,"_data.json",sep="")
  #assign the right language book to people (Swedes and Norwegians can get the Danish language one too)
  if(length(grep("\\.dk$",email))==1 | length(grep("\\.no$",email))==1 | length(grep("\\.se$",email))==1){
    booklink<-"https://www.saxo.com/dk/forstaa-dit-dna_lasse-westergaard-folkersen_haeftet_9788770170154"
  }else{
    booklink<-"https://www.worldscientific.com/worldscibooks/10.1142/11070"
  }
  
  message <- paste("<HTML>We have completed processing the file <i>",filename,"</i>. You can now go to <a href='www.impute.me'>www.impute.me</a> and explore the analysis-modules using this ID:<br><br> <b>",uniqueID,"</b><br><br>The service is non-profit, but the computing price for an imputation is approximately 5 USD per imputation. So if you have not done so already, please make a contribution to keep the servers running (<u><a href='",paypal,"'>paypal</a></u>).<br><br>If you have any further questions, please refer to the book <u><a href='",booklink,"'>'Understand your DNA'</a></u> that serves as a guide for the underlying concepts of this analysis.<br><br>For advanced users, it is also possible to download analyzed data as <a href=",location_json,">json-format</a> files. These contain calculated phenotype information.<br></HTML>",sep="")
  if(email_address != "" & email_password != ""){
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
  }else{
    print(paste("Email is not configured - this is the intended message:",message))
  }
}

setwd("..")
unlink(runDir,recursive=TRUE)

