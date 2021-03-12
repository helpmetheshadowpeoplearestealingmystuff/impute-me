# 
#Setup this to run every hour.  It will check for jobs to handle in the /home/ubuntu/imputations queueing area 
#and if present it will execute the relevant scripts, run_imputation, summarize_imputation, and also the 
#PRS-calculation scripts run_export_scripts
#
# Suggested setup for cronjob (edit with crontab -e)
# 50 * * * * Rscript /home/ubuntu/srv/impute-me/imputeme/BULK_imputation_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-impute-cron.log 2>&1

source("/home/ubuntu/srv/impute-me/functions.R")

#check if anything is ready for bulk run imputation
uniqueIDs<-check_for_cron_ready_jobs("bulk")

#prepare a runDir for the bulk running
if(!file.exists("/home/ubuntu/bulk_imputations/"))dir.create("/home/ubuntu/bulk_imputations/")
runDir<-paste("/home/ubuntu/bulk_imputations/",format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"_bulk",sep="")
dir.create(runDir)

#run the imputation (this takes quite a while)
run_bulk_imputation(uniqueIDs, runDir)  

#delete the rundir afterwards (because files are now in summary_folder)
unlink(runDir,recursive=TRUE)

#loop over all samples and perform the summarization step.
for(uniqueID in uniqueIDs){
  print(paste0(Sys.time(),": Looping over uniqueID ",uniqueID," for the summarization and scoring part"))
  
  #load variables specifically for this uniqueID
  summary_folder<-paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID)
    
  #summarizing files
  summarize_imputation(uniqueID=uniqueID,runDir=summary_folder)  

  #Run the genotype extraction routine
  crawl_for_snps_to_analyze(uniqueIDs=uniqueID)
  
  #Run the json extraction routine
  run_export_script(uniqueIDs=uniqueID)
  
  #final transfer of files
  transfer_cleanup_and_mailout(uniqueID=uniqueID)
  
}

