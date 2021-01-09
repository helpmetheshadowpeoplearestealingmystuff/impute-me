# 
#Setup this to run every hour.  It will check for jobs to handle in the /home/ubuntu/imputations queueing area 
#and if present it will execute the relevant scripts, run_imputation, summarize_imputation, and also the 
#PRS-calculation scripts run_export_scripts
#
# Suggested setup for cronjob (edit with crontab -e)
# 50 * * * * Rscript /home/ubuntu/srv/impute-me/imputeme/imputation_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-impute-cron.log 2>&1


source("/home/ubuntu/srv/impute-me/functions.R")


#check if anything is ready for single run imputation
uniqueID<-check_for_cron_ready_jobs("single")


#run the imputation
run_imputation(uniqueID=uniqueID)

#summarizing files
summarize_imputation(uniqueID=uniqueID,runDir=paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID))

#Run the genotype extraction routine
crawl_for_snps_to_analyze(uniqueIDs=uniqueID)


#Run the json extraction routine
run_export_script(uniqueIDs=uniqueID)


#final transfer of files
transfer_cleanup_and_mailout(uniqueID=uniqueID)





