# 
#Setup this to run every hour.  It will check for jobs to handle in the /home/ubuntu/imputations queueing area 
#and if present it will execute the relevant scripts, run_imputation, summarize_imputation, and also the 
#PRS-calculation scripts run_export_scripts. 
#
#Alternatively, the contents of this script can be called directly from outside a docker container. E.g. like
# docker exec -it <dockerid> R -e "run_imputation(uniqueID=<uniqueID>)"
#The advantage is that this allows bypassing cron-job running completely, which may be smart in some cluster-computer
#setups.




#check if anything is ready for single run imputation
uniqueID<-check_for_cron_ready_jobs("single")

#run the imputation
run_imputation(uniqueID=uniqueID)

#summarizing files
summarize_imputation(uniqueID=uniqueID,runDir=paste0(get_conf("imputations_path"),"imputation_folder_",uniqueID))

#Run the genotype extraction routine
crawl_for_snps_to_analyze(uniqueIDs=uniqueID)

#Run the json extraction routine
run_export_script(uniqueIDs=uniqueID)

#final transfer of files
transfer_cleanup_and_mailout(uniqueID=uniqueID)





