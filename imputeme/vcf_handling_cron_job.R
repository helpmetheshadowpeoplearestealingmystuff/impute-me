# 
#Setup this to run every hour.  It will check for jobs to handle in the /home/ubuntu/vcfs queueing area 
#and if present it will execute the relevant scripts, e.g. convert_vcfs_to_simple_format, and also the 
#PRS-calculation scripts run_export_scripts


#check if anything is ready for single run imputation
uniqueID<-check_for_cron_ready_jobs("vcf")

#run the coversion
convert_vcfs_to_simple_format(uniqueID)

#Run the genotype extraction routine
crawl_for_snps_to_analyze(uniqueIDs=uniqueID)

#Run the json extraction routine
run_export_script(uniqueIDs=uniqueID)

#final transfer of files
transfer_cleanup_and_mailout(uniqueID=uniqueID)
