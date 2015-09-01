
# 
#Strategy - setup this to run every hour on the hour, 
# 	
# sudo crontab -u root -e
# 50 * * * * Rscript /srv/shiny-server/gene-surfer/imputeme/cron_job.R > /var/log/cron_log

source("/srv/shiny-server/gene-surfer/imputeme/functions.R")



library("mailR")
library("rJava")
s<-list.files("/home/ubuntu/imputations/")
foldersToCheck<-grep("^imputation_folder",s,value=T)


for(folderToCheck in foldersToCheck){
	setwd(paste("/home/ubuntu/imputations/",folderToCheck,sep=""))	
	
	if(!file.exists("job_status.txt")){
		print(paste("Didn't find a job-status file - should probably auto-delete",folderToCheck))
		next
	}
	
	jobStatus<-read.table("job_status.txt",stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
	
	
	if(jobStatus=="Job is not ready yet"){
		print(paste("Found job-status file - but job is not ready yet",folderToCheck))
		next
	}
	
	if(jobStatus=="Job is running"){
		print(paste("Found job-status file - but job is already running",folderToCheck))
		next
	}
	
	
	if(jobStatus=="Job is ready"){
		print(paste("Found job-status file and job is ready",folderToCheck)		)
		
		unlink("job_status.txt")
		write.table("Job is running",file="job_status.txt",col.names=F,row.names=F,quote=F)
		
		load("variables.rdata")		
		runDir<-paste("/home/ubuntu/imputations/",paste("imputation_folder",uniqueID,sep="_"),sep="")
		
		run_imputation(
			rawdata=paste(uniqueID,"_raw_data.txt",sep=""), 
			runDir=runDir
		)
		
		#summarizing files
		zipFilesOut<-summarize_imputation(runDir=runDir,uniqueID=uniqueID,destinationDir="/srv/shiny-server")
		
		
		
		print("Getting IP and sending mail")
		ip<-sub("\"}$","",sub("^.+\"ip\":\"","",readLines("http://api.hostip.info/get_json.php", warn=F)))
		location_23andme <- paste(ip,sub("/srv/shiny-server/","",zipFilesOut["23andme"]),sep="/")
		location_gen <- paste(ip,sub("/srv/shiny-server/","",zipFilesOut["23andme"]),sep="/")
		
		message <- paste("We have completed imputation of your genome. For the next 24 hours you can retrieve your imputed genome at this address:\n",
										 location_23andme,
										 "\n\nFor advanced users, it is also possible to download the gen-format files from this location:\n",
										 location_gen)
		
		
		
		mailingResult<-try(stop(),silent=TRUE)
		while(class(mailingResult) == "try-error"){
			print(paste("Trying to mail to",email))
			mailingResult<-try(send.mail(from = "analyzer6063@gmail.com",
																	 to = email,
																	 subject = "Imputation is ready",
																	 body = message,
																	 smtp = list(
																	 	host.name = "smtp.gmail.com", 
																	 	port = 465, 
																	 	user.name = "analyzer6063@gmail.com", 
																	 	passwd = "ei1J#bQA^FA$", 
																	 	ssl = TRUE),
																	 authenticate = TRUE,
																	 send = TRUE))
			Sys.sleep(10)
			
		}
		
		
		
	}
	
	
	
}


