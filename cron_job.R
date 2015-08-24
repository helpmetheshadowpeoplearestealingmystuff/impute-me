
# 
#Strategy - setup this to run every hour on the hour, 
# 	
# MAILTO=lassefolkersen@gmail.com
# 0 * * * * Rscript /srv/shiny-server/gene-surfer/cron_job.R > /var/log/cron_log







#set temp dir
library(mail)
s<-list.files("/home/ubuntu/imputations/")
foldersToCheck<-grep("^imputation_folder",s,value=T)


for(folderToCheck in foldersToCheck){
	setwd(paste("/home/ubuntu/imputations/",folderToCheck))	
	
	if(!file.exists("job_status.txt")){
		print(paste("Didn't find a job-status file - should probably auto-delete",folderToCheck))
		next
	}
	
	jobStatus<-read.table("job_status.txt",stringsAsFactors=FALSE,header=FALSE)
	if(jobStatus="Job is not ready yet"){
		print("Found job-status file - but job is not ready yet",folderToCheck)		
		next
	}
	

	
	
	if(jobStatus="Job is ready"){
		print("Found job-status file - and job is ready",folderToCheck)		
		load("imputation_commands.rdata")
		
		for(i in 1:length(cmd2)){
			print(paste("running cmd",i,"of",length(cmd2)))
			cmd_here<-cmd2[i]
			cmd_here_out<-system(cmd_here,intern=T)	
		}
		

		print("Zipping files")
		outputFiles<-grep("[1-9]\\.gen$",list.files(homeFolder,full.names=T))
		zipFileOut<-paste(homeFolder,paste(uniqueID,".zip",sep=""),sep="/")
		zip(zipFileOut, outputFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
		
		
		print("Moving zip files to download location and clean up")
		finalLocation <- paste("/srv/shiny-server/",zipFileOut,sep="")
		cmd3 <- system("sudo mv", zipFileOut, finalLocation)
		system(cmd3,intern=T)
		setwd("/home/ubuntu/imputations/")
		unlink(homeFolder,recursive = TRUE)
		
		print("Getting IP and sending mail")
		ip<-sub("\"}$","",sub("^.+\"ip\":\"","",readLines("http://api.hostip.info/get_json.php", warn=F)))
		location <- paste(ip,basename(fileOut),sep="/")
		message <- paste("For the next 24 hours you can retrieve your imputed genome at this address:\n",location)
		sendmail(recipient=to, subject=subject, message=message, password="rmail")
		
		print("Wait 24 hours")
		Sys.sleep(24*60*60)
		print("Delete output file")
		unlink(finalLocation)
		break
		
				
	}
	
		
	
}
