
# 
#Strategy - setup this to run every hour on the hour, 
# 	
# sudo crontab -u root -e
# 50 * * * * Rscript /srv/shiny-server/gene-surfer/cron_job.R > /var/log/cron_log




#set temp dir
library(mailR)
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

		load("imputation_commands.rdata")		
		
		impute2path <- "/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2"

		
		for(i in 1:length(cmd2)){
			print(paste("running cmd",i,"of",length(cmd2),"for",uniqueID))
			cmd_here<-cmd2[i]
			cmd_here<- sub("impute2", impute2path, cmd_here)
			cmd_here_out<-system(cmd_here,intern=T)	
		}
		

		print("Zipping files")
		outputFiles<-grep("[1-9]\\.gen$",list.files(),value=T)
		zipFileOut<-paste("/home/ubuntu/imputations",folderToCheck,paste(uniqueID,".zip",sep=""),sep="/")
		zip(zipFileOut, outputFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
		
		
		print("Moving zip files to download location and clean up")
		finalLocation <- paste("/srv/shiny-server/",basename(zipFileOut),sep="")
		cmd3 <- paste("sudo mv", zipFileOut, finalLocation)
		system(cmd3,intern=T)
		setwd("/home/ubuntu/imputations/")
		unlink(folderToCheck,recursive = TRUE)
		
		print("Getting IP and sending mail")
		ip<-sub("\"}$","",sub("^.+\"ip\":\"","",readLines("http://api.hostip.info/get_json.php", warn=F)))
		location <- paste(ip,basename(finalLocation),sep="/")
		message <- paste("For the next 24 hours you can retrieve your imputed genome at this address:\n",location)
		
		
		
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
		
		
		
		
		print("Wait 24 hours")
		Sys.sleep(24*60*60)
		print("Delete output file")
		unlink(finalLocation)
		break
		
				
	}
	
		
	
}


