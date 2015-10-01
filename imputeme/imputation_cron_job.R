
# 
#Strategy - setup this to run every hour on the hour, 
# 	
#Don't run it as root. THis is better
# sudo crontab -u shiny -e
# 50 * * * * Rscript /srv/shiny-server/gene-surfer/imputeme/imputation_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-impute-cron.log 2>&1




source("/srv/shiny-server/gene-surfer/functions.R")
library("mailR")
library("rJava")
library("tools")
s<-list.files("/home/ubuntu/imputations/")
foldersToCheck<-grep("^imputation_folder",s,value=T)


runningJobCount<-0
for(folderToCheck in foldersToCheck){
	jobStatusFile<-paste("/home/ubuntu/imputations/",folderToCheck,"/job_status.txt",sep="")
	if(file.exists(jobStatusFile)){
		jobStatus<-read.table(jobStatusFile,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
		if(jobStatus=="Job is running"){
			runningJobCount<-runningJobCount+1
		}
	}else{
		print(paste("Didn't find job_status.txt in",	folderToCheck))
	}
}


if(runningJobCount>(maxImputations-1)){
	print(paste("Found",runningJobCount,"running jobs, and max is",maxImputations,"so doing nothing"))
}else{
	
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
			zipFilesOut<-summarize_imputation(runDir=runDir,uniqueID=uniqueID,destinationDir="/home/ubuntu/data")
			
			
			
			#creating the pData file
			timeStamp<-format(Sys.time(),"%Y-%m-%d-%H-%M")
			md5sum <- md5sum(paste(uniqueID,"_raw_data.txt",sep=""))
			gender<-system(paste("cut --delimiter=' ' -f 5 ",runDir,"/step_1.ped",sep=""),intern=T)
			f<-file(paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep=""),"w")
			writeLines(paste(c("uniqueID","filename","email","first_timeStamp","md5sum","gender"),collapse="\t"),f)
			writeLines(paste(c(uniqueID,filename,email,timeStamp,md5sum,gender),collapse="\t"),f)
			close(f)
			
			
			#Run the genotype extraction routine
			crawl_for_snps_to_analyze(uniqueIDs=uniqueID)
			
			
			#making a link out to where the data can be retrieved		
			file.symlink(
				from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".23andme.zip",sep=""),
				to=paste("/srv/shiny-server/gene-surfer/www/",uniqueID,".23andme.zip",sep="")
			)
			file.symlink(
				from=paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip",sep=""),
				to=paste("/srv/shiny-server/gene-surfer/www/",uniqueID,".gen.zip",sep="")
			)
			
			print("Getting IP and sending mail")
			ip<-sub("\"}$","",sub("^.+\"ip\":\"","",readLines("http://api.hostip.info/get_json.php", warn=F)))
			location_23andme <- paste(ip,"/www/",uniqueID,".23andme.zip",sep="")
			location_gen <- paste(ip,"/www/",uniqueID,".gen.zip",sep="")
			
			
			message <- paste("<HTML>We have completed imputation of your genome. You can retrieve your imputed genome at this address:<br>",
											 location_23andme,
											 "<br><br>For advanced users, it is also possible to download the <a href=",location_gen,">gen-format files</a></HTML> ",sep="")
			
			
			
			
			mailingResult<-try(stop(),silent=TRUE)
			while(class(mailingResult) == "try-error"){
				print(paste("Trying to mail to",email))
				mailingResult<-try(send.mail(from = "analyzer6063@gmail.com",
																		 to = email,
																		 subject = "Imputation is ready",
																		 body = message,
																		 html=T,
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
			
			setwd("..")
			unlink(runDir,recursive=TRUE)
			
		}
	}
}

