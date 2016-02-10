
# sudo crontab -u shiny -e
# 00 20 * * * Rscript /srv/shiny-server/gene-surfer/imputeme/deletion_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-delete-cron.log 2>&1

source("/srv/shiny-server/gene-surfer/functions.R")

folders<-list.files("/home/ubuntu/data")

#in days days
keeping_time<-14

for(folder in folders){
	pData<-try(read.table(paste("/home/ubuntu/data/",folder,"/pData.txt",sep=""),sep="\t",header=T,stringsAsFactors=F))
	if(class(pData)=="try-error")next
	if(!all(c("protect_from_deletion","first_timeStamp")%in%colnames(pData)))next
	if(!pData[1,"protect_from_deletion"]){
		# start<-Sys.time()
		start<-strptime(pData[1,"first_timeStamp"],"%Y-%m-%d-%H-%M")
		timedif<-difftime(Sys.time(),start, units="days")
		if(timedif > keeping_time){
			print(paste("Deleting",folder,"because it is",timedif,"days old"))	
			
			if("data" %in% routinely_delete_this){
				unlink(paste("/home/ubuntu/data/",folder,sep=""),recursive=T)
			}
			if("link" %in% routinely_delete_this){
				f1<-paste("/srv/shiny-server/gene-surfer/www/",folder,".gen.zip",sep="")
				f2<-paste("/srv/shiny-server/gene-surfer/www/",folder,".gen.zip",sep="")
				if(file.exists(f1))unlink(f1)
				if(file.exists(f2))unlink(f2)
			}
			
		}
	}
}