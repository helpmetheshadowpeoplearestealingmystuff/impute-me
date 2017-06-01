
# sudo crontab -u shiny -e
# 00 20 * * * Rscript /home/ubuntu/srv/impute-me/imputeme/deletion_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-delete-cron.log 2>&1

source("/home/ubuntu/srv/impute-me/functions.R")

uniqueIDs<-list.files("/home/ubuntu/data")

#in days days
keeping_time<-14

for(uniqueID in uniqueIDs){
  pData<-try(read.table(paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep=""),sep="\t",header=T,stringsAsFactors=F))
  if(class(pData)=="try-error")next
  if(!all(c("protect_from_deletion","first_timeStamp")%in%colnames(pData)))next

  f1<-paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".23andme.zip",sep="")
  f2<-paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip",sep="")
  f3<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip",sep="")
  f4<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".23andme.zip",sep="")
  f5<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".input_data.zip",sep="")
  
  
  if(!any(file.exists(c(f1,f2,f3,f4,f5)))){
    print(paste("Skipping",uniqueID,"because all have already been deleted"))
    next
  }
  
  start<-strptime(pData[1,"first_timeStamp"],"%Y-%m-%d-%H-%M")
  timedif<-difftime(Sys.time(),start, units="days")
  if(timedif > keeping_time){
    
    #always delete download links at over-time (too easy to hack otherwise)
    if("link" %in% routinely_delete_this){
      f1<-paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".23andme.zip",sep="")
      f2<-paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip",sep="")
      if(file.exists(f1))unlink(f1)
      if(file.exists(f2))unlink(f2)
    }
    
    #then check if user is protected from deletion or not   
    if(!pData[1,"protect_from_deletion"]){
      print(paste("Deleting all",uniqueID,"because it is",round(timedif),"days old, and not protected from deletion"))	
      if("data" %in% routinely_delete_this){
        if(file.exists(f1))unlink(f1)
        if(file.exists(f2))unlink(f2)
        if(file.exists(f3))unlink(f3)
        if(file.exists(f4))unlink(f4)
        if(file.exists(f5))unlink(f5)
        #earlier version just deleted the entire folder, including submitter information and md5sums
        #but this resulted in too many repetitive re-submissions from users.
      }
      
      
    }else{ #i.e. user in protected from deletion (but we still delete the most bulky files)
      print(paste("Deleting only easy-impute file from ",uniqueID,"because it is",round(timedif),"days old, and protect from deletion"))	
      if(file.exists(f4))unlink(f4)
    }
    
  }
}






#also delete the usage reports, just to clean up a little
unlink(list.files("/home/ubuntu/srv/impute-me/www/",pattern="_report\\.pdf$",full.names=T))