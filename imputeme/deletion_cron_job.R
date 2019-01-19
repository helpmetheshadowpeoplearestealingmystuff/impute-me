
# crontab -e
# 00 20 * * * Rscript /home/ubuntu/srv/impute-me/imputeme/deletion_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-delete-cron.log 2>&1

source("/home/ubuntu/srv/impute-me/functions.R")

uniqueIDs<-list.files("/home/ubuntu/data")



#in days - for traceable and non-traceable, respectively
keeping_time_1 <- 14
keeping_time_2 <- 2 * 365

for(uniqueID in uniqueIDs){
  pData<-try(read.table(paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep=""),sep="\t",header=T,stringsAsFactors=F))
  if(class(pData)=="try-error")next
  if(!all(c("protect_from_deletion","first_timeStamp")%in%colnames(pData)))next
  
  #DNA-traceable files - links
  f1<-paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".simple_format.zip",sep="")
  f2<-paste("/home/ubuntu/srv/impute-me/www/",uniqueID,".gen.zip",sep="")
  
  #DNA-traceable files - data
  f3<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".gen.zip",sep="")
  f4<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".simple_format.zip",sep="")
  f5<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".input_data.zip",sep="")
  
  #Non-traceable files - link
  f6<-paste("/home/ubuntu/srv/impute-me/www/",uniqueID,"_data.json",sep="")
  
  #non-traceable files - data
  f7<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json",sep="")
  f8<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.all_gwas.gz",sep="")
  f9<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.gz",sep="")
  f10<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.nonsenser.gz",sep="")
  f11<-paste("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.ethnicity.gz",sep="")
  
  #time assignment
  start<-strptime(pData[1,"first_timeStamp"],"%Y-%m-%d-%H-%M")
  timedif<-difftime(Sys.time(),start, units="days")
  
  
  
  
  #Handling day 14 deletions
  if(timedif > keeping_time_1 & any(file.exists(c(f1,f2,f3,f4,f5,f6)))){

    #modify pdata to not save personally identifiable information    
    pData[,"email"]<-NULL
    pData[,"filename"]<-NULL
    pdata_path<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
    write.table(pdata,file=pdata_path,sep="\t",col.names=T,row.names=F,quote=F)
    
    
    #modify json to not save personally identifiable information
    json_path <- paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json")
    d0<-fromJSON(json_path)
    d0[["original_filename"]] <- NULL
    d0[["original_submission_email"]] <- NULL
    d1<-toJSON(d0)
    f<-file(json_path,"w")
    writeLines(d1,f)
    close(f)
    
    
        
    #always delete download links at over-time (too easy to hack otherwise)
    if("link" %in% routinely_delete_this){
      if(file.exists(f1))unlink(f1)
      if(file.exists(f2))unlink(f2)
      if(file.exists(f6))unlink(f6)
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
      
      
    }else{ #i.e. user is in protected from deletion (but we still delete the most bulky files)
      print(paste("NOT deleting files from ",uniqueID,"even though it is",round(timedif),"days old. It is protect from deletion"))	
      # if(file.exists(f4))unlink(f4) #optinally delete easy-impute file
    }
    
  }
  
  
  #handle year 2 deletions
  if(timedif > keeping_time_2 & any(file.exists(c(f7,f8,f9,f10,f11)))){
    
    #then check if user is protected from deletion or not   
    if(!pData[1,"protect_from_deletion"]){
      print(paste("Files from ",uniqueID,"are now",round(timedif),"days old, and are removed due to two-year-limit"))
      
      try(unlink(f7))
      try(unlink(f8))
      try(unlink(f9))
      try(unlink(f10))
      try(unlink(f11))

      
    }else{
      print(paste("Files from ",uniqueID,"are now",round(timedif),"days old, but are kept beyond two-year-limit because of protect-deletion status"))      
    }
    
    
  }
  
  
}






#also delete the usage reports, just to clean up a little
unlink(list.files("/home/ubuntu/srv/impute-me/www/",pattern="_report\\.pdf$",full.names=T))




#also delete the summary reports, just to clean up a little
age_limit <- 7
l<-data.frame(
  f=list.files("/home/ubuntu/srv/impute-me/www",pattern="^summary_",full.names=T),
  stringsAsFactors = F
)
l[,"time"]<-file.info(l[,"f"])[,"mtime"]
l[,"age"]<-difftime( Sys.time(),l[,"time"],units="days")
l[,"delete"] <- l[,"age"] > age_limit
unlink(l[l[,"delete"],"f"],recursive=T)






