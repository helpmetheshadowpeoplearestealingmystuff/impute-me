
# crontab -e
# 00 20 * * * Rscript /home/ubuntu/srv/impute-me/imputemany/imputemany_check_cron_job.R > /home/ubuntu/logs/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-imputemany-check-cron.log 2>&1

library("mailR")
library("rJava")
library("tools")
source("/home/ubuntu/srv/impute-me/functions.R")




emails_to_send <- list()
imputemany_registry_path <-"/home/ubuntu/misc_files/imputemany_registry.txt"


#check if registry exists and create it if not
if(!file.exists(imputemany_registry_path)){
  f<-file(imputemany_registry_path,"w")
  writeLines(paste(c("upload_time","has_been_sent","error_sent","length","email","uniqueIDs"),collapse="\t"),f)
  close(f)
}

#upload time, has-been-sent, error_sent, length, email, uniqueIDs
d <- read.table(imputemany_registry_path,sep="\t",header=T,stringsAsFactors = F)
# Header are c("upload_time","has_been_sent","error_sent","length","email","uniqueIDs")
d[,"date"] <- as.POSIXct(d[,"upload_time"],tryFormats="%Y-%m-%d_%Hh-%Mm-%Ss")
d[,"age_days"] <- difftime(Sys.time(), d[,"date"], units = "days")
rownames(d) <- d[,"upload_time"]


#check the ones that are MORE than 14 days old - send error mails if not finished
day_split <- 14
d1 <- d[d[,"age_days"] > day_split,]
if(nrow(d1)>0){
  d2 <- d1[!d1[,"has_been_sent"] & !d1[,"error_sent"],]
  if(nrow(d2)>0){
    for(upload_time in rownames(d2)){
      text1 <- paste("This imputemany-study have not been finished and it's",round(d2[upload_time,"age_days"]),"days since it was submitted:",d2[upload_time,"upload_time"])  
      
      emails_to_send[[paste0("mail_",upload_time)]] <- list()
      emails_to_send[[paste0("mail_",upload_time)]][["to"]] <- error_report_mail
      emails_to_send[[paste0("mail_",upload_time)]][["subject"]] <- "Error in imputemany-pipeline"
      emails_to_send[[paste0("mail_",upload_time)]][["text"]] <- text1
      
      
      #change the error_sent flag to TRUE
      cmd1 <- paste0("sed -i 's/",upload_time,"\tFALSE\tFALSE/",upload_time,"\tFALSE\tTRUE/g' ",imputemany_registry_path)
      system(cmd1)
      Sys.sleep(0.1)
      
    }
  }
}
  
  

#check the ones that are LESS than 14 days old - send ok mails if they are finished
d3 <- d[d[,"age_days"] <= day_split & !d[,"has_been_sent"],]
if(nrow(d3)>0){
  for(upload_time in rownames(d3)){
    uniqueIDs <- strsplit(d3[upload_time,"uniqueIDs"],",")[[1]]
    if(all(uniqueIDs%in%list.files("/home/ubuntu/data"))){
      
      
      file_out_web <- summarize_imputemany_json(uniqueIDs,upload_time)
      
      text2 <- paste0("<HTML>The imputation and PRS pipeline has finished processing ",d3[upload_time,"length"]," samples from the upload with time-stamp: ",d3[upload_time,"upload_time"],
                      ". <br><br>The summarized data can be download at this address:<br>",
                      "<u><a href='",file_out_web,"'>",
                      file_out_web,"</a></u><br><br></HTML>")
      
      emails_to_send[[paste0("mail_",upload_time)]] <- list()
      emails_to_send[[paste0("mail_",upload_time)]][["to"]] <- d3[upload_time,"email"]
      emails_to_send[[paste0("mail_",upload_time)]][["subject"]] <- "Imputation and PRS pipeline finished"
      emails_to_send[[paste0("mail_",upload_time)]][["text"]] <- text2
      emails_to_send[[paste0("mail_",upload_time)]][["uniqueIDs"]] <- uniqueIDs
      
      
      #change the has_been_seent flag to TRUE
      cmd1 <- paste0("sed -i 's/",upload_time,"\tFALSE/",upload_time,"\tTRUE/g' ",imputemany_registry_path)
      system(cmd1)
      Sys.sleep(0.1)
    }
  }
}
  
  
  
  
if(length(emails_to_send)==0)stop("Didn't find any finished imputemany runs to report")


for(j in 1:length(emails_to_send)){
  print(paste("Send mail to",emails_to_send[[j]][["to"]],"with subject:",emails_to_send[[j]][["subject"]]))
  mailingResult<-try(send.mail(from = email_address,
                               to = emails_to_send[[j]][["to"]],
                               subject = emails_to_send[[j]][["subject"]],
                               body = emails_to_send[[j]][["text"]],
                               html=T,
                               smtp = list(
                                 host.name = "smtp.gmail.com",
                                 port = 465,
                                 user.name = email_address,
                                 passwd = email_password,
                                 ssl = TRUE),
                               authenticate = TRUE,
                               send = TRUE))
  
  Sys.sleep(0.5)
  
  
  
  #For some collaborations we need to check the accepted_emails list and see if 
  #an imputemany-run needs to be sent off in a special way (curl POST, separate-email, 
  #different normalization, etc). This will be done with a dedicated non-github 
  #function because some of the collaborators don't like their name or methods on github
  email <- emails_to_send[[j]][["to"]]
  uniqueIDs <- emails_to_send[[j]][["uniqueIDs"]]
  source("/home/ubuntu/misc_files/send_off_handler.R")
  special_send_off_handler(email, uniqueIDs)  
  
}



