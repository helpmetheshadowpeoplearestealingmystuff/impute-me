
# crontab -e
# 00 20 * * * Rscript /home/ubuntu/srv/impute-me/imputeme/space_check_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-space-check-cron.log 2>&1

library("gmailr")
library("tools")


df<-system("df",intern=T)

df1<-grep(paste0(get_conf("data_path"),"$"),df,value=T)
percentage1<-as.numeric(sub("^.+ ","",sub(paste0("% +",get_conf("data_path"),"$"),"",df1)))
if(length(percentage1)==0)percentage1  <- 0  
  
  
df2<-grep("/$",df,value=T)
percentage2<-as.numeric(sub("^.+ ","",sub("% /$","",df2)))



if(percentage1 > 90 | percentage2 > 90){
  print("should send warning mail")  
  if(percentage1==0){
    message <- paste0("The data-partition of a node server at impute.me is at ",percentage2,"% capacity")    
  }else{
    message <- paste0("The data-partition of the hub server at impute.me is at ",percentage1,"% and ",percentage2,"% capacity (data and main, respectively)")    
  }
  
  gm_auth_configure( path ="~/misc_files/mailchecker.json")
  gm_auth(email=get_conf("from_email_address"),cache="~/misc_files/mail_secret")
  prepared_email <- try(gm_mime() %>%
                          gm_to(get_conf("error_report_mail")) %>%
                          gm_from(get_conf("from_email_address")) %>%
                          gm_subject("Impute.me is almost full") %>%
                          gm_html_body(message))
  mailingResult<-try(gm_send_message(prepared_email))
  
	
  
}else{
  print(paste0("No problem with space: ",percentage2,"% filled."))
  
}