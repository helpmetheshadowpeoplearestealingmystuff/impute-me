
# crontab -e
# 00 20 * * * Rscript /home/ubuntu/srv/impute-me/imputeme/space_check_cron_job.R > /home/ubuntu/misc_files/cron_logs/`date +\%Y\%m\%d\%H\%M\%S`-space-check-cron.log 2>&1

library("mailR")
library("rJava")
library("tools")
source("/home/ubuntu/srv/impute-me/functions.R")


df1<-system("df",intern=T)
df2<-grep("/home/ubuntu/data$",df1,value=T)
percentage<-as.numeric(sub("^.+ ","",sub("% +/home/ubuntu/data$","",df2)))


if(percentage > 90){
  print("should send warning mail")  
  
  	mailingResult<-try(send.mail(from = email_address,
  															 to = "lassefolkersen@gmail.com",
  															 subject = "Impute.me is almost full",
  															 body = paste0("The data-partition of the hub server at impute.me is at ",percentage,"% capacity"),
  															 html=T,
  															 smtp = list(
  															 	host.name = "smtp.gmail.com",
  															 	port = 465,
  															 	user.name = email_address,
  															 	passwd = email_password,
  															 	ssl = TRUE),
  															 authenticate = TRUE,
  															 send = TRUE))

  
}