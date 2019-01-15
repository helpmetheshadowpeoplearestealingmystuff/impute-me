library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")

shinyServer(function(input, output) {
  
  
  output$load1 <- renderPlot({ 
    par(bg = rgb(red=1, green=1, blue=1, alpha=0, maxColorValue = 1))
    
    processes<-list.files("/home/ubuntu/imputations/")
    totalProcesses <- length(processes)
    runningProcesses<-0
    remoteRunningProcesses<-0
    for(process in processes){
      status_file<-paste("/home/ubuntu/imputations/",process,"/job_status.txt",sep="")
      if(file.exists(status_file)){
        jobStatus<-read.table(status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
        if(jobStatus=="Job is running"){
          runningProcesses<-runningProcesses+1
        }
        if(jobStatus=="Job is remote-running"){
          remoteRunningProcesses<-remoteRunningProcesses+1
        }
        
      }
    }
    
    # 		remoteRunningProcesses<-2
    # 		runningProcesses<-1
    # 		totalProcesses<-6
    currentLoad<-matrix(c(runningProcesses,remoteRunningProcesses,totalProcesses-remoteRunningProcesses-runningProcesses),ncol=1)
    barplot(currentLoad,xlim=c(0,maxImputationsInQueue+4),ylim=c(0,2),main="",horiz=T,xaxt="n",col=c("grey20","grey40","grey70"))
    axis(side=1,at=seq(1,maxImputationsInQueue,2),labels=seq(1,maxImputationsInQueue,2))
    title(xlab="imputations")
    abline(v=maxImputationsInQueue,lty=2)
    text(maxImputationsInQueue+0.02,1.3,adj=0,label="Max queue")
    legend("topright",pch=15,col=c("grey20","grey40","grey70"),legend=c("Running","Remote-running","Queued"))
    
  })
  
  output$text1 <- renderText({ 
    if(input$goButton > 0){
      library("mailR")
      
      #start progress tracker			
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = 'Generating report', value = 0)
      updateProgress <- function(value = NULL, detail = NULL, max=NULL) {
        if(is.null(max))max <- 50
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + 1/max
        }
        progress$set(value = value, detail = detail)
      }
      
      
      
      relative_webpath<-generate_report(updateProgress = updateProgress)
      link<-paste0("http://www.impute.me/",relative_webpath)
      message <- paste("<HTML>The report can be downloaded from <u><a href='",link,"'>this link</a></u></HTML> ",sep="")
      send.mail(from = email_address,
                to = error_report_mail,
                subject = "Status report",
                body = message,
                html=T,
                smtp = list(
                  host.name = "smtp.gmail.com", 
                  port = 465, 
                  user.name = email_address, 
                  passwd = email_password, 
                  ssl = TRUE),
                authenticate = TRUE,
                send = TRUE)
      return("Status report have been mailed")
    }
  })
  
  
  output$text2 <- renderText({
    if(input$insertFastEmail > 1){
      return("only submit one job")
    }
    if(input$insertFastEmail == 1){
      fast_queue_path <- "/home/ubuntu/misc_files/fast_queue_emails.txt"
      imputation_path <- "/home/ubuntu/imputations/"
      
      
      if(!file.exists(fast_queue_path)){
        system(paste0("touch ",fast_queue_path))
      }
    
      #split and check emails
      inputs<-input$email
      inputs<-strsplit(inputs,",")[[1]]
      inputs <- gsub(" +$","",gsub("^ +","",inputs))
      for(i in 1:length(inputs)){
        e <- inputs[i]
        not_email <- e == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(e)) != ""
        not_unique_id <- nchar(e)!=12 | length(grep("^[Ii][Dd]_",e))==0
        if(not_email &  not_unique_id){
          stop(safeError(paste("a real email adress or uniqueID is needed:",e)))
        }
        if(not_unique_id){
          inputs[i] <- tolower(e)
        }else{
          inputs[i]<-sub("^[Ii][Dd]","id",e)
        }
      }        
      
      
      #get folder ids corresponding to these inputs
      u <- vector() #counter for insertations
      for(folderToCheck in list.files(imputation_path,full.names = T)){
        f<-paste0(folderToCheck,"/variables.rdata")
        if(!file.exists(f))  next
        load(f)
        if(email %in% inputs | uniqueID %in% inputs){
          b<-paste(basename(folderToCheck),FALSE) #FALSE, because bulk_imputations are not allowed to load these
          write(b,file=fast_queue_path,append=TRUE)			
          names(b) <- email
          u<-c(u,b) #update counter for insertations
        }
      }
      
      
      #then load the entire file and remove duplicates plus the ones that are not found in imputation_path anymore
      y<-try(read.table(fast_queue_path,stringsAsFactors = F))
      if(class(y)=="try-error"){
        y<-data.frame(V1=vector(),V2=vector())
      }else{
        y<-y[!duplicated(y[,"V1"]),,drop=FALSE]
      }
      y<-y[y[,"V1"]%in%list.files(imputation_path),,drop=FALSE]
      write.table(y, file=fast_queue_path,sep="\t",quote=F,row.names=F,col.names=F)
      
      #then return a status
      return(paste("From",length(inputs),"emails or unique-IDs received,",length(u),"emails/uniqueIDs have been put in the fast queue:",paste(paste0(names(u)," (",sub("imputation_folder_","",u),")"),collapse=", ")))
    }
  })
  
  
  
})


