library("shiny")
library("kandinsky")

source("/home/ubuntu/srv/impute-me/functions.R")

dataFolder<-"/home/ubuntu/data/"



shinyServer(function(input, output) {

  output$plot_1 <- renderPlot({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      uniqueID<-isolate(gsub(" ","",input$uniqueID))
      if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
      if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
      if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
        Sys.sleep(3) #wait a little to prevent raw-force fishing	
        stop(safeError(paste("Did not find a user with this id",uniqueID)))
      }
      gwas_snps_path<-paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.all_gwas.gz")
      if(!file.exists(gwas_snps_path))stop(safeError("Didn't find a gwas cached file for this user"))
      genotypes<-read.table(gwas_snps_path,sep="\t",stringsAsFactors = F,header=T)
      if(nrow(genotypes)==0)stop(safeError("Found an empty gwas cached file for this user"))
      kandinsky(genotypes)
      
      
      
      #write the score to the log file
      log_function<-function(uniqueID,study_id,genotypes){
        user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"kandinsky",uniqueID)
        m<-paste(m,collapse="\t")
        if(file.exists(user_log_file)){
          write(m,file=user_log_file,append=TRUE)
        }else{
          write(m,file=user_log_file,append=FALSE)
        }
      }
      try(log_function(uniqueID,study_id,genotypes))
      
    }
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = paste(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"kandinsky.pdf",sep="_"),
    content = function(file){

      uniqueID<-isolate(gsub(" ","",input$uniqueID))
      if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
      if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
      if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
        Sys.sleep(3) #wait a little to prevent raw-force fishing	
        stop(safeError(paste("Did not find a user with this id",uniqueID)))
      }
      gwas_snps_path<-paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.all_gwas.gz")
      if(!file.exists(gwas_snps_path))stop(safeError("Didn't find a gwas cached file for this user"))
      genotypes<-read.table(gwas_snps_path,sep="\t",stringsAsFactors = F,header=T)
      if(nrow(genotypes)==0)stop(safeError("Found an empty gwas cached file for this user"))
      
      pdf(file,width=9,height=7)
      kandinsky(genotypes)
      dev.off()
      
      
    }
  )
  
	
})





