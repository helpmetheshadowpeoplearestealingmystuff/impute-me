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
      
      pdf(file)
      kandinsky(genotypes)
      dev.off()
      
      
    }
  )
  
	
})





