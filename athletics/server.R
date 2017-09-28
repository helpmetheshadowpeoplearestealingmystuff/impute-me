library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")


# uniqueID<-"id_57n662948"

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  get_data <- reactive({
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      print(paste("Ok",input$goButton))
    }
    uniqueID<-isolate(gsub(" ","",input$uniqueID))
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError("Did not find a user with this id"))
      
      
      table_file <-"/home/ubuntu/srv/impute-me/athletics/SNPs_to_analyze.txt"
      table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
      table<-table[table[,"Domain"]%in%"Table1",]
      rownames(table)<-table[,"SNP"]
      
      #get genotypes and calculate gheight
      genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
      table[,"Your genotype"]<-genotypes[rownames(table),]
      
      #subset to relevant data    
      table<-table[,c("SNP","Your genotype","Comment")]
      colnames(table)<-c("SNP","Your genotype","Description")
      
      
      #write the score to the log file
      log_function<-function(uniqueID){
        user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"athletics",uniqueID)
        m<-paste(m,collapse="\t")
        if(file.exists(user_log_file)){
          write(m,file=user_log_file,append=TRUE)
        }else{
          write(m,file=user_log_file,append=FALSE)
        }
      }
      try(log_function(uniqueID))
      return(table)
    }
  })
  
  output$table1 <- renderDataTable({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      table1<-get_data()
      
      return(table1)
    }
  })
  
  output$text1 <- renderDataTable({ 
    if(input$goButton == 0){
      return("")
    }else if(input$goButton > 0) {
      message <- "These SNPs are the most well-known atheletics SNPs. They all have fairly well-supported studies behind them, albeit limited effect sizes"
      
      return(message)
    }
  })
  
})


