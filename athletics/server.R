library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")



# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  get_data <- reactive({
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      print(paste("Ok",input$goButton))
    }
    uniqueID<-gsub(" ","",input$uniqueID)
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError("Did not find a user with this id"))
    }      
    
    table_file <-"/home/ubuntu/srv/impute-me/athletics/SNPs_to_analyze.txt"
    request<-table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
  
    
    
    
    #get genotypes and calculate gheight
    request<-request[!duplicated(request[,"SNP"]),]
    rownames(request)<-request[,"SNP"]
    genotypes<-get_genotypes(uniqueID=uniqueID,request=request)
    table[,"genotype"]<-genotypes[table[,"SNP"],]
    
    #silly check
    if("Beta" %in% colnames(table)){
      colnames(table)[colnames(table)%in%"Beta"]<-"effect_size"
      
    }
    
    
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
    
  })
  
  output$table1 <- renderDataTable({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      table<-get_data()
    
      #subset to relevant data    
      table<-table[table[,"Domain"]%in%"Table1",]
      table<-table[,c("SNP","genotype","Comment")]
      colnames(table)<-c("SNP","Your genotype","Description")
      
      return(table)
    }
  })
  
  output$text1 <- renderText({ 
    if(input$goButton == 0){
      return("")
    }else if(input$goButton > 0) {
      message <- "<br><br>These SNPs are the most well-known atheletics SNPs. They all have fairly well-supported studies behind them, albeit limited effect sizes<br>"
      
      return(message)
    }
  })
  
  
  
  output$table2 <- renderDataTable({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      table<-get_data()
      
      source_notes <- input$source_notes
      
      domains <- data.frame(row.names=c('ACL rupture','Achilles tendon','Stress fracture','Osteoarthritis','Iron Biomarker','Vitamin E','Vitamin D','Magnesium','Vitamin B','Homocysteine','Phytosterols','Bone mineral density'))
      
      
      for(i in 1:nrow(domains)){
        d<-table[table[,"Domain"]%in%rownames(domains)[i],]
        rownames(d) <- d[,"SNP"]
        
        #try to see if it is numeric-ok
        ef <- suppressWarnings(as.numeric(d[,"effect_size"]))
        if(sum(is.na(ef))==0){
          d[,"effect_size"] <- ef
        }else{ #else just insert 1 because it is "effect allele increase risk"
          d[,"effect_size"] <- 1
        }
        
        d<-get_GRS_2(d, mean_scale=T, unit_variance=T, verbose=T)
        population_sum_sd<-sqrt(sum(d[,"population_score_sd"]^2,na.rm=T))
        GRS_beta <-sum(d[,"score_diff"],na.rm=T) / population_sum_sd
        domains[i,"Number of SNPs"] <- sum(!is.na(d[,"score_diff"]))
        domains[i,"Level-score"] <- paste(signif(GRS_beta*100,3),"%")
        
        if(source_notes){
          
          d[,"snps_line"] <- rownames(d)
          d[,"duplicated"]<-duplicated(d[,"Comment"])
          duplicated_snps<-unique(rownames(d)[d[,"duplicated"]])
          for(duplicated_snp in duplicated_snps){
            w<-which(d[,"Comment"]  %in% d[duplicated_snp,"Comment"])
            d[!d[w,"duplicated"],"snps_line"]<-paste(rownames(d)[w],collapse=", ")
          }
          d<-d[!d[,"duplicated"],]
          domains[i,"Source notes"]<-paste(paste0(d[,"snps_line"],": ",d[,"Comment"]),collapse="; ")
        }

      }
      
      
      return(domains)
    }
  })
  
  output$text2 <- renderText({ 
    if(input$goButton == 0){
      return("")
    }else if(input$goButton > 0) {
      message <- "<br><br>This table calculates genetic risk scores for all domains covered in <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/25919592'>Goodlin et al</a></u>, which covers a number of often encountered injuries and dietary needs in athletics. The risk score is indicated as percentile, i.e. 'percent of population with a lower score'. So it <i>should not</i> be translated as the direkt risk propensity, just how much more genetic effect there is for any of these domains.<br><br>"
      
      return(message)
    }
  })
  
  
  
  
  
})


