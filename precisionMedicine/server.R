library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")




# Define server logic for random distribution application
shinyServer(function(input, output) {
	
  
  
  get_data <- reactive({
    
    #initial UI data gathering and user-check
    uniqueID<-gsub(" ","",input$uniqueID)
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError(paste("Did not find a user with this id",uniqueID)))
    }
    
    table_file <-"/home/ubuntu/srv/impute-me/precisionMedicine/SNPs_to_analyze.txt"
    SNPs_to_retrieve<-SNPs_to_analyze<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
    
    #retrieving SNPs
    SNPs_to_retrieve<-SNPs_to_retrieve[!duplicated(SNPs_to_retrieve[,"SNP"]),]
    rownames(SNPs_to_retrieve) <- SNPs_to_retrieve[,"SNP"]
    SNPs_to_retrieve<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_retrieve)

    
    #inserting SNPs and calculating GRS
    SNPs_to_analyze[,"Your genotype"] <- SNPs_to_retrieve[rownames(SNPs_to_analyze),"genotype"]


    
    #write the query to the log file
    log_function<-function(uniqueID){
      user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"drug_response",uniqueID)
      m<-paste(m,collapse="\t")
      if(file.exists(user_log_file)){
        write(m,file=user_log_file,append=TRUE)
      }else{
        write(m,file=user_log_file,append=FALSE)
      }
    }
    try(log_function(uniqueID))
    return(SNPs_to_analyze)
  })
  
  
  
  
  # output$downloadData <- downloadHandler(
  #   filename = paste(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"data.xlsx",sep="_"),
  #   content = function(file){
  #     library(openxlsx)
  # 
  #     SNPs_to_analyze<-get_data()
  #       
  #       #summarising allele info into single-columns
  #       SNPs_to_analyze[,"Risk/non-risk Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
  #       SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
  #       
  #       #adding genotype GRS and rounding MAF
  #       # SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
  #       
  #       
  #       # keep<-c("SNP","genotype","Risk/non-risk Allele","personal_score","population_score_average","effect_size","Major/minor Allele","minor_allele_freq","Source_PMID","gene","Context","Comment")
  #       SNPs_to_analyze<-SNPs_to_analyze[,keep]
  #       colnames(SNPs_to_analyze)<-c("SNP","Your Genotype","Risk/ non-risk Allele","SNP-score","SNP-score (population normalized)","Effect Size","Major/ minor Allele","Minor Allele Frequency","Source PMID","Gene","Context","Comment")
  #     
  #     write.xlsx(x=SNPs_to_analyze,file=file,rowNames=F)
  #     
  #   }
  # )

  
#   output$table1 <- renderTable({ 
# 		if(input$goButton == 0){
# 			return(NULL)
# 		}
# 	  table<-get_data()
# 		table<-table[,c("SNP","genotype","effect_allele")]
# 		colnames(table)<-c("SNP","Your genotype","Risk allele")
# 		table<-table["rs2395029",,drop=FALSE]
# 		# rownames(table)<-NULL
# 		return(table)
# 		
# 	},include.rownames = FALSE)
# 	
# 	
# 	output$table2 <- renderTable({ 
# 		if(input$goButton == 0){
# 			return(NULL)
# 		}
# 	  SNPs_to_analyze<-get_data()
# 	  
# 	  SNPs_to_analyze<-SNPs_to_analyze[SNPs_to_analyze[,"Source_PMID"]%in%"28223407",]
# 	  
# 	  population_sum_sd<-sqrt(sum(SNPs_to_analyze[,"population_score_sd"]^2,na.rm=T))
# 	  GRS_beta <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T) / population_sum_sd
# 	  
# 	  
# 		Proportion<-signif(pnorm(GRS_beta,mean=0,sd=1),2)*100
# 		if(Proportion > 80){
# 		  type <- "High Genetic Risk"
# 		}else{
# 		  type<-"All Others"
# 		}
# 		
# 		
# 		out<-data.frame("Z_score"=GRS_beta,"Percent_score"=paste0(Proportion,"%"),"risk_level"=type)
# 		colnames(out)<-c("GRS Z-score","Percentile Score","'Score category'")
# 		return(out)
# 		
# 	},include.rownames = FALSE)
# 	
# 	
# 	
# 	
# 	
# 	
# 	output$table3 <- renderTable({ 
# 		if(input$goButton == 0){
# 			return(NULL)
# 		}
# 	  table<-get_data()
# 	  
# 		table<-table[,c("SNP","genotype","effect_allele")]
# 		colnames(table)<-c("SNP","Your genotype","Effect allele")
# 		table<-table["rs1799971",,drop=FALSE]
# 		# rownames(table)<-NULL
# 		return(table)
# 		
# 	},include.rownames = FALSE)
# 	
# 	
# 	
# 	
# 	
# 	
# 	output$table4 <- renderTable({ 
# 		if(input$goButton == 0){
# 			return(NULL)
# 		}
# 	  table<-get_data()
# 	  
# 		table<-table[,c("SNP","genotype","effect_allele")]
# 		colnames(table)<-c("SNP","Your genotype","Effect allele")
# 		table<-table[c("rs3745274","rs2279343"),,drop=FALSE]
# 	
# 		
# 		
# 	
# 		
# 		
# 		
# 		return(table)
# 		
# 	},include.rownames = FALSE)
# 	
	
	
	
	
	
	output$table5 <- renderTable({ 
	  if(input$goButton == 0){
	    return(NULL)
	  }
	  table<-get_data()
	  # table[,"Ethnicity"]<-table[,"Disease"]<-table[,"Drug"]<-NULL
	  table[,"Ethnicity"]<-table[,"Disease"]<-NULL
	  return(table)
	  
	},include.rownames = FALSE)
	
})