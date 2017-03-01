library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")




# Define server logic for random distribution application
shinyServer(function(input, output) {
	
  
  
  get_data <- reactive({
    
    #initial UI data gathering and user-check
    uniqueID<-input$uniqueID
    if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
    if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(paste("Did not find a user with this id",uniqueID))
    }
    table_file <-"/home/ubuntu/srv/impute-me/statins/SNPs_to_analyze.txt"
    SNPs_to_analyze<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
    rownames(SNPs_to_analyze)<-SNPs_to_analyze[,"SNP"]
    genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
    SNPs_to_analyze[,"GRS"]<-get_GRS_2(genotypes=genotypes, betas=SNPs_to_analyze)
    SNPs_to_analyze[,"Your genotype"]<-genotypes[rownames(SNPs_to_analyze),]
    
    
    keep<-c("SNP","locus","Your genotype","effect_allele","non_effect_allele","GRS","Beta","minor_allele","major_allele","minor_allele_freq","Source_PMID","Direction","gene","Context","Comment")
    SNPs_to_analyze<-SNPs_to_analyze[,keep]

    
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
  
  
  
  
  output$downloadData <- downloadHandler(
    filename = paste(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"data.xlsx",sep="_"),
    content = function(file){
      library(openxlsx)

      SNPs_to_analyze<-get_data()
      
      #summarising allele info into single-columns
      SNPs_to_analyze[,"Risk/non-risk Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
      SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
      
      #adding genotype GRS and rounding MAF
      SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
      SNPs_to_analyze[,"GRS"] <- signif(SNPs_to_analyze[,"GRS"], 2)
      
      
      
      keep<-c("SNP","Your genotype","Risk/non-risk Allele","GRS","Beta","Major/minor Allele","minor_allele_freq","Source_PMID","gene","Context","Comment")
      SNPs_to_analyze<-SNPs_to_analyze[,keep]
      colnames(SNPs_to_analyze)<-c("SNP","Your Genotype","Risk/ non-risk Allele","Your GRS (this SNP)","Effect Size","Major/ minor Allele","Minor Allele Frequency","Source PMID","Gene","Context","Comment")
      
      write.xlsx(x=SNPs_to_analyze,file=file,rowNames=F)
      
    }
  )

  
  output$table1 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  table<-get_data()
		table<-table[,c("SNP","Your genotype","effect_allele")]
		colnames(table)<-c("SNP","Your genotype","Risk allele")
		table<-table["rs2395029",,drop=FALSE]
		# rownames(table)<-NULL
		return(table)
		
	},include.rownames = FALSE)
	
	
	output$table2 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  SNPs_to_analyze<-get_data()
	  
		
		
		GRS_beta<-mean(SNPs_to_analyze[,"GRS"],na.rm=T)

		Proportion<-signif(pnorm(GRS_beta,mean=0,sd=1),2)*100
		high_level<- Proportion > 50
		
		
		out<-data.frame("Z-score"=GRS_beta,"Percent-score"=paste0(Proportion,"%"),"High level?"=high_level)
		return(out)
		
	},include.rownames = FALSE)
	
	
	
	
	
	
	output$table3 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  table<-get_data()
	  
		table<-table[,c("SNP","Your genotype","effect_allele")]
		colnames(table)<-c("SNP","Your genotype","Effect allele")
		table<-table["rs1799971",,drop=FALSE]
		# rownames(table)<-NULL
		return(table)
		
	},include.rownames = FALSE)
	
	
	
	
	
	
	output$table4 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  table<-get_data()
	  
		table<-table[,c("SNP","Your genotype","effect_allele")]
		colnames(table)<-c("SNP","Your genotype","Effect allele")
		table<-table[c("rs3745274","rs2279343"),,drop=FALSE]
	
		
		
	
		
		
		
		return(table)
		
	},include.rownames = FALSE)
})