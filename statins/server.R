library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")


# uniqueID<-"id_57n662948"

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
    table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
    rownames(table)<-table[,"SNP"]
    genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
    table[,"Your genotype"]<-genotypes[rownames(table),]

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
    return(table)
  })
  

  
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
	  table<-get_data()
	  
		# table<-table[,c("SNP","Your genotype","effect_allele")]
		table<-table[table[,"Source_PMID"]%in%"28223407",]
		# table<-table["rs4363657",,drop=FALSE]
		# rownames(table)<-NULL
		return(table)
		
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