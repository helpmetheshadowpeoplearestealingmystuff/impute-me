library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")


shinyServer(function(input, output) {
	
	get_table_here <- reactive({
		# 				if(input$goButton == 0){
		# 					return(NULL)
		# 				}
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError("Did not find a user with this id"))
		}
		table_file <-"/home/ubuntu/srv/impute-me/rareDiseases/SNPs_to_analyze.txt"
		request <- table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
    
		request<-request[!duplicated(request[,"SNP"]),]
		rownames(request) <- request[,"SNP"]
		genotypes<-get_genotypes(uniqueID=uniqueID,request=request )
		
		iXXXX_na_count <- sum(is.na(genotypes[grep("^i",rownames(genotypes)),1]))
		if(iXXXX_na_count > 10){ #then this is probably not a 23andme array
		  
		  #remove the iXXXX
		  table<-table[grep("^i",table[,"SNP"],invert=T),]
		  table<-table[order(table[,"disease_name"]),]
		  
		  #more intelligible comment
		  table[grep("^original",table[,"comment"]),"comment"] <-"rs-id from original 23andme"
		  
		  #adding genotypes in (many will be missing unfortuntaly)
		  table[,"Your genotype"]<-genotypes[table[,"SNP"],]
		  
		}else{ #then it is a 23andme array
		  #remove the stand-in stuff
		  table<-table[grep("^stand-in",table[,"comment"],invert=T),]
		  table<-table[order(table[,"disease_name"]),]
		  
		  
		  #more intelligible comment
		  table[,"comment"] <-""
		  
		  
		  #adding genotypes in (many will be missing unfortuntaly)
		  table[,"Your genotype"]<-genotypes[table[,"SNP"],]
		  
		}
		
		
		table[,"First_allele"]<-substr(table[,"Your genotype"],1,1)
		table[,"Second_allele"]<-substr(table[,"Your genotype"],3,3)
		
		table[,"First_carrier"]<-table[,"First_allele"]==table[,"risk_allele"]
		table[,"Second_carrier"]<-table[,"Second_allele"]==table[,"risk_allele"]
		return(table)
		
	})
	
	output$table1 <- renderDataTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		table<-get_table_here()
		
		# diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
		
		table<-table[,c("SNP","Your genotype","risk_allele","non_risk_allele","disease_name","comment")]
		colnames(table)<-c("SNP","Your genotype","Risk-allele","Non-Risk-allele","Inherited Condition","Comment")
		return(table)
		
		
	},options =list(pageLength = 200,searching = FALSE))
	
	output$text_advice1 <- renderText({ 
		
		if(input$goButton == 0){
			return(NULL)
		}
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		table<-get_table_here()
		
		diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
		diseases_of_interest<-diseases_of_interest[!is.na(diseases_of_interest)]
		
		if(length(diseases_of_interest)==0){
			m <- "There's no particular inherited conditions that you should pay attention to, according to this analysis"	
		}else if(length(diseases_of_interest)==1){
			m <- paste("According to this analysis, you should pay particular attention to the inherited condition:",diseases_of_interest)
		}else{
			m <- paste("According to this analysis, you should pay particular attention to these",length(diseases_of_interest),"inherited conditions:",paste(diseases_of_interest,collapse=", "))	
		}
		m <- paste(m,".<br>",sep="")

		
		#write the query to the log file
		log_function<-function(uniqueID,diseases_of_interest){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"rareDiseases",uniqueID,paste(diseases_of_interest,collapse=";"))
			m<-paste(m,collapse="\t")
			if(file.exists(user_log_file)){
				write(m,file=user_log_file,append=TRUE)
			}else{
				write(m,file=user_log_file,append=FALSE)
			}
		}
		try(log_function(uniqueID,diseases_of_interest))
		
				
		return(m)
	})
	
	
	
	
})





