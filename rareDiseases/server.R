library("shiny")


source("/srv/shiny-server/gene-surfer/functions.R")


shinyServer(function(input, output) {
	
	get_table_here <- reactive({
		# 				if(input$goButton == 0){
		# 					return(NULL)
		# 				}
		uniqueID<-isolate(input$uniqueID)
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop("Did not find a user with this id")
		}
		table_file <-"/srv/shiny-server/gene-surfer/rareDiseases/SNPs_to_analyze.txt"
		table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
		
		#we have to remove the i3003137/Beta-Thalassemia because it's double with sickle-cell anemia
		table<-table[!(table[,"SNP"]%in%"i3003137" & table[,"disease_name"]%in%"Beta Thalassemia"),]
		
		
		rownames(table)<-table[,"SNP"]
		genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
		
		table[,"Your genotype"]<-genotypes[rownames(table),]
		
		table[,"First_allele"]<-substr(table[,"Your genotype"],1,1)
		table[,"Second_allele"]<-substr(table[,"Your genotype"],3,3)
		#Not necessary - actually it's quite good to have the male-X genotype of " " explicit, because it's certainly not a risk-genotype
		# table[table[,"First_allele"]==" ","First_allele"]<-NA
		# table[table[,"Second_allele"]==" ","Second_allele"]<-NA
		
		table[,"First_carrier"]<-table[,"First_allele"]==table[,"risk_allele"]
		table[,"Second_carrier"]<-table[,"Second_allele"]==table[,"risk_allele"]
		return(table)
		
	})
	
	output$table1 <- renderDataTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
		uniqueID<-isolate(input$uniqueID)
		table<-get_table_here()
		
		# diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
		
		table<-table[,c("SNP","Your genotype","risk_allele","non_risk_allele","disease_name")]
		colnames(table)<-c("SNP","Your genotype","Risk-allele","Non-Risk-allele","Inherited Condition")
		return(table)
		
		
	},options =list(pageLength = 200,searching = FALSE))
	
	output$text_advice1 <- renderText({ 
		
		if(input$goButton == 0){
			return(NULL)
		}
		uniqueID<-isolate(input$uniqueID)
		table<-get_table_here()
		
		diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
		
		
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



# 	output$table1 <- renderDataTable({ 
# 		if(input$goButton == 0){
# 			return(NULL)
# 		}
# 		uniqueID<-isolate(input$uniqueID)
# 		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
# 		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
# 		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
# 			Sys.sleep(3) #wait a little to prevent raw-force fishing	
# 			stop("Did not find a user with this id")
# 		}
# 		table_file <-"/srv/shiny-server/gene-surfer/rareDiseases/SNPs_to_analyze.txt"
# 		table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
# 		
# 		#we have to remove the i3003137/Beta-Thalassemia because it's double with sickle-cell anemia
# 		table<-table[!(table[,"SNP"]%in%"i3003137" & table[,"disease_name"]%in%"Beta Thalassemia"),]
# 		
# 		
# 		rownames(table)<-table[,"SNP"]
# 		genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
# 		
# 		table[,"Your genotype"]<-genotypes[rownames(table),]
# 		
# 		table[,"First_allele"]<-substr(table[,"Your genotype"],1,1)
# 		table[,"Second_allele"]<-substr(table[,"Your genotype"],3,3)
# 		#Not necessary - actually it's quite good to have the male-X genotype of " " explicit, because it's certainly not a risk-genotype
# 		# table[table[,"First_allele"]==" ","First_allele"]<-NA
# 		# table[table[,"Second_allele"]==" ","Second_allele"]<-NA
# 		
# 		
# 		table[,"First_carrier"]<-table[,"First_allele"]==table[,"risk_allele"]
# 		table[,"Second_carrier"]<-table[,"Second_allele"]==table[,"risk_allele"]
# 		
# 		
# 		diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
# 		
# 		
# 		table<-table[,c("SNP","Your genotype","alt_risk_allele","alt_non_risk_allele","disease_name")]
# 		colnames(table)<-c("SNP","Your genotype","Risk-allele","Non-Risk-allele","Inherited Condition")
# 		return(table)
# 	})
# 	


# 	output$text_advice1 <- renderText({ 
# 		if(input$goButton == 0){
# 			return("")
# 		}else if(input$goButton > 0) {
# 			uniqueID<-isolate(input$uniqueID)
# 			if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
# 			if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
# 			if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
# 				Sys.sleep(3) #wait a little to prevent raw-force fishing	
# 				stop("Did not find a user with this id")
# 			}
# 			table_file <-"/srv/shiny-server/gene-surfer/rareDiseases/SNPs_to_analyze.txt"
# 			table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
# 			
# 			#we have to remove the i3003137/Beta-Thalassemia because it's double with sickle-cell anemia
# 			table<-table[!(table[,"SNP"]%in%"i3003137" & table[,"disease_name"]%in%"Beta Thalassemia"),]
# 			
# 			rownames(table)<-table[,"SNP"]
# 			genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
# 			table[,"Your genotype"]<-genotypes[rownames(table),]
# 			
# 			table[,"First_allele"]<-substr(table[,"Your genotype"],1,1)
# 			table[,"Second_allele"]<-substr(table[,"Your genotype"],3,3)
# 			#Not necessary - actually it's quite good to have the male-X genotype of " " explicit, because it's certainly not a risk-genotype
# 			# table[table[,"First_allele"]==" ","First_allele"]<-NA
# 			# table[table[,"Second_allele"]==" ","Second_allele"]<-NA
# 			
# 			
# 			table[,"First_carrier"]<-table[,"First_allele"]==table[,"risk_allele"]
# 			table[,"Second_carrier"]<-table[,"Second_allele"]==table[,"risk_allele"]
# 			
# 			diseases_of_interest <- sort(unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"]))
# 			
# 			
# 			if(length(diseases_of_interest)==0){
# 				m <- "There's no particular inherited conditions that you should pay attention to, according to this analysis"	
# 			}else if(length(diseases_of_interest)==1){
# 				m <- paste("According to this analysis, you should pay particular attention to the inherited condition:",diseases_of_interest)
# 			}else{
# 				m <- paste("According to this analysis, you should pay particular attention to these",length(diseases_of_interest),"inherited conditions:",paste(diseases_of_interest,collapse=", "))	
# 			}
# 			m <- paste(m,".<br>",sep="")
# 			
# 		}
# 		return(m)
# 	})




