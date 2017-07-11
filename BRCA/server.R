library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")


# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	output$table1 <- renderDataTable({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError("Did not find a user with this id"))
		}
		
		
		#Get gender
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
		
		
		BRCA_table_file <-"/home/ubuntu/srv/impute-me/BRCA/SNPs_to_analyze.txt"
		BRCA_table<-read.table(BRCA_table_file,sep="\t",header=T,stringsAsFactors=F)

		rownames(BRCA_table)<-BRCA_table[,"SNP"]
		BRCA_table[BRCA_table[,"chr_name"]%in%13,"gene"]<-"BRCA2"
		BRCA_table[BRCA_table[,"chr_name"]%in%17,"gene"]<-"BRCA1"
		
		BRCA_table["i4000377","gene"]<-"BRCA1"
		BRCA_table["i4000378","gene"]<-"BRCA1"
		BRCA_table["i4000379","gene"]<-"BRCA2"
		
		BRCA_table["i4000377","consequence_type_tv"]<-"Direct from 23andme"
		BRCA_table["i4000378","consequence_type_tv"]<-"Direct from 23andme"
		BRCA_table["i4000379","consequence_type_tv"]<-"Direct from 23andme"
		
		
		#get genotypes 
		genotypes<-get_genotypes(uniqueID=uniqueID,request=BRCA_table)
		
		BRCA_table[,"Your genotype"]<-genotypes[rownames(BRCA_table),]

		BRCA_table<-BRCA_table[,c("SNP","gene","Your genotype","normal","polyphen_prediction","sift_prediction","consequence_type_tv")]

		
		#write the score to the log file
		log_function<-function(uniqueID){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"BRCA",uniqueID)
			m<-paste(m,collapse="\t")
			if(file.exists(user_log_file)){
				write(m,file=user_log_file,append=TRUE)
			}else{
				write(m,file=user_log_file,append=FALSE)
			}
		}
		try(log_function(uniqueID))
		
				
		return(BRCA_table)
		
		
		
	})
	
})


