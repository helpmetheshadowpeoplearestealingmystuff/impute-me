library("shiny")


source("/srv/shiny-server/gene-surfer/functions.R")


# uniqueID<-"id_57n662948"

# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	output$table1 <- renderDataTable({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
		uniqueID<-isolate(input$uniqueID)
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop("Did not find a user with this id")
		}

		
		table_file <-"/srv/shiny-server/gene-surfer/statins/SNPs_to_analyze.txt"
		table<-read.table(SLCO1B1_table_file,sep="\t",header=T,stringsAsFactors=F)
		rownames(table)<-table[,"SNP"]
		genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
		
		table[,"Your genotype"]<-genotypes[rownames(table),]
		table<-table[,c("SNP","Your genotype","effect_allele")]
		colnames(table)<-c("SNP","Your genotype","Risk allele")
		table<-table["rs2395029",,drop=FALSE]
		return(table)

	})
	
	
	
	output$table2 <- renderDataTable({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
		uniqueID<-isolate(input$uniqueID)
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop("Did not find a user with this id")
		}
		
		
		#Get gender
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F)[1,"gender"]
		
		
		SLCO1B1_table_file <-"/srv/shiny-server/gene-surfer/statins/SNPs_to_analyze.txt"
		SLCO1B1_table<-read.table(SLCO1B1_table_file,sep="\t",header=T,stringsAsFactors=F)

		rownames(SLCO1B1_table)<-SLCO1B1_table[,"SNP"]

		#get genotypes and calculate gheight
		genotypes<-get_genotypes(uniqueID=uniqueID,request=SLCO1B1_table)
		
		SLCO1B1_table[,"Your genotype"]<-genotypes[rownames(SLCO1B1_table),]

		SLCO1B1_table<-SLCO1B1_table[,c("SNP","Your genotype","effect_allele")]
		colnames(SLCO1B1_table)<-c("SNP","Your genotype","Risk allele")
		SLCO1B1_table<-SLCO1B1_table["rs4363657",,drop=FALSE]
		return(SLCO1B1_table)
		
		
		
	})
	
})


