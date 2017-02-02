library("shiny")
# source("/srv/shiny-server/gene-surfer/functions.R")


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
		
		
		#Get gender
		# gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
		
		#get genotypes and calculate gheight
		# genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
		
		# table[,"Your genotype"]<-genotypes[rownames(table),]

		# table<-table[,c("SNP","Your genotype","Comment")]
		# colnames(table)<-c("SNP","Your genotype","Description")
		
		return(table)
		
		
		
	})
	
})


