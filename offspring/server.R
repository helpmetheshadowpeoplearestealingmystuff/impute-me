library("shiny")
source("/home/ubuntu/srv/impute-me/functions.R")


# Define server logic
shinyServer(function(input, output) {
	output$table1 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
		uniqueID<-isolate(input$uniqueID)
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop("Did not find a user with this id")
		}

		
		
		
		table_file <-"../offspring/SNPs_to_analyze.txt"
		table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
		rownames(table)<-table[,"SNP"]
		#This will return a copy of the SNPs_to_analyze.txt, with the genotypes of this specific person (=uniqueID) as a new column
		genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
		table[,"Your genotype"]<-genotypes[rownames(table),]
		
		
		#
		#
		#
		#
		#
		#(Optionally) Do any calculations necessary here. Or just return the table as is for showing on web site.
		#
		#
		#
		#
		#
		
		return(table)
		
	},include.rownames = FALSE)
})