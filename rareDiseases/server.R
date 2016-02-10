library("shiny")


source("/srv/shiny-server/gene-surfer/functions.R")


# uniqueID<-"id_57n662948"

# Define server logic for random distribution application
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
		table_file <-"/srv/shiny-server/gene-surfer/rareDiseases/SNPs_to_analyze.txt"
		table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
		rownames(table)<-table[,"SNP"]
		genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
		
		table[,"Your genotype"]<-genotypes[rownames(table),]
		table<-table[,c("SNP","Your genotype","risk_allele","non_risk_allele","disease_name")]
		colnames(table)<-c("SNP","Your genotype","Risk-allele","Non-Risk-allele","Inherited Condition")
		return(table)
		
	},include.rownames = FALSE)
	
})


