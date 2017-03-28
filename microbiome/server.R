library("shiny")
source("/home/ubuntu/srv/impute-me/functions.R")


#Replace 'template' with name of module throughout the script

# Define server logic for a template
shinyServer(function(input, output) {
	
	
	
	output$text_1 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			m<-paste0("<small><b>Your interpretation:</b> For each of the strains of bacteria you can read your own genotype and see if your are host-genetically disposed to have an increased proportion of this particular strain. The consequence of being particularly disposed are fairly unclear, however, but refer to the primary literature on gut microbiome for more information.<br><br><br>
								<b>Methods:</b> The findings were taken from the study by <u><a href='http://www.ncbi.nlm.nih.gov/pubmed/?term=26374288'>Blekhman et al (2015)</a></u>. Supplementary material table S5.</small>")
			
		}
		return(m)
	})
	
	
	
	
	
	
	
	
	
	output$table1 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError("Did not find a user with this id"))
		}

		
		
		
		table_file <-"../microbiome/SNPs_to_analyze.txt"
		table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
		rownames(table)<-table[,"SNP"]
		#This will return a copy of the SNPs_to_analyze.txt, with the genotypes of this specific person (=uniqueID) as a new column
		genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
		table[,"Your genotype"]<-genotypes[rownames(table),]
		
		
		
		table <- table[,c("SNP","Your genotype","Increasing_allele","Bacteria")]
		
		#write the score to the log file
		log_function<-function(uniqueID){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"microbiome",uniqueID)
			m<-paste(m,collapse="\t")
			if(file.exists(user_log_file)){
				write(m,file=user_log_file,append=TRUE)
			}else{
				write(m,file=user_log_file,append=FALSE)
			}
		}
		try(log_function(uniqueID))
		
		return(table)
		
	},include.rownames = FALSE)
})