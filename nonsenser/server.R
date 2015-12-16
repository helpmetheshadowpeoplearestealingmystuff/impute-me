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
		
		
		#Get gender
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F)[1,"gender"]
		
		load("/srv/shiny-server/gene-surfer/nonsenser/2015-12-16_all_coding_SNPs.rdata")
		
		coding_snps[,"SNP"]<-rownames(coding_snps)

		#get genotypes and calculate gheight
		genotypes<-get_genotypes(uniqueID=uniqueID,request=coding_snps,namingLabel="cached.nonsenser")
		
		coding_snps[,"Your genotype"]<-genotypes[rownames(coding_snps),]

		out<-coding_snps[,c("SNP","Your genotype","Common allele","Frequency","SIFT_pred","MutationTaster_pred","Polyphen2_HDIV_pred")]
		return(out)
	})
	
})


