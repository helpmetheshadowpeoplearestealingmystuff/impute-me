library("shiny")



source("/srv/shiny-server/gene-surfer/functions.R")

# uniqueID <- "id_152N62530"

shinyServer(function(input, output) {
	
	output$text_RA1 <- renderText({ 
		input$goButton
			m<-"Explanation on top - always on"
		return(m)
	})
	
	
	output$text_RA2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			height_provided<-isolate(input$height_provided)
			
				m<-"<small><b>Details:</b> More details only shown after go.</small>"
			
		}
		return(m)
	})
	
	
	output$plot_RA1 <- renderPlot({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			

		}else if(input$goButton > 0) {

			
			
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
			
			SNPs_to_analyze_file<-"/srv/shiny-server/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"
			SNPs_to_analyze<-read.table(SNPs_to_analyze_file,sep="\t",stringsAsFactors=F,header=T,row.names=1)
			
			
			GRSs_file<-"/srv/shiny-server/gene-surfer/autoimmuneDiseases/2016-05-17_GRSs_examples.rdata"
			load(GRSs_file)
			
			diseases<-c("AS","CD","PS","PSC","UC")
			disease <- "AS"
			
			#get genotypes and calculate gheight
			genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
			
			
			#remember TO REMOVE THE NON-SIGNIFICANTs
			
			#get risk score
			or_column<-paste0("OR.",disease,".")
			SNPs_to_analyze[,"Beta"]<-log10(SNPs_to_analyze[,or_column])
			
			GRS_beta <-get_GRS(genotypes=genotypes,betas=SNPs_to_analyze)
			
			# GRS_OR <- 10^GRS_beta
			
			hist(GRSs[disease,],breaks=30,main="Risk distributions",xlab="Genetic risk score")
			abline(v=GRS_beta)
			
		}
	})
	
	
	
	
})


