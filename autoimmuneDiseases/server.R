library("shiny")



source("/srv/shiny-server/gene-surfer/functions.R")

uniqueID <- "id_152N62530"

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
			
			
			diseases<-c("AS","CD","PS","PSC","UC")
			disease <- "AS"
			
			#get genotypes and calculate gheight
			genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
			
			
			#get risk score
			beta_column<-paste0("OR.",disease,".")

			gheight<-get_GRS(genotypes=genotypes,betas=SNPs_to_analyze,beta_column=beta_column)
			
			
			#also store this in the pData
			pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
			pData[,"gheight"]<-gheight
			write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
			
			
			#set gender stereotype colours
			if(gender == 1){
				backgroundCol<-colorRampPalette(colorRampPalette(c("white", "#08519C"))(10))
				mainCol<-"dodgerblue"
			}else{
				backgroundCol<-colorRampPalette(colorRampPalette(c("white", "firebrick1"))(10))
				mainCol<-"red"
			}
			
			
			#load database for comparison
			#this is a file that contains the GWAS heights
			heights_pre_registered_file<-"/home/ubuntu/misc_files/background_heights.txt"
			heights_pre_registered<-read.table(heights_pre_registered_file,sep="\t",stringsAsFactors=F,header=T)
			smoothScatter(
				x=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%gender,"gheight"],
				y=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%gender,"real_height"],
				xlab="genetic height",ylab="real height (cm)",
				colramp=backgroundCol
			)
			
			
			
			
			#load previous users data
			otherPersons<-list.files("/home/ubuntu/data/",full.names=T)
			heights_in_data<-data.frame(height=vector(),gheight=vector(),gender=vector(),stringsAsFactors=F)
			for(otherPerson in otherPersons){
				if(!file.info(otherPerson)[["isdir"]])next
				if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
				otherPersonPdata<-read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F)
				if(!all(c("gheight","height","gender")%in%colnames(otherPersonPdata)))next
				if(otherPersonPdata[1,"gender"] != gender) next #only plot persons of the same gender
				heights_in_data<-rbind(heights_in_data,otherPersonPdata[1,c("height","gheight","gender")])
			}
			#then plot them
			points(
				x=heights_in_data[,"gheight"],
				y=heights_in_data[,"height"],
				col="black",
				bg=mainCol,
				pch=21
			)
			
			
			#Plot the current users data
			if(height_provided){
				points(x=gheight, y=real_height,cex=3, col="black",bg=mainCol,pch=21)
			}else{
				abline(v=	gheight, lwd=2, col=mainCol)
			}
		}
	})
	
	
	
	
})


