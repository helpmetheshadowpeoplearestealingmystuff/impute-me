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
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F)[1,"gender"]
		
		
		BRCA_table_file <-"BRCA/SNPs_to_analyze.txt"
		BRCA_table<-read.table(BRCA_table_file,sep="\t",header=T,stringsAsFactors=F)

		
		#get genotypes and calculate gheight
		genotypes<-get_genotypes(uniqueID=uniqueID,request=giant_sup)
		gheight<-get_GRS(genotypes=genotypes,betas=giant_sup)
		
		
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
		
		
		
# 		male_heights<-heights_in_data[heights_in_data[,"gender"]%in%1,]
# 		female_heights<-heights_in_data[heights_in_data[,"gender"]%in%2,]
# 		
# 		xlim<-range(heights_in_data[,"gheight"],na.rm=T)
# 		ylim_male <-range(male_heights[,"height"],na.rm=T)
# 		ylim_female <-range(female_heights[,"height"],na.rm=T)
# 		
# 		par(mai=c(1.36,1.093333,1.093333,0.960000))
# 		plot(NULL,xlim=xlim,ylim=c(0,1),xlab="genetic height",yaxt="n",ylab="")
# 		axis(2,at=seq(0,1,0.1), labels=round(seq(from=ylim_male[1],to=ylim_male[2],length.out=11)))
# 		axis(4,at=seq(0,1,0.1), labels=round(seq(from=ylim_female[1],to=ylim_female[2],length.out=11)))
# 		
# 		mtext("Male height (cm)",side=2,padj=-4)
# 		points(
# 			x=male_heights[,"gheight"], 
# 			y=(male_heights[,"height"]-ylim_male[1])/(ylim_male[2]-ylim_male[1]),#unit scale 
# 			col="dodgerblue", #gotta love stereotypes for clean communcation
# 			pch=19
# 		)
# 		mtext("Female height (cm)",side=4,padj=4)
# 		points(
# 			x=female_heights[,"gheight"], 
# 			y=(female_heights[,"height"]-ylim_female[1])/(ylim_female[2]-ylim_female[1]),#unit scale 
# 			col="red", #gotta love stereotypes for clean communcation
# 			pch=19
# 		)
# 		
# 		if(height_provided){
# 			if(gender==1){
# 				y<-(real_height-ylim_male[1])/(ylim_male[2]-ylim_male[1])
# 				points(x=gheight, y=real_height,cex=3, col="dodgerblue",pch=19)
# 			}else{
# 				y<-(real_height-ylim_female[1])/(ylim_female[2]-ylim_female[1])
# 				points(x=gheight, y=real_height,cex=3, col="red",pch=19)
# 			}
# 		}else{
# 			abline(v=	gheight, lwd=2, col="red")
# 			
# 		}
		
	})
	
})


