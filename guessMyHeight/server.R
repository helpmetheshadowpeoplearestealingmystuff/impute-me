library("shiny")


source("/srv/shiny-server/gene-surfer/functions.R")




# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	
	output$text1 <- renderText({ 
		paste("None")
	})
	
	
	output$plot1 <- renderPlot({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
		uniqueID<-isolate(input$uniqueID)
		print(nchar(uniqueID))
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop("Did not find a user with this id")
		}
		
		height_provided<-isolate(input$height_provided)
		if(height_provided){
			real_height<-as.numeric(isolate(input$real_height))
			real_age<-as.numeric(isolate(input$real_age))
			
			if(is.na(real_height))stop("Must give you real height in cm")
			if(is.na(real_age))stop("Must give you real age in years")
			
			if(real_age<0 | real_age>100)stop("real age must be a number between 0 and 100")
			if(real_height>210 )stop("real height must be number below 210 cm (or write me an email if you are actually taller than that)")
			if(real_height<140 ){
				if(real_age>15){
					stop("for adults, real height must be number above 150 cm (or write me an email if you are actually shorter than that)")
				}
			}
			
			#also store this in the pData
			pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
			pData[,"height"]<-real_height
			pData[,"age"]<-real_age
			write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
			
		}else{
			real_height<-NA	
			real_age<-NA
			
			pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
			if("height"%in%colnames(pData)){
				real_height<-pData[1,"height"]
			}else{
				real_height<-NA	
			}
			if("age"%in%colnames(pData)){
				real_age<-pData[1,"age"]
			}else{
				real_age<-NA	
			}
			
		}
		
		#Get gender
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F)[1,"gender"]
		
		
		giant_sup_path<-"/home/ubuntu/misc_files/GIANT_modified_table.txt"
		giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
		giant_sup[,"chr_name"]<-giant_sup[,"Chr"]
		
		#get genotypes and calculate gheight
		genotypes<-get_genotypes(uniqueID=uniqueID,request=giant_sup)
		gheight<-get_gheight(genotypes=genotypes,betas=giant_sup)
		
		
		#also store this in the pData
		pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
		pData[,"gheight"]<-gheight
		write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
		
		
		#load database for comparison
		#this is a file that could contain the GWAS heights
		heights_pre_registered_file<-"/home/ubuntu/misc_files/height_registrered.txt"
		heights_pre_registered<-read.table(heights_pre_registered_file,sep="\t",stringsAsFactors=F)
		colnames(heights_pre_registered)<-c("time","id","gheight","height","age","gender","real")
		heights_pre_registered<-heights_pre_registered[,c("height","gheight","gender")]
		
		
		#this loops over all pData files
		otherPersons<-list.files("/home/ubuntu/data/",full.names=T)
		heights_in_data<-data.frame(height=vector(),gheight=vector(),gender=vector(),stringsAsFactors=F)
		for(otherPerson in otherPersons){
			if(!file.info(otherPerson)[["isdir"]])next
			if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
			otherPersonPdata<-read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F)
			if(!all(c("gheight","height","gender")%in%colnames(otherPersonPdata)))next
			heights_in_data<-rbind(heights_in_data,otherPersonPdata[1,c("height","gheight","gender")])
		}
		
		heights_in_data<-rbind(heights_in_data,heights_pre_registered)
		
		male_heights<-heights_in_data[heights_in_data[,"gender"]%in%1,]
		female_heights<-heights_in_data[heights_in_data[,"gender"]%in%2,]
		
		xlim<-range(heights_in_data[,"gheight"],na.rm=T)
		ylim_male <-range(male_heights[,"height"],na.rm=T)
		ylim_female <-range(female_heights[,"height"],na.rm=T)
		
		par(mai=c(1.36,1.093333,1.093333,0.960000))
		plot(NULL,xlim=xlim,ylim=c(0,1),xlab="genetic height",yaxt="n",ylab="")
		axis(2,at=seq(0,1,0.1), labels=round(seq(from=ylim_male[1],to=ylim_male[2],length.out=11)))
		axis(4,at=seq(0,1,0.1), labels=round(seq(from=ylim_female[1],to=ylim_female[2],length.out=11)))
		
		mtext("Male height (cm)",side=2,padj=-4)
		points(
			x=male_heights[,"gheight"], 
			y=(male_heights[,"height"]-ylim_male[1])/(ylim_male[2]-ylim_male[1]),#unit scale 
			col="dodgerblue", #gotta love stereotypes for clean communcation
			pch=19
		)
		mtext("Female height (cm)",side=4,padj=4)
		points(
			x=female_heights[,"gheight"], 
			y=(female_heights[,"height"]-ylim_female[1])/(ylim_female[2]-ylim_female[1]),#unit scale 
			col="red", #gotta love stereotypes for clean communcation
			pch=19
		)
		
		if(height_provided){
			if(gender==1){
				y<-(real_height-ylim_male[1])/(ylim_male[2]-ylim_male[1])
				points(x=gheight, y=real_height,cex=3, col="dodgerblue",pch=19)
			}else{
				y<-(real_height-ylim_female[1])/(ylim_female[2]-ylim_female[1])
				points(x=gheight, y=real_height,cex=3, col="red",pch=19)
			}
		}else{
			abline(v=	gheight, lwd=2, col="red")
			
		}
		
	})
	
})


