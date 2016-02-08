library("shiny")


source("/srv/shiny-server/gene-surfer/functions.R")


#create the image map
edge<-20
col<-y<-x<-vector()
for(red in seq(0,1,1/edge)){	
	for(blonde in seq(0,1,1/edge)){
		col<-c(col,hsv(h=0.1 - (red/10),s=min(c(1,1-blonde + (red/2))),v=blonde))
		x<-c(x,blonde)
		y<-c(y,red)
	}
}
d<-data.frame(x=x,y=y,col=col,stringsAsFactors=F)
d[,"index"]<-d[,"x"]*edge + d[,"y"]*edge*(1+edge)
z<-matrix(d[,"index"],ncol=edge+1,nrow=edge+1,byrow=F)
x<-y<-seq(0,1,1/edge)

# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	output$text_height1 <- renderText({ 
		height_provided<-isolate(input$height_provided)
		if(height_provided){
			m<-"The large dot indicates your actual height (up the Y-axis) and your genetic height (out the X-axis). If the dot is inside the colour-shading your genetic height matches your actual height."
		}else{
			m<-"The vertical bar indicates your genetic height. The coloured cloud indicates normal actual heights for people with your specific genetic height. Estimated height can therefore be read off on the Y-axis, where the colour-cloud intersect your genetic height."
		}
		return(m)
	})
	
	
	output$text_height2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			height_provided<-isolate(input$height_provided)
			if(height_provided){
				m<-"<small><b>Details:</b> The large dot shows your height on the Y-axis and your genetic height on the X-axis. The genetic height is calculated as <A HREF='https://en.wikipedia.org/wiki/Standard_score'>Z-score</A>, which basically means the number of standard deviations above or below the population mean. The population mean is shown as the background colour smear, and is according to the <u><A HREF='http://www.ncbi.nlm.nih.gov/pubmed/?term=25282103'>currently largest height-GWAS</A></u>. If smaller dots are show, they represent previous users.</small>"
			}else{
				m<-"<small><b>Details:</b> The vertical bar shows your genetic height on the X-axis. The genetic height is calculated as <A HREF='https://en.wikipedia.org/wiki/Standard_score'>Z-score</A>, which basically means the number of standard deviations above or below the population mean. The population mean is shown as the background colour smear, and is according to the <A HREF='http://www.ncbi.nlm.nih.gov/pubmed/?term=25282103'>currently largest height-GWAS</A>. If smaller dots are show, they represent previous users.</small>"
			}
		}
		return(m)
	})
	
	
	output$plot_height1 <- renderPlot({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			
			heights_pre_registered_file<-"/home/ubuntu/misc_files/background_heights.txt"
			heights_pre_registered<-read.table(heights_pre_registered_file,sep="\t",stringsAsFactors=F,header=T)
			smoothScatter(
				x=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%2,"gheight"],
				y=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%2,"real_height"],
				xlab="genetic height",ylab="real height (cm)"
				# colramp=colorRampPalette(colorRampPalette(c("white", "#08519C"))(10))
			)
			
		}else if(input$goButton > 0) {
			
			uniqueID<-isolate(input$uniqueID)
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
			}
			
			#Get gender
			gender<-read.table(pDataFile,header=T,stringsAsFactors=F)[1,"gender"]
			
			
			giant_sup_path<-"/srv/shiny-server/gene-surfer/guessMyHeight/SNPs_to_analyze.txt"
			giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
			
			
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
		}
	})
	
	
	
	output$text_haircol1 <- renderText({ 
		col_provided <- isolate(input$col_provided)
		if(!col_provided){
			m<-"<HTML>This plot shows your estimated genetic hair colour.</HTML>"
		}else{
			m<-"<HTML>This plot shows your estimated genetic hair colour as well as your actual hair colour. Thank you for helping with algorithm optimization.</HTML>"
		}
		return(m)
	})
	
	output$text_haircol2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			col_provided <- isolate(input$col_provided)
			if(!col_provided){
				m<-"<small>The circle shows your estimated genetic hair colour. Please consider providing your own hair colour - these algorithms are not fully tuned yet, and for example we really need to hear from someone with red hair.</small>"
			}else{
				m<-"<small>The black/white circle shows your estimated genetic hair colour. The blue circle shows your real hair colour. By providing this information we can fine-tune our estimation algorithms, which currently leaves quite a lot to be wished for. Thank you!</small>"
			}
		}
		return(m)
	})
	
	
	
	
	output$plot_haircol1 <- renderPlot({
		uniqueID <- isolate(input$uniqueID)
		col_provided <- isolate(input$col_provided)
		
		#paint the image map
		image(x=x, y=y, z=z, col = d[,"col"], axes = FALSE,xlab="",ylab="")
		
		if(input$goButton > 0) {
			
			
			#Check unique ID
			uniqueID<-isolate(input$uniqueID)
			if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
			if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
			pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
			if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
				Sys.sleep(3) #wait a little to prevent raw-force fishing	
				stop("Did not find a user with this id")
			}
			
			
			
			if(col_provided){
				real_blonde <- isolate(input$blondeness)/100
				real_red <- isolate(input$redheadness)/100
				
				#also store this in the pData
				pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
				pData[,"red_hair"]<-real_red
				pData[,"blonde_hair"]<-real_blonde
				write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
			}else{
				real_brown<-NA	
				real_red<-NA
			}
			
			
			
			#get the gColour
			GRS_file_name<-"/srv/shiny-server/gene-surfer/hairColour/SNPs_to_analyze.txt"
			GRS_file<-read.table(GRS_file_name,sep="\t",header=T,stringsAsFactors=F)
			for(component in c("brown","red")){
				print(paste("Getting",component,"g-haircolour"))
				GRS_file_here<-GRS_file[GRS_file[,"Category"]%in%component,]
				rownames(GRS_file_here)<-GRS_file_here[,"SNP"]
				#get genotypes and calculate gHairColour
				genotypes<-get_genotypes(uniqueID=uniqueID,request=GRS_file_here)
				gHairColour<-get_GRS(genotypes=genotypes,betas=GRS_file_here)
				assign(paste("gColour",component,sep="_"),gHairColour)
			}
			
			#also store this in the pData
			pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
			pData[,"g_red_hair"]<-gColour_red
			pData[,"g_brown_hair"]<-gColour_brown
			write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
			
			
			
			#Calibrating and plotting
			brown_calibrate<-function(x){max(c(0,min(c(1, ((x-8)/50)))))}
			red_calibrate<-function(x){max(c(0.1,min(c(1,1-(x/6)))))}
			
			blondeness<-brown_calibrate(gColour_brown)
			redheadness<-red_calibrate(gColour_red)
			
			
			points(x=blondeness,y=redheadness,pch=1,col="white",cex=10,lwd=1)
			points(x=blondeness,y=redheadness,pch=1,col="gray50",cex=9.8,lwd=1)
			points(x=blondeness,y=redheadness,pch=1,col="black",cex=9.6,lwd=1)
			points(x=blondeness,y=redheadness,pch=1,col="gray50",cex=9.4,lwd=1)
			points(x=blondeness,y=redheadness,pch=1,col="white",cex=9.2,lwd=1)
			
			if(col_provided){
				
				points(x=real_blonde,y=real_red,pch=1,col="cyan",cex=10,lwd=1)
				points(x=real_blonde,y=real_red,pch=1,col="dodgerblue",cex=9.8,lwd=1)
				points(x=real_blonde,y=real_red,pch=1,col="darkblue",cex=9.6,lwd=1)
				points(x=real_blonde,y=real_red,pch=1,col="dodgerblue",cex=9.4,lwd=1)
				points(x=real_blonde,y=real_red,pch=1,col="cyan",cex=9.2,lwd=1)
			}
		}
	})#,width=400,height=200)
	
	
	
})


