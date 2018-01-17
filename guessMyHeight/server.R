library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")


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
		input$goButton
		height_provided<-isolate(input$height_provided)
		if(height_provided){
			m<-"The large dot indicates your actual height (up the Y-axis) and your genetic height (out the X-axis). If the dot is inside the colour-shading your genetic height matches your actual height."
		}else{
			m<-"The vertical bar indicates your genetic height. The coloured cloud indicates normal actual heights for people with your specific genetic height. From these two you can find your estimated actual height."
		}
		return(m)
	})
	
	
	output$text_height2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			height_provided<-isolate(input$height_provided)
			if(height_provided){
				m<-"<small><b>Details:</b> The largest dot shows your height on the Y-axis and your genetic height on the X-axis. The genetic height is calculated as <A HREF='https://en.wikipedia.org/wiki/Standard_score'>Z-score</A>, which basically means the number of standard deviations above or below the population mean. The population mean is shown as the background colour smear, and is according to the <u><A HREF='http://www.ncbi.nlm.nih.gov/pubmed/?term=25282103'>currently largest height-GWAS</A></u>. Smaller dots shown represent other users.</small>"
			}else{
				m<-"<small><b>Details:</b> The vertical bar shows your genetic height on the X-axis. The genetic height is calculated as <u><A HREF='https://en.wikipedia.org/wiki/Standard_score'>Z-score</A></u>, which basically means the number of standard deviations above or below the population mean. The population mean is shown as the background colour smear, and is according to the <u><A HREF='http://www.ncbi.nlm.nih.gov/pubmed/?term=25282103'>currently largest height-GWAS</A></u>. If smaller dots are shown, they represent previous users.</small>"
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
			
		  uniqueID<-isolate(gsub(" ","",input$uniqueID))
			if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
			if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
			pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
			
			if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
				Sys.sleep(3) #wait a little to prevent raw-force fishing	
				stop(safeError("Did not find a user with this id"))
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
				pData<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")
				pData[,"height"]<-real_height
				pData[,"age"]<-real_age
				write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
				
			}else{
				real_height<-NA	
				real_age<-NA
			}
			
			#Get gender
			gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
			
			
			giant_sup_path<-"/home/ubuntu/srv/impute-me/guessMyHeight/SNPs_to_analyze.txt"
			giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
			
			
			#get genotypes and calculate gheight
			genotypes<-get_genotypes(uniqueID=uniqueID,request=giant_sup)
			gheight<-get_GRS(genotypes=genotypes,betas=giant_sup)
			
			
			#also store this in the pData
			pData<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")
			pData[,"gheight"]<-gheight
			write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)

			#also store this in the all_heights file (for faster loading)
			line<-paste(c(uniqueID,real_height,gheight,gender),collapse="\t")
			all_heights_file<-"/home/ubuntu/misc_files/all_heights.txt"
			if(!is.na(real_height)){
			  write(line,file=all_heights_file,append=TRUE)  
			}
			
						
			
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
			# otherPersons<-list.files("/home/ubuntu/data/",full.names=T)
			# heights_in_data<-data.frame(height=vector(),gheight=vector(),gender=vector(),stringsAsFactors=F)
			# for(otherPerson in otherPersons){
			# 	if(!file.info(otherPerson)[["isdir"]])next
			# 	if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
			# 	otherPersonPdata<-try(read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F,comment.char="",quote=""),silent=T)
			# 	if(class(otherPersonPdata)=="try-error")next
			# 	if(!all(c("gheight","height","gender")%in%colnames(otherPersonPdata)))next
			# 	if(otherPersonPdata[1,"gender"] != gender) next #only plot persons of the same gender
			# 	heights_in_data<-rbind(heights_in_data,otherPersonPdata[1,c("height","gheight","gender")])
			# }
			heights_in_data <- read.table(all_heights_file,sep="\t",header=T,stringsAsFactors = F)
			#only same-gender
			heights_in_data<-heights_in_data[heights_in_data[,"gender"] %in% gender,]
			
			
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
			
			
			#write the score to the log file
			log_function<-function(uniqueID,gheight,gender){
				user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
				m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"guessMyHeight",uniqueID,gheight,gender)
				m<-paste(m,collapse="\t")
				if(file.exists(user_log_file)){
					write(m,file=user_log_file,append=TRUE)
				}else{
					write(m,file=user_log_file,append=FALSE)
				}
			}
			try(log_function(uniqueID,gheight,gender))
			
		}
	})
	
	
	
	output$text_haircol1 <- renderText({ 
		col_provided <- isolate(input$col_provided)
		input$goButton
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
				m<-"<small>The circle shows your estimated genetic hair colour. Please consider providing your own hair colour - these algorithms could use a bit better tuning, and for example we really need to hear from someone with red hair.</small>"
			}else{
				m<-"<small>The black/white circle shows your estimated genetic hair colour. The blue circle shows your real hair colour. By providing this information we can fine-tune our estimation algorithms, which currently leaves quite a lot to be wished for. Thank you!</small>"
			}
		}
		return(m)
	})
	
	
	
	
	output$plot_haircol1 <- renderPlot({
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		col_provided <- isolate(input$col_provided)
		
		#paint the image map
		image(x=x, y=y, z=z, col = d[,"col"], axes = FALSE,xlab="",ylab="")
		
		if(input$goButton > 0) {
			
			
			#Check unique ID
		  uniqueID<-isolate(gsub(" ","",input$uniqueID))
			if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
			if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
			pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
			if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
				Sys.sleep(3) #wait a little to prevent raw-force fishing	
				stop(safeError("Did not find a user with this id"))
			}
			
			
			
			if(col_provided){
				real_blonde <- isolate(input$blondeness)/100
				real_red <- isolate(input$redheadness)/100
				
				#also store this in the pData
				pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
				pData[,"red_hair"]<-real_red
				pData[,"blonde_hair"]<-real_blonde
				write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
			}
			
			
			#get the gColour
			GRS_file_name<-"/home/ubuntu/srv/impute-me/hairColour/SNPs_to_analyze.txt"
			GRS_file<-read.table(GRS_file_name,sep="\t",header=T,stringsAsFactors=F)
			for(component in c("blonde","red")){
				print(paste("Getting",component,"g-haircolour"))
				s1<-GRS_file[GRS_file[,"Category"]%in%component,]
				rownames(s1)<-s1[,"SNP"]
				#get genotypes and calculate gHairColour
				s1[,"genotype"]<-get_genotypes(uniqueID=uniqueID,request=s1)
				
				s1<-get_GRS_2(s1,mean_scale=T,unit_variance=T)
				population_sum_sd<-sqrt(sum(s1[,"population_score_sd"]^2,na.rm=T))
				GRS <-sum(s1[,"score_diff"],na.rm=T) / population_sum_sd  
				assign(paste("gColour",component,sep="_"),GRS)
			}
			

			#Calibrating and plotting (must be on a scale from 0 to 1, 1 being more up or more right)
			#from distribution analysis we know that max blond is 5 and min blonde is -2, and e.g. chinese is -0.8 whereas
			#my sister is 4.1. There's no known red-heads, so we just take the extremes of -3 to 4
			
			
			blond_calibrate<-function(x){max(c(0,min(c(1, (x+1)/6))))}
			red_calibrate<-function(x){max(c(0,min(c(1, (x+1)/5))))}
			
			
			
			
			blondeness<-blond_calibrate(gColour_blonde)
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


