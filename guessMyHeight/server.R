library("shiny")
library("jsonlite")

#this is for the background "cloud" - it's from the Wood et al study
heights_pre_registered_file<-paste0(get_conf("code_path"),"guessMyHeight/background_heights.txt")
#this is a on-the-fly file for saving user height info
all_heights_file<-paste0(get_conf("misc_files_path"),"all_heights.txt")

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
			m<-"The vertical bar indicates your genetic height. The coloured cloud shows a comparison of real heights and genetic height-scores from a large group of people. From these two you can find your estimated actual height."
		}
		return(m)
	})
	
	
	output$text_height2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			gheight_choice<-isolate(input$gheight_choice)
			if(gheight_choice == "height_30718517"){
			  pmid_link<-"https://pubmed.ncbi.nlm.nih.gov/30718517/"
			}else if(gheight_choice == "height_25282103"){
			  pmid_link<-"https://pubmed.ncbi.nlm.nih.gov/25282103/"
			}else{
			  stop(safeError("Problem with study choice"))
			}
			
			height_provided<-isolate(input$height_provided)
			if(height_provided){
				m<-paste0("<small><b>Details:</b> The largest dot shows your height on the Y-axis and your genetic height on the X-axis. The genetic height is calculated as <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-score</a></u>, based on <u><a href='",pmid_link,"'>this study</a></u>. A Z-score basically means the number of standard deviations above or below the population mean. The population mean is shown as the background colour smear. Smaller dots, if shown, represent previous users.</small>")
			}else{
				m<-paste0("<small><b>Details:</b> The vertical bar shows your genetic height on the X-axis. The genetic height is calculated as <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-score</a></u>, based on <u><a href='",pmid_link,"'>this study</a></u>. A Z-score basically means the number of standard deviations above or below the population mean. The population mean is shown as the background colour smear. If smaller dots are shown, they represent previous users that have volunteered their own height information. The data is corrected for sex as far as this is possible - some data-providers unfortunately do not include measurements on sex-chromosomes.</small>")
			}
		}
		return(m)
	})
	
	
	output$plot_height1 <- renderPlot({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			
			heights_pre_registered<-read.table(heights_pre_registered_file,sep="\t",stringsAsFactors=F,header=T)
			smoothScatter(
				x=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%2,"gheight"],
				y=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%2,"real_height"],
				xlab="genetic height",ylab="real height (cm)"
			)
			
		}else if(input$goButton > 0) {

		  #set and check uniqueID			
		  uniqueID<-isolate(gsub(" ","",input$uniqueID))
			if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
			if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
			if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
				Sys.sleep(3) #wait a little to prevent raw-force fishing	
				stop(safeError("Did not find a user with this id"))
			}
  
			#get advanced options and file paths
			gheight_choice<-isolate(input$gheight_choice)
			sex_choice<-isolate(input$sex_choice)
			pData_path<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
			json_path<-paste(get_conf("data_path"),uniqueID,"/",uniqueID,"_data.json",sep="")
			if(!file.exists(pData_path) | !file.exists(json_path))stop(safeError("Essential sample-files missing. This could indicate a problem with the completion of your data processing."))
			
			
			#check if height is provided - store it if so
			height_provided<-isolate(input$height_provided)
			if(height_provided){
				real_height<-as.numeric(isolate(input$real_height))
				real_age<-as.numeric(isolate(input$real_age))
				if(is.na(real_height))stop("Must give you real height in cm")
				if(is.na(real_age))stop("Must give you real age in years")
				if(real_age<0 | real_age>100)stop("real age must be a number between 0 and 100")
				if(real_height>210 )stop("real height must be number below 210 cm (or write me an email if you are actually more than that)")
				if(real_height<140 ){
					if(real_age>15){
						stop(safeError("for adults, real height must be number above 140 cm (or write me an email if you are actually less than that)"))
					}
				}
				#store this in the pData (because it's mutable data - so pData)
				pData<-read.table(pData_path,header=T,stringsAsFactors=F,sep="\t")
				pData[,"height"]<-real_height
				pData[,"age"]<-real_age
				write.table(pData,file=pData_path,sep="\t",col.names=T,row.names=F,quote=F)
				#else just set them to NA
			}else{
				real_height<-NA	
				real_age<-NA
			}
			
			
			#Get sex/gender and set stereotype colours 
			sex<-gender<-read.table(pData_path,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
			if(sex_choice=="guess"){ #in this case we take it from the pData file, where it's based on the sex-chromosome availability
			  if(sex == 1){
			    backgroundCol<-colorRampPalette(colorRampPalette(c("white", "#08519C"))(10))
			    mainCol<-"dodgerblue"
			  }else if(sex == 2){
			    backgroundCol<-colorRampPalette(colorRampPalette(c("white", "firebrick1"))(10))
			    mainCol<-"red"
			  }else{
			    stop(safeError("Please specify sex under advanced options"))
			  }
			}else if(sex_choice=="male"){
			  backgroundCol<-colorRampPalette(colorRampPalette(c("white", "#08519C"))(10))
			  mainCol<-"dodgerblue"
			  gender <- 1
			}else if(sex_choice=="female"){
			  backgroundCol<-colorRampPalette(colorRampPalette(c("white", "firebrick1"))(10))
			  mainCol<-"red"
			  gender <- 2
			}else{stop("Odd")}
			
			#get height SNPs (for Wood et al - height_25282103)
			SNPs_to_analyze_path<-paste0(get_conf("code_path"),"guessMyHeight/SNPs_to_analyze.txt")
			SNPs_to_analyze<-read.table(SNPs_to_analyze_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
			SNPs_to_analyze<-SNPs_to_analyze[SNPs_to_analyze[,"category"]%in%c("height"),]
			SNPs_to_analyze[,"genotype"]<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
			height_25282103<-signif(sum(get_GRS_2(SNPs_to_analyze,mean_scale = F, unit_variance = F)[,"personal_score"],na.rm=T),3)
			
			#get height SNPs (for Chung et al - height_30718517)
			height_30718517<-try(fromJSON(json_path)[["prs"]][["height_30718517.height.reformat01.txt"]][["GRS"]])
			if(class(height_30718517)=="try-error"){
			  height_30718517<-NA
			}else if(is.null(height_30718517)){
                          height_30718517<-NA
			}else{
			   height_30718517<-signif(height_30718517,3)
			}
			
			#assign the chosen height to plotted variable as gheight
			gheight <- get(gheight_choice)
			if(is.null(gheight) || is.na(gheight))stop(safeError(paste("The",gheight_choice,"height score have not been calculated for this sample.")))
			
			
			#also store this in the all_heights file (for faster loading than the previous pData-based setup)
			line<-paste(c(uniqueID,real_height,sex,height_25282103,height_30718517),collapse="\t")
			if(!is.na(real_height) & uniqueID != "id_613z86871"){ #only save if height is given and it is not the test user
			  try(write(line,file=all_heights_file,append=TRUE))
			}
			
						
			#load database for the cloud/background-comparison
			#this is a file that contains the GWAS heights
			heights_pre_registered<-read.table(heights_pre_registered_file,sep="\t",stringsAsFactors=F,header=T)
			smoothScatter(
				x=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%gender,"gheight"],
				y=heights_pre_registered[heights_pre_registered[,"real_gender"]%in%gender,"real_height"],
				xlab="genetic height",ylab="real height (cm)",
				colramp=backgroundCol
			)
			
			
			
			
			#load previous users data
			heights_in_data <- try(read.table(all_heights_file,sep="\t",header=T,stringsAsFactors = F),silent=T)
			
			
			#make robust against non-initialized files
			if(class(heights_in_data)=="try-error"){
			  heights_in_data <-as.data.frame(matrix(nrow=0,ncol=5,dimnames=list(NULL,c("uniqueID","height","gender","height_25282103","height_30718517"))))
			  if(!file.exists(dirname(all_heights_file)))dir.create(dirname(all_heights_file),recursive=T)
			  write.table(heights_in_data,file=all_heights_file, quote=F,row.names=F,col.names=T,sep="\t")
			}else{
			  if(ncol(heights_in_data)!=5 || !all(colnames(heights_in_data)==c("uniqueID","height","gender","height_25282103","height_30718517"))){
		      
			    #Temporary check of format (later this can be deleted and reverted just to the stop part)
			    if(ncol(heights_in_data)==4 && all(colnames(heights_in_data)==c("uniqueID","height","gheight","gender"))){
			      print("reformatting the all_heights.txt to match new requirements")
			      heights_in_data<-heights_in_data[,c("uniqueID","height","gender","gheight")]
			      heights_in_data[,"height_30718517"]<-NA
			      colnames(heights_in_data)[4]<-"height_25282103"
			      write.table(heights_in_data,all_heights_file,sep="\t",col.names=T,row.names=F,quote=F)
			    }else{
			      stop("Problem with pre-saved height-file")  
			    }
			  }
			}
			
			#only same-gender and only first entry from everyone
			heights_in_data<-heights_in_data[heights_in_data[,"gender"] %in% gender,]
			heights_in_data<-heights_in_data[!duplicated(heights_in_data[,"uniqueID"]),]
			
			#then plot them
			points(
				x=heights_in_data[,gheight_choice],
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
				user_log_file<-paste(get_conf("data_path"),uniqueID,"/user_log_file.txt",sep="")
				m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"guessMyHeight",uniqueID,gheight,gender,real_height,real_age,gheight_choice,sex_choice)
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
			pData_path<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
			if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
				Sys.sleep(3) #wait a little to prevent raw-force fishing	
				stop(safeError("Did not find a user with this id"))
			}
			
			
			
			if(col_provided){
				real_blonde <- isolate(input$blondeness)/100
				real_red <- isolate(input$redheadness)/100
				
				#also store this in the pData (because it's mutable)
				pData<-read.table(pData_path,header=T,sep="\t",stringsAsFactors=F)
				pData[,"red_hair"]<-real_red
				pData[,"blonde_hair"]<-real_blonde
				write.table(pData,file=pData_path,sep="\t",col.names=T,row.names=F,quote=F)
			}
			
			
			#get the gColour
			GRS_file_name<-paste0(get_conf("code_path"),"guessMyHeight/SNPs_to_analyze.txt")
			GRS_file<-read.table(GRS_file_name,sep="\t",header=T,stringsAsFactors=F)
			GRS_file<-GRS_file[GRS_file[,"category"]%in%c("blonde","red"),]
			for(component in c("blonde","red")){
				# print(paste("Getting",component,"g-haircolour"))
				s1<-GRS_file[GRS_file[,"category"]%in%component,]
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
		
		
		
		
	})
	
	
	
})


