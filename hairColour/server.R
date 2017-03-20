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
	
	
	output$text1 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			col_provided <- isolate(input$col_provided)
			if(!col_provided){
				m<-"<HTML>The circle shows your estimated genetic hair colour. Please consider providing your own hair colour - these algorithms are not fully tuned yet, and for example we really need to hear from someone with red hair.</HTML>"
			}else{
				m<-"<HTML>The black/white circle shows your estimated genetic hair colour. The blue circle shows your real hair colour. By providing this information we can fine-tune our estimation algorithms, which currently leaves quite a lot to be wished for. Thank you!</HTML>"
				
				
			}
			
		}
		return(m)
	})
	
	
	
	
	
	output$haircol1 <- renderPlot({
		
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		col_provided <- isolate(input$col_provided)
		
		#paint the image map
		image(x=x, y=y, z=z, col = d[,"col"], axes = FALSE,xlab="",ylab="")
		
		if(input$goButton > 0) {
			
			
			#Check unique ID
		  uniqueID<-isolate(gsub(" ","",input$uniqueID))
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
				pData<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")
				pData[,"red_hair"]<-real_red
				pData[,"blonde_hair"]<-real_blonde
				write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
			}else{
				real_brown<-NA	
				real_red<-NA
			}
			
			
			
			#get the gColour
			GRS_file_name<-"/home/ubuntu/srv/impute-me/hairColour/SNPs_to_analyze.txt"
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
			pData<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")
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

