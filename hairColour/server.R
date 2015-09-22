library("shiny")


brown<-data.frame(
	h = rep(0.1, 100),
	s=seq(0,1,length.out=100),
	v=seq(1,0,length.out=100)
)
brown[,"col"]<-hsv(h=brown[,"h"],s=brown[,"s"],v=brown[,"v"])



source("/srv/shiny-server/gene-surfer/functions.R")


# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	output$haircol1 <- renderPlot({
		blondenessProvided <- isolate(input$blondeness)
		redheadnessProvided <- isolate(input$redheadness)
		uniqueID <- isolate(input$uniqueID)
		col_provided <- isolate(input$col_provided)
		input$goButton
		
		plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="blondeness",ylab="redheadness",xaxt="n",yaxt="n",frame=F)
		
		for(blonde in seq(0,1,0.01)){
			for(red in seq(0,1,0.01)){	
				col<-hsv(
					h=0.1 - (red/10),
					s=min(c(1,1-blonde + (red/2))),
					v=blonde 
				)
				points(x=blonde,y=red,col=col,pch=19,cex=2)
			}
		}
		
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
		
		
		
		
		blondeness<-max(c(0,min(c(1, ((gColour_brown-8)/50)))))
		
		
		redheadness<-max(c(0.1,min(c(1,1-(gColour_red/6)))))
		
		# ?points
		# m<-paste("Blondeness:",gColour_brown,"-",blondeness,"Readheadness:",gColour_red,"-",redheadness)
		# mtext(m)
		points(x=blondeness,y=redheadness,pch=1,col="white",cex=10,lwd=2)
		points(x=blondeness,y=redheadness,pch=1,col="gray50",cex=9.8,lwd=2)
		points(x=blondeness,y=redheadness,pch=1,col="black",cex=9.6,lwd=2)
		points(x=blondeness,y=redheadness,pch=1,col="gray50",cex=9.4,lwd=2)
		points(x=blondeness,y=redheadness,pch=1,col="white",cex=9.2,lwd=2)
		
		if(col_provided){
			points(
				x=blondenessProvided/100,
				y=redheadnessProvided/100,
				pch=19,
				col="blue"
			)
		}
		
	})#,width=400,height=200)
	
	
	
})

