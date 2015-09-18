library("shiny")


brown<-data.frame(
	h = rep(0.1, 100),
	s=seq(0,1,length.out=100),
	v=seq(1,0,length.out=100)
)
brown[,"col"]<-hsv(h=brown[,"h"],s=brown[,"s"],v=brown[,"v"])



# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	output$haircol1 <- renderPlot({
		blondeness <- input$blondeness
		redheadness <- input$redheadness
		col_provided <- input$col_provided
		input$goButton
		
		plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="blondeness",ylab="redheadness",xaxt="n",yaxt="n",frame=F)
		
		for(blonde in seq(0,1,0.02)){
			for(red in seq(0,1,0.02)){	
				col<-hsv(
					h=0.1 - (red/10),
					s=min(c(1,1-blonde + (red/2))),
					v=blonde 
				)
				points(x=blonde,y=red,col=col,pch=19,cex=2)
			}
		}
		
		if(col_provided){
			print(paste("Drawing point",blondeness,redheadness))
			points(
				x=blondeness/100,
				y=redheadness/100,
				pch=1,
				col="white"
				# col=hsv(h=0.1 - (redheadness/10),s=min(c(1,1-blondeness + (redheadness/2))),v=1-blondeness )
			)
		}
		
	},width=400,height=200)
	
	
	
	
	output$haircol2 <- renderPlot({
		blondeness <- isolate(input$blondeness)
		col_provided <- isolate(input$col_provided)
		input$goButton
		
		plot(NULL,ylim=c(0,100),xlim=c(0,100),ylab="Real hair colour",xlab="Genetic hair colour",frame=F)
		# 		for(i in 1:100){
		# 			symbols(100-(i),50,rectangles=matrix(c(1,100),ncol=2),fg=col[i,"col"],bg=col[i,"col"],add=TRUE,inches=F)	
		# 		}
		# symbols(50,50,rectangles=matrix(c(100,100),ncol=2),fg="black",bg=NA,add=T,inches=F)
		if(col_provided){
			abline(h=blondeness,lwd=2,col="grey30")
		}
		
		
		# 		
		# 		for(component in c("brown","red")){
		# 			GRS_file<-read.table(paste("hairColour/2015-09-18_eriksson_2010_table1_",component,".txt",sep=""),sep="\t",header=T,stringsAsFactors=F,row.names=1)
		# 			
		# 			
		# 			GRS_file<-brown_GRS_file
		# 			GRS_file[,"Effect..Allele"]<-sapply(strsplit(GRS_file[,"Alleles"],"/"),function(x){x[1]})
		# 			GRS_file[,"non_effect_allele"]<-sapply(strsplit(GRS_file[,"Alleles"],"/"),function(x){x[2]})
		# 			
		# 			#get genotypes and calculate gHairColour
		# 			genotypes<-get_genotypes(uniqueID=uniqueID,request=GRS_file)
		# 			gHairColour<-get_gHairColour(genotypes=genotypes,betas=GRS_file)
		# 			assign(paste("gColour",component,sep="_"),gHairColour)
		# 		}
		
		#Still do to -- somehow get the gHairCOlour plotted
		
		
	})
})

