library("shiny")


options(shiny.maxRequestSize=10*1024^2) 

source("functions.R")




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
		}
		
		
# 		giant_sup_path<-"/home/ubuntu/misc_files/GIANT_height_250k_Supplementary_Tables_20131030.txt"
# 		giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
# 		giant_sup<-giant_sup[order(abs(giant_sup[,"Beta"]),decreasing=T),]
# 		giant_sup[,"chr_name"]<-giant_sup[,"Chr"]
# 		genotypes<-get_genotypes(uniqueID=uniqueID,request=giant_sup)
# 		

		
		# 		frame()
# 		mtext("Hej")
	})
	
})


