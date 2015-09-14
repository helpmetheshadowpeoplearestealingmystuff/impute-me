library("shiny")


options(shiny.maxRequestSize=10*1024^2) 

source("/srv/shiny-server/gene-surfer/functions.R")





# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	
	output$text1 <- renderText({ 
		paste("Currently selected file is\n",input$largeFile[["name"]],"(size",round(input$largeFile[["size"]]/1000000),"MB)")
	})
	
	
	output$text2 <- renderText({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return("")
		}else if(input$goButton == 1) {
			return("Ok")
		}else{
			stop("Please don't try to submit the job more than once.")	
		}
		
	})
	
	
	
	output$text3 <- renderText({ 
		# Take a dependency on input$goButton
		if(input$goButton == 1){
			path <- isolate(input$largeFile[["datapath"]])
			email <- isolate(input$email)
			filename <- isolate(input$largeFile[["name"]])
			if(is.null(path))return("No file selected")
			out<-prepare_23andme_genome(path,email,filename)
			return(out)
		}
	})
})


