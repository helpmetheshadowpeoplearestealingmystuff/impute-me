library("shiny")


options(shiny.maxRequestSize=150*1024^2) 







shinyServer(function(input, output) {

	output$text <- renderText({ 
	  #start progress tracker			
	  progress <- shiny::Progress$new()
	  on.exit(progress$close())
	  progress$set(message = 'Checking file-upload', value = 0)
	  updateProgress <- function(value = NULL, detail = NULL, max=NULL) {
	    if(is.null(max))max <- 50
	    if (is.null(value)) {
	      value <- progress$getValue()
	      value <- value + 1/max
	    }
	    progress$set(value = value, detail = detail)
	  }

	  # Take a dependency on input$goButton
		if(input$goButton > 0){
			path <- isolate(input$largeFile[["datapath"]])
      if(is.null(path))return("No file selected - don't press 'start imputation' before tracker says upload complete.")

			#start preparing
			out<-prepare_imputemany_genome(
			  path=path,
			  email = isolate(input$email),
			  updateProgress = updateProgress,
			  should_be_imputed = isolate(input$should_be_imputed),
			  protect_from_deletion = FALSE,
			  filename=isolate(input$largeFile[["name"]])
			)
			
			return(out)
		}
	})
})


