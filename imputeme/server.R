

library("shiny")


options(shiny.maxRequestSize=400*1024^2) 

source("/home/ubuntu/srv/impute-me/functions.R")






# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  output$text <- renderText({ 
    #preparing progress tracker      
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "", value = 0)
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
      email <- isolate(input$email)
      protect_from_deletion <- FALSE
      filename <- isolate(input$largeFile[["name"]])
      
      if(is.null(path))return("No file ready - make sure the upload tracker says 'upload completed' before clicking go")
      
      
      
      
      
      
      #executing prepare function
      out <- prepare_individual_genome(
        path=path,
        email=email,
        filename=filename, 
        updateProgress = updateProgress,
        protect_from_deletion=protect_from_deletion
      )
      return(out)
      
    }
  })
})


