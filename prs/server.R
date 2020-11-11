library("shiny")
library("jsonlite")
source("/home/ubuntu/srv/impute-me/functions.R")





# Define server logic for a template
shinyServer(function(input, output) {
  output$text <- renderText({ 
    if(input$goButton == 0){
      message <- "<br>Polygenic risk scores essentially means the weighted sum of effects from many SNPs. How they are calculated is a matter of great debate in current genetics. This module explores their calculation using the so-called <u><a href='https://github.com/bvilhjal/ldpred'>LD-pred</a></u> algorithm. The main difference between this and the current <u><a href='https://www.impute.me/AllDiseases/'>Complex Disease</a></u> module is that virtually all measured SNPs in the entire genome are included in the calculation, not just those that are genome-wide significant.<br><br>"
    }else if(input$goButton > 0) {
      message <- "<br>Since the module is highly experimental, the current reporting basically is the LD-pred score output as is, along with QC information on number of included SNPs. This means that in this module the score currently only can be interpreted on a relative scale, i.e. you can upload two samples and see which one is higher.<br><br>
      In the main 'complex disease' module however, these scores are extracted and normalized to zero-mean and unit-variance. This means that the scores when browsed in the main module are Z-scores, as is indeed the aim to have all scores be.<br><br>"
    }
    return(message)
  })
  
  
  
  get_data <- reactive({
    if(input$goButton == 0){
      return(NULL)
    }
    study_id <- isolate(input$trait_choice)
    uniqueID<-gsub(" ","",isolate(input$uniqueID))
    ethnicity_group<-input$ethnicity_group
    
    
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError("Did not find a user with this id"))
    }
    
    #json file loading
    json_file<-paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json")
    if(!file.exists(json_file))stop(safeError("Didn't find a json data file. Maybe data was from before implementation of this?"))
    d<-fromJSON(json_file)
    
    #check existence of content
    if(!"prs" %in% names(d))stop(safeError("Didn't find a PRS-entry in your calculations. Submission was probably made prior to launch of PRS module"))
    if(!study_id %in% names(d[["prs"]]))stop(safeError("Didn't find a PRS-entry for this trait in your calculations. Submission was probably made prior to implementation. Try another trait or re-upload data."))
    
    #extract relevant data
    d1 <- d[["prs"]][[study_id]]

    #stop if the alleles checked count is too low
    if(d1[["alleles_observed"]] < 600000)stop(safeError(paste("Only",d1[["alleles_checked"]],"variants were found to be available for PRS calculation. This could indicate that something went wrong in the calculation and that the PRS should not be trusted.")))

    
    #ethnicity considerations
    if(ethnicity_group == "automatic"){
      e<-try(d[["ethnicity"]][["guessed_super_pop"]],silent=F)
      if(is.null(e) || is.na(e) ||  !e %in% c("AFR", "AMR", "EAS", "EUR", "SAS")){
        ethnicity_group<-"global"
      }else{
        ethnicity_group <- e
      }
    }
    if(ethnicity_group == "global"){
      #do nothing. Note the density curve location.
      densityCurvePath<-"/home/ubuntu/srv/impute-me/prs/2019-09-17_densities_ALL.rdata"
    }else{
      #note the density curve location
      densityCurvePath<-paste0("/home/ubuntu/srv/impute-me/prs/2019-09-17_densities_",ethnicity_group,".rdata")
    }
    
    
    load(densityCurvePath)
    if(!paste0(study_id,"_y") %in% rownames(densities))stop(safeError(paste("This",study_id,"trait was not found to have density-plotting available")))
    
    distributionCurve<-list(x=densities[paste0(study_id,"_x"),],y=densities[paste0(study_id,"_y"),])
    
    
    
    #write the score to the log file
    log_function<-function(uniqueID){
      user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"prs",uniqueID,study_id,signif(d1[["GRS"]],3))
      m<-paste(m,collapse="\t")
      if(file.exists(user_log_file)){
        write(m,file=user_log_file,append=TRUE)
      }else{
        write(m,file=user_log_file,append=FALSE)
      }
    }
    try(log_function(uniqueID))
    
    #return data    
    return(list(
      d1 = d1,
      distributionCurve=distributionCurve
    ))
    
  })
  
  
  output$table1 <- renderTable({ 
    if(input$goButton == 0){
      return(NULL)
    }
    
    
    o <- get_data()
    
    d1 <- o[["d1"]]
    
    #create basic result table
    table <- data.frame(
      "Polygenic Risk Score"=signif(d1[["GRS"]],3),
      "Alleles checked (plink CNT)"=d1[["alleles_checked"]],
      "Alleles observed (plink CNT2)"=d1[["alleles_observed"]],
      check.names=F
    )
    
    
    
    
    return(table)
    
  },include.rownames = FALSE)
  
  
  
  
  
  output$plot_1 <- renderPlot({ 
    if(input$goButton == 0){
      return(NULL)
    }
    
    o<-get_data()
    d1<-o[["d1"]]
    GRS_beta <- d1[["GRS"]]
    
    
    distributionCurve <- o[["distributionCurve"]]
    xlim<-range(distributionCurve[["x"]])
    control_mean<-0
    control_sd<-1
    # y_control<-dnorm(x,mean=control_mean,sd=control_sd)
    ylim <- c(0,max(distributionCurve[["y"]]))
    
    plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n",lwd=2)
    
    
    #draw the main line
    abline(v=GRS_beta,lwd=3)
    
    
    #optionally add real distribution curve
    if(!is.null(distributionCurve)){
      real_x <- distributionCurve[["x"]]
      real_y <- distributionCurve[["y"]]
      adj_y<-real_y# * (ylim[2] / max(real_y))
      lines(x=real_x,y=adj_y,lty=2)
      
    }
    
    legend("topleft",legend=c("User distribution","Your genetic risk score"),lty=c(2,1),lwd=c(2,3),col=c("black","black"))
    
    
  })
  
  
  
})