library("shiny")
library("jsonlite")
source("/home/ubuntu/srv/impute-me/functions.R")





# Define server logic for a template
shinyServer(function(input, output) {
  output$text <- renderText({ 
    if(input$goButton == 0){
      message <- "<br>Polygenic risk scores essentially means the weighted sum of effects from many SNPs. How they are calculated is a matter of great debate in current genetics. This module explores their calculation using the so-called <u><a href='https://github.com/bvilhjal/ldpred'>LD-pred</a></u> algorithm. The main difference between this and the current <u><a href='https://www.impute.me/AllDiseases/'>Complex Disease</a></u> module is that virtually all measured SNPs in the entire genome are included in the calculation, not just those that are genome-wide significant.<br><br>"
    }else if(input$goButton > 0) {
      message <- "<br>Since the module is highly experimental, the current reporting basically is the LD-pred score output as is, along with QC information on number of included SNPs. This means that the score currently only can be interpreted on a relative scale, i.e. you can upload two samples and see which one is higher.<br><br>
      Obviously this is an area of very active development, and we aim to provide a setup wherein scores are reported within a framework of risk relative to e.g. large population segments or similar.<br><br>"
    }
    return(message)
  })
  
  
  
  
  output$table1 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
    trait_choice <- input$trait_choice
    uniqueID<-gsub(" ","",input$uniqueID)

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
		if(!trait_choice %in% names(d[["prs"]]))stop(safeError("Didn't find a PRS-entry for this trait in your calculations. Submission was probably made prior to implementation. Try another trait or re-upload data."))

	  print("checks ok")
	  
	  #extract relevant data
	  d1 <- d[["prs"]][[trait_choice]]
	  
	  #temp-fix because of 2019-01-11-refactoring (can be removed later)
	  if(any(c("total_snps","contributing_snps")%in%names(d1))){
	    names(d1)[names(d1)%in%"contributing_snps"]<-"alleles_checked"
	    names(d1)[names(d1)%in%"total_snps"]<-"alleles_observed"
	  }
	    
	  #create basic result table
	  table <- data.frame(
	    "Polygenic Risk Score"=signif(d1[["GRS"]],3),
	    "Alleles checked (plink CNT)"=d1[["alleles_checked"]],
	    "Alleles observed (plink CNT2)"=d1[["alleles_observed"]],
	    check.names=F
	  )

	  print(table)
		
		return(table)
		
	},include.rownames = FALSE)
})