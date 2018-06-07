library("shiny")


source("/home/ubuntu/srv/impute-me/functions.R")
# source("../functions.R")


table_file <-"/home/ubuntu/srv/impute-me/precisionMedicine/SNPs_to_analyze.txt"
# table_file <-"SNPs_to_analyze.txt"



# Define server logic for random distribution application
shinyServer(function(input, output) {
	
  
  
  
  output$ui <- renderUI({
    # uniqueID <- gsub(" ","",input$uniqueID)
    disease <- input$disease
    blank <- selectInput("drug", "Drug:", choices = c())
    SNPs_to_analyze<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
    if(!disease%in%SNPs_to_analyze[,"disease"]){
      return(blank)
    }else{
      drugs <- sort(unique(SNPs_to_analyze[SNPs_to_analyze[,"disease"]%in%disease,"drug"]))
      out <- selectInput("drug", "Drug:", drugs)
      return(out)
    }
  })
  
  
  
  
  
  get_data <- reactive({
    
    #initial UI data gathering and user-check
    uniqueID<-gsub(" ","",input$uniqueID)
    disease <- input$disease
    drug <- input$drug
    
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError(paste("Did not find a user with this id",uniqueID)))
    }
    
    SNPs_to_analyze<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
    
    #subset to only relevant
    SNPs_to_analyze<-SNPs_to_analyze[SNPs_to_analyze[,"disease"]%in%disease & SNPs_to_analyze[,"drug"]%in%drug,]
    if(nrow(SNPs_to_analyze)==0)stop(safeError("No SNPs found for this disease-drug combination"))
    
    
    
    
    #retrieving SNPs
    SNPs_to_retrieve<-SNPs_to_analyze
    SNPs_to_retrieve<-SNPs_to_retrieve[!duplicated(SNPs_to_retrieve[,"SNP"]),]
    rownames(SNPs_to_retrieve) <- SNPs_to_retrieve[,"SNP"]
    SNPs_to_retrieve<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_retrieve)

    
    #inserting SNPs and calculating GRS
    SNPs_to_analyze[,"genotype"] <- SNPs_to_retrieve[SNPs_to_analyze[,"SNP"],"genotype"]

  
    
    #write the query to the log file
    log_function<-function(uniqueID){
      user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"drug_response",uniqueID)
      m<-paste(m,collapse="\t")
      if(file.exists(user_log_file)){
        write(m,file=user_log_file,append=TRUE)
      }else{
        write(m,file=user_log_file,append=FALSE)
      }
    }
    try(log_function(uniqueID))
    
    
    
    return(SNPs_to_analyze)
  })
  
  
  output$table1 <- renderTable({ 
    if(input$goButton == 0){
      return(NULL)
    }
    d1<-get_data()
    
    per_study <- data.frame(row.names=unique(d1[,"PMID"]))
    
    for(study in rownames(per_study)){
      
      d2<-d1[d1[,"PMID"]%in%study,]
      
      per_study[study,"disease"] <- unique(d2[,"disease"])
      per_study[study,"drug"] <- unique(d2[,"drug"])
      per_study[study,"study"] <- paste0("<u><a href='https://www.ncbi.nlm.nih.gov/pubmed/",study,"'>Study-link</a></u>")
      
      rownames(d2)<-d2[,"SNP"]
      d3<-try(get_GRS_2(d2, mean_scale=T, unit_variance=T, verbose=T))
      if(class(d3)=="try-error"){
        per_study[study,"Z-score"] <- "Not calculated"
        per_study[study,"Percentage"] <- "Not calculated"
        
      }else{
        population_sum_sd<-sqrt(sum(d3[,"population_score_sd"]^2,na.rm=T))
        if(population_sum_sd == 0)stop(safeError("It was not possible to calculate polygenic summarize scores for this trait. Single-SNP analysis is given in table below."))
        GRS <-sum(d3[,"score_diff"],na.rm=T) / population_sum_sd
        percentage<-floor(pnorm(GRS,mean=0,sd=1)*100)
        per_study[study,"Z-score"] <- signif(GRS,2)
        per_study[study,"Percentage"] <- signif(percentage,2)
        
      }
      
      
      
    }
    
    rownames(per_study) <- NULL
    
    return(per_study)
  }, sanitize.text.function = function(x) x) 
  
  
	
	
	
	output$table2 <- renderTable({ 
	  if(input$goButton == 0){
	    return(NULL)
	  }
	  table<-get_data()
	  table[,"minor/major allele"]<-apply(table[,c("minor_allele", "major_allele")],1,paste,collapse="/")
	  table[,"effect/alternative allele"]<-apply(table[,c("effect_allele", "non_effect_allele")],1,paste,collapse="/")
	  

	  order <- c( "SNP", "genotype" ,  "effect/alternative allele", "effect_size",    "effect_direction", "effect_measure", "minor/major allele","minor_allele_freq","gene","PMID" )
	  
	  missing <- order[!order%in%colnames(table)]
	  if(length(missing)>0)stop(safeError("Missing some columns"))
	  
	  table[,"minor_allele_freq"] <- signif(table[,"minor_allele_freq"],2)
	  table[,"effect_size"] <- signif(table[,"effect_size"],2)
	  
	  table<-table[,order]

	  return(table)
	  
	},include.rownames = FALSE)
	
})