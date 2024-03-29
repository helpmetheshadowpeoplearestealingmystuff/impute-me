library("shiny")




shinyServer(function(input, output) {
	
  output$text1 <- renderText({ 
    
    if(input$goButton == 0){
      m <- "The overview of rare disease variants found in this module is not the most extensive single-SNP effects available online. They are shown here because they are all well-supported strong genetic effects, for a selection of rare inherited diseases where microarray analysis made sense. This was the reason these SNPs were included in the 2016-version of the 23andme health.<br><br> 

      Especially the last part - that microarray analysis made sense - is very important when analysing the genetics of rare disease; the microarray technology used in consumer genetics is not optimal because the really strong mutations typically are <i>not</i> measured on a microarray. DNA-sequencing is required to detect them. Therefore microarray analysis of rare disease effects has many problems with false negative results. There's a lot of further details to this discussion, chapter 3.5 in <u><a href='https://www.worldscientific.com/worldscibooks/10.1142/11070'>this book</a></u> is a good place to seek more information.<br><br>
      
      Nonetheless, the 2016-selection of microarray-measurable SNPs made by 23andme still is reasonably relevant to report, particularly for the carrier-information. For non-23andme users, this module has the additional benefit of translating the data for proprietary 23andme SNPs, with the caveat that because the SNPs are very rare they are often hard to impute.<br><br>"
      
      
    }else{
      m <- "The table shows the variants which are known to confer a range of severe inherited conditions. They are all fairly rare conditions. Slightly more common, however, is the case of being a carrier of these conditions. Being a carrier means that a person have one copy of a disease-causing allele, but will not be affected because the condition only manifests if both copies are of the disease-causing type.<br><br>"
    }
    
    
    return(m)
  })
  
  
  
	get_table_here <- reactive({
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError("Did not find a user with this id"))
		}
	  
	  
	  
	  #Get vcf-class and abort module if TRUE
	  pDataFile<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
	  is_vcf<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"imputation_type"]=="vcf")
	  if(!is.na(is_vcf) && class(is_vcf)!="try-error" && length(is_vcf) == 1 && is_vcf)stop(safeError("This module has been disabled for users submitting vcf files. That's because vcf files usually are derived from DNA-sequencing, but the investigations made in this module are tailored to microarray-data. Directly analyzing the vcf file itself, outside of impute.me, is likely to be more informative for you in context of the questions asked by this module."))
	  
	  #Get list of rare-variants feasible to analyse with imputed microarrays
		table_file <-paste0(get_conf("code_path"),"rareDiseases/SNPs_to_analyze.txt")
		request <- table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F,comment.char="",quote="")
    
		request<-request[!duplicated(request[,"SNP"]),]
		rownames(request) <- request[,"SNP"]
		genotypes<-get_genotypes(uniqueID=uniqueID,request=request )
		
		iXXXX_na_count <- sum(is.na(genotypes[grep("^i",rownames(genotypes)),1]))
		if(iXXXX_na_count > 10){ #then this is probably not a 23andme array
		  
		  #remove the iXXXX
		  table<-table[grep("^i",table[,"SNP"],invert=T),]
		  table<-table[order(table[,"disease_name"]),]
		  
		  #more intelligible comment
		  table[grep("^original",table[,"comment"]),"comment"] <-"rs-id from original 23andme"
		  
		  #adding genotypes in (many will be missing unfortunately)
		  table[,"Your genotype"]<-genotypes[table[,"SNP"],]
		  
		}else{ #then it is a 23andme array
		  #remove the stand-in stuff
		  table<-table[grep("^stand-in",table[,"comment"],invert=T),]
		  table<-table[order(table[,"disease_name"]),]
		  
		  
		  #more intelligible comment
		  table[,"comment"] <-""
		  
		  
		  #adding genotypes in (many will be missing unfortuntaly)
		  table[,"Your genotype"]<-genotypes[table[,"SNP"],]
		  
		}
		
		
		table[,"First_allele"]<-substr(table[,"Your genotype"],1,1)
		table[,"Second_allele"]<-substr(table[,"Your genotype"],3,3)
		
		table[,"First_carrier"]<-table[,"First_allele"]==table[,"risk_allele"]
		table[,"Second_carrier"]<-table[,"Second_allele"]==table[,"risk_allele"]
		return(table)
		
	})
	
	output$table1 <- DT::renderDataTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		table<-get_table_here()
		
		# diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
		
		table<-table[,c("SNP","Your genotype","risk_allele","non_risk_allele","disease_name","comment")]
		colnames(table)<-c("SNP","Your genotype","Risk-allele","Non-Risk-allele","Inherited Condition","Comment")
		return(table)
		
		
	},options =list(pageLength = 200,searching = FALSE),rownames= FALSE)
	
	output$text2 <- renderText({ 
		
		if(input$goButton == 0){
			return(NULL)
		}
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		table<-get_table_here()
		
		diseases_of_interest <- unique(table[table[,"Second_carrier"] | table[,"First_carrier"],"disease_name"])
		diseases_of_interest<-diseases_of_interest[!is.na(diseases_of_interest)]
		
		if(length(diseases_of_interest)==0){
			m <- "There's no particular inherited conditions that you should pay attention to, according to this analysis"	
		}else if(length(diseases_of_interest)==1){
			m <- paste("According to this analysis, you should pay particular attention to the inherited condition:",diseases_of_interest)
		}else{
			m <- paste("According to this analysis, you should pay particular attention to these",length(diseases_of_interest),"inherited conditions:",paste(diseases_of_interest,collapse=", "))	
		}
		m <- paste(m,".<br>",sep="")

		
		percent_missing<-signif(100 * sum(is.na(table[,"Your genotype"])) / nrow(table),2)
		if(percent_missing > 50){
		  m<-paste0(m, "<br>Note however, that ",percent_missing,"% of the queried SNPs were unavailable in your data, <i>even</i> after imputation. This is because these rare-disease causing SNPs are difficult to impute, and only the microarray-types from 23andme have custom modifications in order to measure them.<br>")
		  
		}
		
		#write the query to the log file
		log_function<-function(uniqueID,diseases_of_interest){
			user_log_file<-paste(get_conf("data_path"),uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"rareDiseases",uniqueID,paste(diseases_of_interest,collapse=";"))
			m<-paste(m,collapse="\t")
			if(file.exists(user_log_file)){
				write(m,file=user_log_file,append=TRUE)
			}else{
				write(m,file=user_log_file,append=FALSE)
			}
		}
		try(log_function(uniqueID,diseases_of_interest))
		
				
		return(m)
	})
	

	
		
	
	
})





