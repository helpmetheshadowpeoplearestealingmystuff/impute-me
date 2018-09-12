library("shiny")
source("/home/ubuntu/srv/impute-me/functions.R")



#path for pre-calculated LD-pred scores
ldpred_path <- "/home/ubuntu/ldpred/DEPSYM_2016_OKBAY.EurUnrel.hapmap3.all.ldpred.effects"


# Define server logic for a template
shinyServer(function(input, output) {
	output$table1 <- renderTable({ 
		if(input$goButton == 0){
			return(NULL)
		}
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError("Did not find a user with this id"))
		}

	  
	  
		
		#instead of the usual R-based extraction routine, we try to use the plink score function together with the pre-calculated LD-pred scores.
	  weighting_scheme <- input$weighting_scheme
	  
	  all_weights <- read.table(ldpred_path)
    all_weights[,"beta"] <- all_weights[,weighting_scheme]
	  
	  
		genotypes<-get_genotypes(uniqueID=uniqueID,request=all_weights)
		
		
		# skip the usual population weighting, and try the simpler version first (should anyway fail for anything but EUR, per Curtis 2018)
		# all_weights <-get_GRS_2(all_weights,mean_scale=T, unit_variance=T, verbose=T)
		# population_sum_sd<-sqrt(sum(SNPs_to_analyze[,"population_score_sd"]^2,na.rm=T))
		# if(population_sum_sd == 0)stop(safeError("For some reason we couldn't analyse this particular trait from your genomic data."))
		# GRS <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T) / population_sum_sd
		
		all_weights <-get_GRS_2(all_weights,mean_scale=F, unit_variance=F, verbose=T)
		

		GRS <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T)
		
		table <- data.frame(LDpred_score = GRS)
		
		
		return(table)
		
	},include.rownames = FALSE)
})