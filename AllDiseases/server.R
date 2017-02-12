library("shiny")

source("/srv/shiny-server/gene-surfer/functions.R")
dataFolder<-"/home/ubuntu/data/"
snps_file<-"/srv/shiny-server/gene-surfer/AllDiseases/2017-02-12_semi_curated_version_gwas_central.rdata"
trait_file<-"/srv/shiny-server/gene-surfer/AllDiseases/2017-02-12_trait_overoverview.rdata"


#testing

# snps_file<-"AllDiseases/2017-02-12_semi_curated_version_gwas_central.rdata"
# trait_file<-"AllDiseases/2017-02-12_trait_overoverview.rdata"
load(snps_file)

shinyServer(function(input, output) {
	
	output$text_1 <- renderText({ 
		
		if(input$goButton == 0){
			trait_pmid<-isolate(input$trait)
			uniqueID<-isolate(input$uniqueID)
			m<-paste0("A genetic risk score is an arbitrary value that gives a summary of a large number of different SNPs each of which contribute a little to disease risk. The higher the value, the higher the risk of developing disease. More details of its interpretation, calculation and limitations can be found in the <u><a href='http://www.impute.me/autoimmuneDiseases/'>specialized trait GWAS modules</a></u>"
			)
			
		}else{
			m<-""
		}
		return(m)
	})
	
	
	output$text_2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			trait_pmid<-isolate(input$trait)
			uniqueID<-isolate(input$uniqueID)
			m<-"Further description"			
			
		}
		return(m)
	})
	
	get_data <- reactive({
		trait_pmid<-isolate(input$trait)
		uniqueID<-isolate(input$uniqueID)
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop("Did not find a user with this id")
		}
		trait<-strsplit(trait_pmid," // ")[[1]][1]
		pmid<-strsplit(trait_pmid," // ")[[1]][2]
		if(!pmid%in%data[,"PUBMEDID"])stop(paste("PMID",pmid,"was not found in system"))
		if(!trait%in%data[,"DISEASE.TRAIT"])stop(paste("trait",trait,"was not found"))
		
		
		SNPs_to_analyze<-data[data[,"PUBMEDID"]%in%pmid & data[,"DISEASE.TRAIT"]%in%trait ,]
		
		
		link<-unique(SNPs_to_analyze[,"LINK"])
		if(length(link)!=1)stop("Problem with link length")
		author<-unique(SNPs_to_analyze[,"FIRST.AUTHOR"])
		if(length(author)!=1)stop("Problem with author length")
		
		textToReturn <- paste0("Retrieved ",nrow(SNPs_to_analyze)," SNPs from <u><a href='",link,"'>",author," et al (PMID ",pmid,")</a></u>, which were reported to be associated with ",trait,".")
		
		
		
		#if any of the SNPs are duplicated we have to merge them (this by the way is an odd situation
		#why would a SNP be listed twice in the results for the same trait - let's aim to merge only in GRS
		#but list all in table for the sake of transparency)
		if(any(duplicated(SNPs_to_analyze[,"SNP"]))){
			SNPs_requested<-SNPs_to_analyze[!duplicated(SNPs_to_analyze[,"SNP"]),]
			# SNPs_requested<-SNPs_requested[,c("SNP","chr_name")]
			rownames(SNPs_requested)<-SNPs_requested[,"SNP"]
			genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_requested, namingLabel="cached.all_gwas")
			genotypes[,"GRS"] <-get_GRS_2(genotypes=genotypes,betas=SNPs_requested)
			
			#Handle the duplications and make a proper warning
			duplicates<-sort(unique(SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"SNP"]))
			warnForDiscrepancyInBeta<-FALSE
			for(duplicate in duplicates){
				s1<-SNPs_to_analyze[SNPs_to_analyze[,"SNP"]%in%duplicate,]
				if(
					length(unique(s1[,"effect_allele"]))!=1|
					length(unique(s1[,"non_effect_allele"]))!=1|
					length(unique(s1[,"Beta"]))!=1){
					warnForDiscrepancyInBeta<-TRUE
				}
			}
			duplicates_example<-paste(duplicates[1:min(c(5,length(duplicates)))],collapse=", ")			
			if(warnForDiscrepancyInBeta){
				textToReturn <- paste0(textToReturn," Note ",length(duplicate)," SNPs were entered twice for this GWAS, and the effect-size and direction was not consistent. An arbitrary choice was made, but please check the results table carefully for details (",duplicates_example,").")
			}else{
				textToReturn <- paste0(textToReturn," Note ",length(duplicate)," SNPs were entered twice for this GWAS, but the effect-size and direction was consistent (",duplicates_example,").")
			}
		}else{
			genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze, namingLabel="cached.all_gwas")
			genotypes[,"GRS"] <-get_GRS_2(genotypes=genotypes,betas=SNPs_to_analyze)
			
		}
		
		
		
		return(list(
			SNPs_to_analyze=SNPs_to_analyze,
			genotypes=genotypes))
				 
		
		
		
	})

	
	
		
	output$plot_1 <- renderPlot({ 
		SNPs_to_analyze<-get_data()
		
		
		
		control_mean<-0
		control_sd<-1
		xlim<-c(control_mean - control_sd*3, control_mean + control_sd*3)
		
		x<-seq(xlim[1],xlim[2],length.out=100)
		y_control<-dnorm(x,mean=control_mean,sd=control_sd)
		plot(x,y_control,type="l",col="blue",ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n",lwd=2)
		
		
		#fill the controls
		if(all(!x<GRS_beta))stop("GRS too low to plot")
		max_GRS_i<-max(which(x<GRS_beta))
		upper_x<-x[1:max_GRS_i]
		upper_y<-y_control[1:max_GRS_i]
		x_lines <- c(upper_x,GRS_beta,GRS_beta,xlim[1])
		y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
		polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
		
		#add control text
		prop<-signif(pnorm(GRS_beta,mean=control_mean,sd=control_sd),2)
		x_text<-upper_x[round(length(upper_x)/2)]
		y_text<-upper_y[round(length(upper_y)/2)] / 2
		text(x_text,y_text,paste0(prop*100,"%"),col="blue")
		
		
		
		#draw the main line
		abline(v=GRS_beta,lwd=3)
		
		
		
		
		
		legend("topleft",legend=c("Population distribution","Your genetic risk score"),lty=c(1,1),lwd=c(2,3),col=c("blue","black"))
		
		
	})
	
	
	
	
	
	
})


