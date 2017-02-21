library("shiny")

source("/home/ubuntu/srv/impute-me/functions.R")
dataFolder<-"/home/ubuntu/data/"
snps_file<-"/home/ubuntu/srv/impute-me/AllDiseases/2017-02-12_semi_curated_version_gwas_central.rdata"
trait_file<-"/home/ubuntu/srv/impute-me/AllDiseases/2017-02-12_trait_overoverview.rdata"


#testing

# snps_file<-"AllDiseases/2017-02-12_semi_curated_version_gwas_central.rdata"
# trait_file<-"AllDiseases/2017-02-12_trait_overoverview.rdata"


#preload
load(snps_file)
load(trait_file)

shinyServer(function(input, output) {
	
	output$text_1 <- renderText({ 
		
		if(input$goButton == 0){
			m<-paste0("A genetic risk score is an arbitrary value that gives a summary of a large number of different SNPs each of which contribute a little to disease risk. The higher the value, the higher the risk of developing disease. More details of its interpretation, calculation and limitations can be found in the specialized trait GWAS modules on <u><a href='http://www.impute.me/autoimmuneDiseases/'>autoimmune diseases</a></u> or <u><a href='http://www.impute.me/leukemia/'>leukemia</a></u>, with the added caveat that only automated curation have been performed on these thousands of additional studies.<br><br>"
			)
			
		}else{
			m<-""
		}
		return(m)
	})
	
	
	
	get_data <- reactive({
		
		#initial UI data gathering and user-check
		study_id<-input$trait
		uniqueID<-input$uniqueID
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(paste("Did not find a user with this id",uniqueID))
		}
		
		
		#getting the relevant trait name, pmid and SNPs to analyze
		trait<-traits[study_id,"trait"]
		pmid<-traits[study_id,"PMID"]
		if(!pmid%in%data[,"PUBMEDID"])stop(paste("PMID",pmid,"was not found in system"))
		if(!trait%in%data[,"DISEASE.TRAIT"])stop(paste("trait",trait,"was not found"))
		SNPs_to_analyze<-data[data[,"study_id"]%in%study_id ,]
		
		
		#gathering some background info for the study		
		link<-unique(SNPs_to_analyze[,"LINK"])
		if(length(link)!=1)stop("Problem with link length")
		author<-unique(SNPs_to_analyze[,"FIRST.AUTHOR"])
		if(length(author)!=1)stop("Problem with author length")
		sampleSize<-unique(SNPs_to_analyze[,"sampleSize"])
		if(length(sampleSize)!=1){
			print("Problem with sampleSize length - but just took mean")
			sampleSize <- round(mean(sampleSize,na.rm=T))
		}
		DATE<-unique(SNPs_to_analyze[,"DATE"])
		if(length(DATE)!=1)stop("Problem with DATE length")
		textToReturn <- paste0("Retrieved ",nrow(SNPs_to_analyze)," SNPs from <u><a href='http://",link,"'>",author," et al (PMID ",pmid,")</a></u>, which were reported to be associated with ",trait,". We highly recommend you to check the original study for further details.")
		textToReturn <- paste0(textToReturn," This study reports a total sample size of ",sampleSize,", as entered on date ",DATE,".")
		
		
		
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
				textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNPs were entered twice for this GWAS, and the effect-size and direction was <b>not consistent</b>. An arbitrary choice was made, but please check the results table carefully for details (",duplicates_example,").")
			}else{
				textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNPs were entered twice for this GWAS, but the effect-size and direction was consistent (",duplicates_example,").")
			}
		}else{
			
			#if no duplicates this is the much simpler solution, and the above 30 lines would be unnecessary
			rownames(SNPs_to_analyze)<-SNPs_to_analyze[,"SNP"]
			genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze, namingLabel="cached.all_gwas")
			genotypes[,"GRS"] <-get_GRS_2(genotypes=genotypes,betas=SNPs_to_analyze)
			
		}
		
		
		#write the score to the log file
		log_function<-function(uniqueID,study_id,genotypes){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"AllDisease",uniqueID,study_id,signif(mean(genotypes[,"GRS"],na.rm=T),3))
			m<-paste(m,collapse="\t")
			if(file.exists(user_log_file)){
				write(m,file=user_log_file,append=TRUE)
			}else{
				write(m,file=user_log_file,append=FALSE)
			}
		}
		try(log_function(uniqueID,study_id,genotypes))
		
		#then return the overall list		
		return(list(
			SNPs_to_analyze=SNPs_to_analyze,
			genotypes=genotypes,
			textToReturn=textToReturn))
		
		
		
		
	})
	
	
	
	
	output$plot_1 <- renderPlot({ 
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			o<-get_data()
			SNPs_to_analyze<-o[["SNPs_to_analyze"]]
			genotypes<-o[["genotypes"]]
			
			GRS_beta<-mean(genotypes[,"GRS"],na.rm=T)
			
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
			
		}		
	})
	
	
	output$text_2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			o<-get_data()
			m<-paste0("<br><br>",o[["textToReturn"]],"<br><br>")
			
		}
		return(m)
	})
	
	
	output$table1 <- renderDataTable({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			
			#getting data
			o<-get_data()
			SNPs_to_analyze<-o[["SNPs_to_analyze"]]
			genotypes<-o[["genotypes"]]
			
			#summarising allele info into single-columns
			SNPs_to_analyze[,"Risk/non-risk Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
			SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
			
			# col_to_remove<-c("DATE","sampleSize","ensembl_alleles","LINK","FIRST.AUTHOR","PUBMEDID","chr_name","CHR_POS","effect_allele","non_effect_allele","major_allele","minor_allele","DISEASE.TRAIT")
			# for(col in col_to_remove){SNPs_to_analyze[,col]<-NULL}
			
			
			#adding genotype GRS and rounding MAF
			SNPs_to_analyze[,"Your Genotype"]<-genotypes[SNPs_to_analyze[,"SNP"],"genotype"]
			SNPs_to_analyze[,"GRS"]<-signif(genotypes[SNPs_to_analyze[,"SNP"],"GRS"],2)
			SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
			
			
			#shortening the reported gene count
			SNPs_to_analyze[,"Reported Gene"]<-sapply(strsplit(SNPs_to_analyze[,"REPORTED.GENE.S."],", "),function(x){
				paste(x[1:min(c(2,length(x)))],collapse=", ")
			})
			
			keep<-c("SNP","REGION","Your Genotype","Risk/non-risk Allele","GRS","Beta","P.VALUE","Major/minor Allele","minor_allele_freq","Reported Gene")
			SNPs_to_analyze<-SNPs_to_analyze[,keep]
			colnames(SNPs_to_analyze)<-c("SNP","Location","Your Genotype","Risk/ non-risk Allele","Your GRS (this SNP)","Effect Size","P-value","Major/ minor Allele","Minor Allele Frequency","Reported Gene")
			
			return(SNPs_to_analyze)
		}
	},options = list(searching = FALSE,paging = FALSE))

	
	output$text_3 <- renderText({ 
		
		if(input$goButton == 0){
			m<-""
			
			
		}else{
			m<-paste0("<small><br><b>Methods</b><br>GWAS data was downloaded from <u><a href='http://www.gwascentral.org/'>GWAS central</a></u>, curated as further described in the paper by <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=24301061'>Beck et al</a></u>. First a per-SNP genetic risk score (GRS) is calculated by counting the risk-alleles multiplied by the effect size (OR or beta as reported in original paper). However, before multiplying with effect size, the GRS is scaled to mean-center and unit-variance relative to the population distribution of possible genotypes. This effectively makes the scores <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-scores</a></u>. This is done using the minor-allele frequency (MAF) for each SNP, as taken from the 1000 genomes project v3. If a person is homozygote for a very rare risk variant this will result in a very high GRS Z-score, conversely of the SNP is common the Z-score will be less extreme. The per-trait GRS is then simply the mean of all per-SNP GRS. Further details of the calculation can be found in the <u><a href='https://github.com/lassefolkersen/impute-me/blob/6c05cef7a182c895fa88cd741e4d529e4bfb8200/functions.R#L1023-L1172'>source code</a></u>. The advantage of this approach is that it does not require further data input than MAF, effect-size and genotype. The disadvantage is that particularly ethnicity-specific genetics may be skewed, according to differences in MAF. This is however anyway the case for the GWAS reported risk-SNPs and effect sizes to begin with, and it is always recommended to check the original study to see what ethnicity was investigated. Another potential issue is that in some cases the term genetic <i>risk</i> score may be problematic. This is for example the case for GWAS of biological quantities were it is not clear if higher values are <i>more</i> risk-related, e.g. HDL-cholesterol or vitamin-levels. Again it is recommended to consult with the original GWAS publication.</small>")
								
		}
		return(m)
	})
	
		
})


