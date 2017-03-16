library("shiny")

source("/home/ubuntu/srv/impute-me/functions.R")
dataFolder<-"/home/ubuntu/data/"
snps_file<-"/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_semi_curated_version_gwas_central.rdata"
trait_file<-"/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_trait_overoverview.rdata"


#testing

# snps_file<-"AllDiseases/2017-02-21_semi_curated_version_gwas_central.rdata"
# trait_file<-"AllDiseases/2017-02-21_trait_overoverview.rdata"


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
		
		#rename beta to effect_size (should probably be removed later)
		colnames(SNPs_to_analyze)[colnames(SNPs_to_analyze)%in%"Beta"]<-"effect_size"
		
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
		textToReturn <- paste0("Retrieved ",nrow(SNPs_to_analyze)," SNPs from <u><a href='http://",link,"'>",author," et al (PMID ",pmid,")</a></u>, which were reported to be associated with ",trait,".")
		textToReturn <- paste0(textToReturn," This study reports a total sample size of ",sampleSize,", as entered on date ",DATE,".")
		
		
		
		#if any of the SNPs are duplicated we have to merge them (this by the way is an odd situation
		#why would a SNP be listed twice in the results for the same trait - let's aim to merge only in GRS
		#but list all in table for the sake of transparency)
		if(any(duplicated(SNPs_to_analyze[,"SNP"]))){
			#Handle the duplications and make a proper warning
			duplicates<-sort(unique(SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"SNP"]))
			warnForDiscrepancyInBeta<-FALSE
			for(duplicate in duplicates){
				s1<-SNPs_to_analyze[SNPs_to_analyze[,"SNP"]%in%duplicate,]
				if(
					length(unique(s1[,"effect_allele"]))!=1|
					length(unique(s1[,"non_effect_allele"]))!=1|
					length(unique(s1[,"effect_size"]))!=1){
					warnForDiscrepancyInBeta<-TRUE
				}
			}
			duplicates_example<-paste(duplicates[1:min(c(5,length(duplicates)))],collapse=", ")			
			if(warnForDiscrepancyInBeta){
				textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNP(s) were entered twice for this GWAS, and the effect-size and direction was <b>not consistent</b>. The beta from the first listed SNP was used, but please cross-check the results table with the original study carefully for details (e.g. ",duplicates_example,"). Duplicates are indicated in the end of table but don't contribute to results.")
			}else{
				textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNP(s) were entered twice for this GWAS, but the effect-size and direction was consistent (e.g. ",duplicates_example,"). Duplicates are indicated in the end of table but don't contribute to results.")
			}
			
			#then we handle it so that the extra lines are removed and inserted in the end instead
			SNPs_to_analyze_duplicates<-SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),]
			rownames(SNPs_to_analyze_duplicates) <- paste0("duplicate_",1:nrow(SNPs_to_analyze_duplicates))
			for(col in c("genotype","personal_score","population_score_average","population_score_sd","score_diff")){
			  SNPs_to_analyze_duplicates[,col]<-NA
			}
			SNPs_to_analyze<-SNPs_to_analyze[!duplicated(SNPs_to_analyze[,"SNP"]),]
		}else{
		  SNPs_to_analyze_duplicates<-SNPs_to_analyze[0,]
		}
		
			
		rownames(SNPs_to_analyze)<-SNPs_to_analyze[,"SNP"]
		genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze, namingLabel="cached.all_gwas")
		SNPs_to_analyze[,"genotype"] <- genotypes[rownames(SNPs_to_analyze),"genotype"]
		SNPs_to_analyze <-get_GRS_2(SNPs_to_analyze,mean_scale=T, unit_variance=T, verbose=T)
		population_sum_sd<-sqrt(sum(SNPs_to_analyze[,"population_score_sd"]^2,na.rm=T))
		GRS <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T) / population_sum_sd
		
		
		
		
		#check for question marks in risk-allele
		c1<-apply(SNPs_to_analyze[,c("minor_allele","major_allele","effect_allele","non_effect_allele")]=="?",1,sum)
		if(sum(c1>0)){
		  textToReturn <- paste0(textToReturn," Also note that ",sum(c1>0)," SNP(s) had <b>missing or discrepant</b> allele information, meaning that risk-allele or minor/major allele could not be correctly assigned. This is indicated with a '?' in the results table and causes the SNP to be omitted from the GRS-calculation. This is likely due to strand-reporting issues, and may be fixable by checking the original study.")
		}
		
		
		
		#add the overall population SD value
		textToReturn <- paste0(textToReturn," The population-wide standard deviation of this GRS was calculated to be ",signif(population_sum_sd,2)," which is taken into account when arriving at a trait GRS Z-score of ",signif(GRS,2),".")
		
		
		
		#add the final summary
		percentage<-signif(pnorm(GRS_beta,mean=control_mean,sd=control_sd),2)*100
		if(percentage < 33.3){
		  summary <- " This is a low score."
		}else if(percentage > 66.6){
		  summary <- " This is a high score."
		}else{
		  summary <- " This fairly average score."
		}
		textToReturn <- paste0(textToReturn," This means that <b>your genetic risk score for this trait will be higher than ",percentage,"% of the general population</>.",summary)
		
		
		#add in the (damn) duplicates
		SNPs_to_analyze<-rbind(SNPs_to_analyze,SNPs_to_analyze_duplicates)
		
		
		#write the score to the log file
		log_function<-function(uniqueID,study_id,genotypes){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"AllDisease",uniqueID,study_id,GRS)
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
			textToReturn=textToReturn,
			GRS=GRS))
	})
	
	
	
	
	output$plot_1 <- renderPlot({ 
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			o<-get_data()
			SNPs_to_analyze<-o[["SNPs_to_analyze"]]
			GRS_beta<-o[["GRS"]]
			
			if(is.na(GRS_beta))stop("Could not calculate overall GRS because all SNPs in the signature were missing information about either risk-allele, effect-size or minor-allele-frequency.")
			
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
			GRS<-o[["GRS"]]
			
			#summarising allele info into single-columns
			SNPs_to_analyze[,"Risk/non-risk Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
			SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
			
			# col_to_remove<-c("DATE","sampleSize","ensembl_alleles","LINK","FIRST.AUTHOR","PUBMEDID","chr_name","CHR_POS","effect_allele","non_effect_allele","major_allele","minor_allele","DISEASE.TRAIT")
			# for(col in col_to_remove){SNPs_to_analyze[,col]<-NULL}
			
			
			
			
			#rounding MAF
			SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
			
			#removing duplicate GRS
			# SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"GRS"]<-""
			
			#shortening the reported gene count
			SNPs_to_analyze[,"Reported Gene"]<-sapply(strsplit(SNPs_to_analyze[,"REPORTED.GENE.S."],", "),function(x){
				paste(x[1:min(c(2,length(x)))],collapse=", ")
			})
			
			
			#marking duplicates
			for(col in c("genotype","personal_score","score_diff")){
			  SNPs_to_analyze[is.na(SNPs_to_analyze[,col]),col] <- ""
			}
			
			
			keep<-c("SNP","REGION","genotype","Risk/non-risk Allele","personal_score","score_diff"
,"effect_size","P.VALUE","Major/minor Allele","minor_allele_freq","Reported Gene")
			SNPs_to_analyze<-SNPs_to_analyze[,keep]
			colnames(SNPs_to_analyze)<-c("SNP","Location","Your Genotype","Risk/ non-risk Allele","SNP-score","SNP-score (population normalized)","Effect Size","P-value","Major/ minor Allele","Minor Allele Frequency","Reported Gene")
			
			return(SNPs_to_analyze)
		}
	},options = list(searching = FALSE,paging = FALSE))

	
	output$text_3 <- renderText({ 
		
		if(input$goButton == 0){
			m<-""
			
			
		}else{
			m<-paste0("<small><br><b>Methods</b><br>GWAS data was downloaded from <u><a href='http://www.gwascentral.org/'>GWAS central</a></u>, curated as further described in the paper by <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=24301061'>Beck et al</a></u>. Then a per-SNP score was calculated by counting the risk-alleles multiplied by the effect size (OR or beta as reported in original paper). This was centered so that the average score in the general population would be zero ('population normalized'). This means, that if a person is homozygote for a very rare risk variant this will result in a very high Z-score, conversely if the SNP is common, the Z-score will be less extreme. The sum of these normalized SNP-scores calculated to get a trait-wide genetic risk score (GRS). This GRS was additionally scaled so that standard-deviation in the general population is 1 (unit-variance), effectively making the scores <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-scores</a></u>. All scaling was done using the minor-allele frequency (MAF) for each SNP, as taken from the 1000 genomes project v3. Further details of the calculation can be found in the <u><a href='https://github.com/lassefolkersen/impute-me/blob/56813bf071d7fa4c0a658c90d2ebee196a781e8a/functions.R#L1166-L1326'>source code</a></u>. The advantage of this approach is that it does not require further data input than MAF, effect-size and genotype. The disadvantage is that particularly ethnicity-specific genetics may be skewed, according to differences in MAF. This is however anyway the case for the GWAS reported risk-SNPs and effect sizes to begin with, and it is always recommended to check the original study to see what ethnicity was investigated. Another potential issue is that in some cases the term genetic <i>risk</i> score may be problematic. This is for example the case for GWAS of biological quantities were it is not clear if higher values are <i>more</i> risk-related, e.g. HDL-cholesterol or vitamin-levels. Again it is recommended to consult with the original GWAS publication. Also check out the <u><a href='http://www.impute.me/HeuristicHealth'>HeuresticHealth-module</a></u> under development - based on this info, but without having to scroll through all entries</small>")
								
		}
		return(m)
	})
	
		
})


