library("shiny")

source("/home/ubuntu/srv/impute-me/functions.R")


shinyServer(function(input, output) {
	
	output$text_1 <- renderText({ 
		
		if(input$goButton == 0){
			m<-paste0("An interesting study of political opinion was performed by <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/24569950'>Hatemi et al</a></u> in 2014. While the authors themselves conclude that results are too weak to be formally significant, it can provide the basis of an interesting self-check on genetics of politics.<br><br>"
			)
			
		}else{
			m<-""
		}
		return(m)
	})
	
	
	
	get_data <- reactive({
		
		#initial UI data gathering and user-check
		uniqueID<-input$uniqueID
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(paste("Did not find a user with this id",uniqueID))
		}
		
		
		#getting the relevant trait name, pmid and SNPs to analyze
		SNPs_to_analyze<-read.table("opinions/SNPs_to_analyze.txt",sep="\t",stringsAsFactors = F,row.names=1)
		
		genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
		genotypes[,"GRS"] <-get_GRS_2(genotypes=genotypes,betas=SNPs_to_analyze)
		
		
		
		#write the score to the log file
		log_function<-function(uniqueID,study_id,genotypes){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"opinions",uniqueID,signif(mean(genotypes[,"GRS"],na.rm=T),3))
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
			genotypes=genotypes
			))
	})
	
	
	
	
	output$plot_1 <- renderPlot({ 
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			o<-get_data()
			SNPs_to_analyze<-o[["SNPs_to_analyze"]]
			genotypes<-o[["genotypes"]]
			
			GRS_beta<-mean(genotypes[,"GRS"],na.rm=T)
			
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
			
			legend("topleft",legend=c("Population distribution","Your genetic opinion score"),lty=c(1,1),lwd=c(2,3),col=c("blue","black"))
			
		}		
	})
	
	
	output$text_2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			m<-"some text about the results"
			
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
			
			#adding genotype GRS and rounding MAF
			SNPs_to_analyze[,"Your Genotype"]<-genotypes[SNPs_to_analyze[,"SNP"],"genotype"]
			SNPs_to_analyze[,"GRS"]<-signif(genotypes[SNPs_to_analyze[,"SNP"],"GRS"],2)
			SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
			
			# keep<-c("SNP","REGION","Your Genotype","Risk/non-risk Allele","GRS","Beta","P.VALUE","Major/minor Allele","minor_allele_freq","Reported Gene")
			# SNPs_to_analyze<-SNPs_to_analyze[,keep]
			# colnames(SNPs_to_analyze)<-c("SNP","Location","Your Genotype","Risk/ non-risk Allele","Your GRS (this SNP)","Effect Size","P-value","Major/ minor Allele","Minor Allele Frequency","Reported Gene")
			
			return(SNPs_to_analyze)
		}
	},options = list(searching = FALSE,paging = FALSE))

	
	output$text_3 <- renderText({ 
		
		if(input$goButton == 0){
			m<-""
			
			
		}else{
			m<-paste0("<small><br><b>Methods</b><br>The genetics risk score was calculated using the data found in <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/24569950'>Hatemi et al</a></u>. The method used is identical to the one describes for the <u><a href='http://www.impute.me/AllDiseases/'>GWAS calculator</a></u>.")
								
		}
		return(m)
	})
	
		
})


