library("shiny")

#REMOVE LATER
# rm(list=ls())
# source("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/functions_local.R")
# uniqueID <- "id_57n662948"
# disease<-"RA"
# SNPs_to_analyze_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/2016-05-21_SNPs_to_analyze_SOURCE.txt"
# means_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/2016-05-21_means.txt"


# 
SNPs_to_analyze_file<-paste0(get_conf("code_path"),"autoimmuneDiseases/2016-05-21_SNPs_to_analyze_SOURCE.txt")
means_file<-paste0(get_conf("code_path"),"autoimmuneDiseases/2016-05-21_means.txt")



diseaseNames<-rbind(
	c("RA","Rheumatoid Arthritis","okada"),
	c("UC","Ulcerative colitis","ellinghaus"),
	c("CD","Crohn’s disease","ellinghaus"),
	c("PS","Psoriasis","ellinghaus"),
	c("PSC","Primary Sclerosing Cholangitis","ellinghaus"),
	c("AS","Ankylosing Spondylitis","ellinghaus")
)
colnames(diseaseNames)<-c("Acronym","Disease","Source")
rownames(diseaseNames)<-diseaseNames[,"Acronym"]



shinyServer(function(input, output) {
	
	output$text_1 <- renderText({ 
		input$goButton
		disease<-isolate(input$disease)
		
		means<-read.table(means_file,sep="\t",header=T,row.names=1,stringsAsFactors=F)
		case_mean<-signif(means[disease,"case_mean"],2)
		control_mean<-signif(means[disease,"control_mean"],2)
		dis<-tolower(diseaseNames[disease,"Disease"])
		
		over_prop<-signif(1-pnorm(case_mean,mean=means[disease,"control_mean"],sd=means[disease,"control_sd"]),2)*100
		
				
		m<-paste0("A genetic risk score is an arbitrary value that gives a summary of a large number of different SNPs each of which contribute a little to disease risk. The higher the value, the higher the risk of developing disease.<br><br>
This plot shows the risk profile for ",dis,". Patients with this disease have genetic risk scores centered around ",case_mean, ", as shown by the shape of the red line. The blue line, in contrast, shows the genetic risk score profile for a matched group of healthy controls. Their genetic risk score for ",dis, " center around ",control_mean,". However, even though they are healthy, several actually have higher genetic risk scores than the average patient's risk score (",over_prop,"%). This means that even with high genetic risk scores, you can be perfectly healthy all your life. It only tweaks the probabities. This illustrates the weak point of using GWAS technology for prognosis"
		)
		return(m)
	})
	
	
	output$text_2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			disease<-isolate(input$disease)
			dis<-tolower(diseaseNames[disease,"Disease"])
			
			sourcePaper<-diseaseNames[disease,"Source"]
			if(sourcePaper == "ellinghaus"){
				pubmed<-"26974007"
			}else if(sourcePaper == "okada"){
				pubmed<-"24390342"
			}else{stop("!!")}
			
			m<-paste0("<small><b>Your interpretation:</b> Your genetic risk score is shown as a vertical black bar. The more to the right it is, the higher a genetic risk score for ",dis," you have. However as explained above, people who remain healthy can also have higher genetic risk scores. The key is therefore to consider how many healthy individuals have a <i>lower</i> score (the blue percentage). The lower the better because it means people with worse genetic profiles than you typically stay healthy Conversely, the red percentage indicates the fraction of patients with the disease who have a <i>higher</i> genetic risk score than you. The overall point, of course, is also to illustrate that as long as GWAS data cannot separate healthy and patient groups better, their prognistic value is limited: completely separated tops would be nice to have clinically, but this is not what exists from GWAS.<br><br><br>
<b>Methods:</b> The GWAS findings were taken from the study by <u><a href='http://www.ncbi.nlm.nih.gov/pubmed/",pubmed,"'>",sourcePaper," et al</a></u>. Probability distributions were calculated as random samplings of genotypes based on the reported minor allele frequency either in cases or controls. Genetic risk scores were calculated as the sum of risk-allele count multiplied with the effect size.</small>")
			
		}
		return(m)
	})
	
	
	output$plot_1 <- renderPlot({ 
		
		disease<-isolate(input$disease)
		source<-diseaseNames[disease,"Source"]
		
		SNPs_to_analyze<-read.table(sub("SOURCE",source,SNPs_to_analyze_file),sep="\t",stringsAsFactors=F,header=T,row.names=1)
		
		# Take a dependency on input$goButton
		if(input$goButton > 0) {
		  uniqueID<-isolate(gsub(" ","",input$uniqueID))
			if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
			if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
			# pDataFile<-paste(dataFolder,uniqueID,"/pData.txt",sep="")
			if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
				Sys.sleep(3) #wait a little to prevent raw-force fishing	
				stop(safeError("Did not find a user with this id"))
			}
			
			genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
			
			
			#get risk score
			if(source == "ellinghaus"){
				or_column<-paste0("OR.",disease,".")
			}else if(source=="okada"){
				or_column<-"OR"
			}else{stop("!")}
			
			SNPs_to_analyze[,"Beta"]<-log10(SNPs_to_analyze[,or_column])
			GRS_beta <-get_GRS(genotypes=genotypes,betas=SNPs_to_analyze)
		}
		means<-read.table(means_file,sep="\t",header=T,row.names=1,stringsAsFactors=F)
		case_mean<-means[disease,"case_mean"]
		case_sd<-means[disease,"case_sd"]
		control_mean<-means[disease,"control_mean"]
		control_sd<-means[disease,"control_sd"]
		
		xlim<-c(case_mean - case_sd*3, case_mean + case_sd*2)
		
		x<-seq(xlim[1],xlim[2],length.out=100)
		y_case<-dnorm(x,mean=case_mean,sd=case_sd)
		y_control<-dnorm(x,mean=control_mean,sd=control_sd)
		y_control_scaled <- y_control* (max(y_case)/max(y_control))

		
				
		
		# pdf("test3.pdf")
		plot(x,y_case,type="l",col="red",ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n",lwd=2)
		lines(x,y_control_scaled,col="blue",fg="darkred",lwd=2)
		
		
		
		
		
		
		
		if(input$goButton > 0) {
		
			
				
			#fill the controls
			if(all(!x<GRS_beta))stop("GRS too low to plot")
			max_GRS_i<-max(which(x<GRS_beta))
			upper_x<-x[1:max_GRS_i]
			upper_y<-y_control_scaled[1:max_GRS_i]
			x_lines <- c(upper_x,GRS_beta,GRS_beta,xlim[1])
			y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
			polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
			
			#add control text
			prop<-signif(pnorm(GRS_beta,mean=control_mean,sd=control_sd),2)
			x_text<-upper_x[round(length(upper_x)/2)]
			y_text<-upper_y[round(length(upper_y)/2)] / 2
			text(x_text,y_text,paste0(prop*100,"%"),col="blue")
			
			
			
			
			#fill the cases
			upper_x<-x[max_GRS_i:length(x)]
			upper_y<-y_case[max_GRS_i:length(x)]
			x_lines <- c(GRS_beta,GRS_beta,upper_x,xlim[2])
			y_lines <- c(0,upper_y[1],upper_y,0)
			polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(1,0,0,0.3), lty = par("lty"))
			
			#add case text
			prop<-signif(1-pnorm(GRS_beta,mean=case_mean,sd=case_sd),2)
			x_text<-upper_x[round(length(upper_x)/2)]
			y_text<-upper_y[round(length(upper_y)/2)] / 2
			text(x_text,y_text,paste0(prop*100,"%"),col="red")
			
			
			#draw the main line
			abline(v=GRS_beta,lwd=3)
			
			
			
			
			
			legend("topleft",legend=c("People with disease","Healthy people","Your genetic risk score"),lty=c(1,1,1),lwd=c(2,2,3),col=c("red","blue","black"))
		}else{
			
			legend("topleft",legend=c("People with disease","Healthy people"),lty=c(1,1),lwd=c(2,2),col=c("red","blue"))
			

			
		}
		
		#write the score to the log file
		log_function<-function(uniqueID,disease,GRS_beta){
			user_log_file<-paste(get_conf("data_path"),uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"autoimmuneDiseases",uniqueID,disease,GRS_beta)
			m<-paste(m,collapse="\t")
			if(file.exists(user_log_file)){
				write(m,file=user_log_file,append=TRUE)
			}else{
				write(m,file=user_log_file,append=FALSE)
			}
		}
		try(log_function(uniqueID,disease,GRS_beta))
		
		
	})
	
	
	
	
})


