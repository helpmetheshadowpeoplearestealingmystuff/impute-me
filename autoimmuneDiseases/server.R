library("shiny")

#REMOVE LATER
# rm(list=ls())
# source("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/functions_local.R")
# dataFolder<-"C:/Users/FOLK/Documents/Work/Bioinformatics/data/"
# uniqueID <- "id_57n662948"
# disease<-"AS"
# SNPs_to_analyze_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"
# means_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/2016-05-18_means.txt"



source("/srv/shiny-server/gene-surfer/functions.R")
dataFolder<-"/home/ubuntu/data/"
SNPs_to_analyze_file<-"/srv/shiny-server/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"
means_file<-"/srv/shiny-server/gene-surfer/autoimmuneDiseases/2016-05-18_means.txt"



diseaseNames<-c("UC"="Ulcerative colitis",
								"CD"="Crohn’s disease",
								"PS"="Psoriasis",
								"PSC"="Primary Sclerosing Cholangitis",
								"AS"="Ankylosing Spondylitis")


shinyServer(function(input, output) {
	
	output$text_1 <- renderText({ 
		input$goButton
		disease<-isolate(input$disease)
		
		means<-read.table(means_file,sep="\t",header=T,row.names=1,stringsAsFactors=F)
		case_mean<-signif(means[disease,"case_mean"],2)
		case_low<-signif(means[disease,"case_mean"] - 2*means[disease,"case_sd"],2)
		case_high<-signif(means[disease,"case_mean"] +2*means[disease,"case_sd"],2)
		control_mean<-signif(means[disease,"control_mean"],2)
		dis<-tolower(diseaseNames[disease])
		
		
		m<-paste0("A genetic risk score is an arbitrary value that gives a summary of a large number of different SNPs each of which contribute a little to disease risk. The higher the value, the higher the risk of developing disease.<br><br>
This plot shows the risk profile for ",dis,". Patients with this disease have genetic risk scores centered around ",case_mean, ", as shown by the shape of the red line. However as also shown, patients with ",dis," often have genetic risk scores as low as ",case_low," and as high as ",case_high,". The blue line, in contrast, shows the genetic risk score profile for a matched group of healthy controls. Even though they are healhty, their genetic risk score for ",dis, " center around ",control_mean,". This means that even with high genetic risk scores, you can be perfectly healthy all your life. It only tweaks the probabities. This illustrates the weak point of using GWAS technology for prognosis"
		)
		return(m)
	})
	
	
	output$text_2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
			disease<-isolate(input$disease)
			dis<-tolower(diseaseNames[disease])
			
			m<-paste0("<small><b>Your interpretation:</b> Your own genetic risk score is shown as a vertical black bar. The more to the right it is, the higher a genetic risk score for ",dis," you have. However as explained above, people who remain healthy can also have higher genetic risk scores. To know how much extra genetic risk you have, the key for interpretation is therefore to look at your genetic risk score and see how much higher or lower the red line is compared to the blue line. If the blue line is much higher, you have much higher chance of remaining healthy. However if the red line is much higher, you risk of ", dis, " is increased.</small>")
			
		}
		return(m)
	})
	
	
	output$plot_1 <- renderPlot({ 
		
		disease<-isolate(input$disease)
		SNPs_to_analyze<-read.table(SNPs_to_analyze_file,sep="\t",stringsAsFactors=F,header=T,row.names=1)
		
		# Take a dependency on input$goButton
		if(input$goButton > 0) {
			uniqueID<-isolate(input$uniqueID)
			if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
			if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
			# pDataFile<-paste(dataFolder,uniqueID,"/pData.txt",sep="")
			if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
				Sys.sleep(3) #wait a little to prevent raw-force fishing	
				stop("Did not find a user with this id")
			}
			
			genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
			
			
			#get risk score
			or_column<-paste0("OR.",disease,".")
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
		plot(x,y_case,type="l",col="red",ylab="Frequency",xlab="Genetic risk score",yaxt="n")
		lines(x,y_control_scaled,col="blue",fg="darkred")
		
		
		
		
		
		
		
		if(input$goButton > 0) {
		
			
				
			#fill the controls
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
			
			
			
			
			
			legend("topleft",legend=c("Disease risk profile","Healthy risk profile","Your genetic risk score"),lty=c(1,1,1),lwd=c(1,1,3),col=c("red","blue","black"))
		}else{
			
			legend("topleft",legend=c("Disease risk profile","Healthy risk profile"),lty=c(1,1),lwd=c(1,1),col=c("red","blue"))
			
			
		}
	})
	
	
	
	
})


