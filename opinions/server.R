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
	  if(input$goButton == 0){
	    return(NULL)
	  }else if(input$goButton > 1){
	    stop("Please only run algorithm - or else reload web-page")
	  }else{
	    
		#initial UI data gathering and user-check
		uniqueID<-input$uniqueID
		real_age<-input$real_age
		real_opinion<-input$real_opinion
		
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(paste("Did not find a user with this id",uniqueID))
		}
		
		real_age <- as.numeric(real_age)
		if(is.na(real_age))stop("Must give your age to participate")
		if(real_age < 1 | real_age > 120)stop("Must your current real age in years to participate")
		
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
		
		
		#getting the relevant trait name, pmid and SNPs to analyze
		SNPs_to_analyze<-read.table("/home/ubuntu/srv/impute-me/opinions/SNPs_to_analyze.txt",sep="\t",stringsAsFactors = F,row.names=1,header=T)
		
		genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
		genotypes[,"GRS"] <-get_GRS_2(genotypes=genotypes,betas=SNPs_to_analyze)
		
		
		
		#store the new info in the pData
		pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
		pData[,"real_age"]<-real_age
		pData[,"real_opinion"]<-real_opinion
		pData[,"g_opinion"]<-mean(genotypes[,"GRS"],na.rm=T)
		write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
		
		
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
			genotypes=genotypes,
			gender=gender,
			real_age=real_age,
			real_opinion=real_opinion
			))
	  }
	})
	
	
	
	
	output$plot_1 <- renderPlot({ 
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			o<-get_data()
			# SNPs_to_analyze<-o[["SNPs_to_analyze"]]
			genotypes<-o[["genotypes"]]
			
			GRS_beta<-mean(genotypes[,"GRS"],na.rm=T)
			
			# if(GRS_beta < -3 | GRS_beta > 3)stop("Your genetic political score is off the scale")
			if(is.na(GRS_beta))stop("Could not calculate overall GRS because all SNPs in the signature were missing information about either risk-allele, effect-size or minor-allele-frequency.")
			
			#get all the other persons in the list
			otherPersons<-list.files("/home/ubuntu/data/",full.names=T)
			opinions_in_data<-data.frame(real_opinion=vector(),g_opinion=vector(),gender=vector(),real_age=vector(),stringsAsFactors=F)
			for(otherPerson in otherPersons){
			  if(!file.info(otherPerson)[["isdir"]])next
			  if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
			  otherPersonPdata<-read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F)
			  if(!all(c("real_opinion","g_opinion","gender","real_age")%in%colnames(otherPersonPdata)))next
			  opinions_in_data<-rbind(opinions_in_data,otherPersonPdata[1,c("real_opinion","g_opinion","gender","real_age")])
			}
			
			xlim <- range(c(opinions_in_data[,"real_opinion"],o[["real_opinion"]]),na.rm=T)
			ylim <- range(c(opinions_in_data[,"g_opinion"],o[["g_opinion"]]),na.rm=T)
			
			plot(NULL,xlim=xlim,ylim=ylim,xlab="Stated political opinion",ylab="Genetic opinion score")
			pch <- c(15,16)
      names(pch)<-c("1","2") #i.e. man/woman (according to plink notation)
      
      
      colRes<-100
      cols<-rev(rgb(seq(0,0.5,length.out=colRes),seq(0.5,1,length.out=colRes),seq(0,0.5,length.out=colRes)))
      names(cols) <- as.character(1:length(cols))
      cols<-cols[seq(0,length(cols),by=10)]
      xmin<- xlim[1]
      xmax<- xlim[1] + (xlim[2] - xlim[1])*0.06
      ymin <- ylim[1] + (ylim[2] - ylim[1])*0.7
      ymax <- ylim[1] + (ylim[2] - ylim[1])*0.78
      scale = (length(cols)-1)/(xmax-xmin)
      # plot(NULL,xlim=c(-4,4),ylim=c(-4,4),ylab="",xlab="",xaxt="n",yaxt="n")
      for (i in 1:(length(cols)-1)) {
        x = (i-1)/scale + xmin
        rect(x,ymin,x+1/scale,ymax, col=cols[i], border=NA)
         
      }
      text(x=xmax,y=mean(c(ymin,ymax)),"Age",adj=-0.2)
      legend("topleft",legend=c("Male","Female"),pch=pch,bty = "n",col=cols["50"])
      
      
			

						
			# plot all the others
			points(
			  x=opinions_in_data[,"real_opinion"],
			  y=opinions_in_data[,"g_opinion"],
			  col=cols[as.character(10*round(opinions_in_data[,"real_age"]/10))],
			  pch=pch[opinions_in_data[,"gender"]]
			)

			#then plot main person
			points(o[["real_opinion"]], y=GRS_beta,pch=pch[o[["gender"]]],cex=2,col=cols[as.character(10*round(o[["real_age"]]/10))])
			abline(h=GRS_beta,lty=2)
			abline(v=o[["real_opinion"]],lty=2)
			
						
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


