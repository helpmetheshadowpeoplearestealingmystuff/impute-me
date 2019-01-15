library("shiny")

source("/home/ubuntu/srv/impute-me/functions.R")


shinyServer(function(input, output) {
	
	output$text_1 <- renderText({ 
		
	  input$goButton
	  
		if(input$goButton == 0){
			m<-paste0("An interesting study of political opinion was performed by <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/24569950'>Hatemi et al</a></u> in 2014. While the authors themselves conclude that results are too weak to be formally significant, it can provide the basis of an interesting self-check on genetics of politics. Note, I doubt we see a strong correlation, but it is fun to test.<br><br>"
			)
			
		}else{
			m<-""
		}
		return(m)
	})
	
	
	
	get_data <- reactive({
	  if(input$goButton == 0){
	    return(NULL)
	  }else{
	    
		#initial UI data gathering and user-check
	  uniqueID<-gsub(" ","",input$uniqueID)
		real_age<-isolate(input$real_age)
		real_opinion<-isolate(input$real_opinion)
		
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError(paste("Did not find a user with this id",uniqueID)))
		}
		
		real_age <- as.numeric(real_age)
		if(is.na(real_age))stop("Must give your age to participate")
		if(real_age < 1 | real_age > 120)stop("Must your current real age in years to participate")
		
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
		
		
		#getting the relevant trait name, pmid and SNPs to analyze
		SNPs_to_analyze<-read.table("/home/ubuntu/srv/impute-me/opinions/SNPs_to_analyze.txt",sep="\t",stringsAsFactors = F,row.names=1,header=T)
		
		genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze)
		
		
		SNPs_to_analyze[,"genotype"] <- genotypes[rownames(SNPs_to_analyze),"genotype"]
		SNPs_to_analyze <-get_GRS_2(SNPs_to_analyze,mean_scale=T, unit_variance=T, verbose=T)
		population_sum_sd<-sqrt(sum(SNPs_to_analyze[,"population_score_sd"]^2,na.rm=T))
		GRS_beta <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T) / population_sum_sd
		if(is.na(GRS_beta))stop("Could not calculate overall GRS because all SNPs in the signature were missing information about either risk-allele, effect-size or minor-allele-frequency.")
		
		
		#store the new info in the pData
		pData<-read.table(pDataFile,header=T,stringsAsFactors=F)
		pData[,"real_age"]<-real_age
		pData[,"real_opinion"]<-real_opinion
		pData[,"g_opinion"]<-GRS_beta
		write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
		
		
		#load database for comparison
		#this is a file that contains the GWAS opinions
		all_opinions_file<-"/home/ubuntu/misc_files/all_opinions.txt"
		opinions_in_data<-try(read.table(all_opinions_file,sep="\t",stringsAsFactors=F,header=T))
		
		#make robust against non-initialized files
		if(class(opinions_in_data)=="try-error"){
		  headers_required <- c("uniqueID","g_opinion","real_opinion","real_age","gender","source","datestamp")
		  opinions_in_data <-as.data.frame(matrix(nrow=0,ncol=length(headers_required),dimnames=list(NULL,headers_required)))
		  write.table(opinions_in_data,file=all_opinions_file, quote=F,row.names=F,col.names=T,sep="\t")
		}
		
		
		
		#also store this in the all_opinions_file file (for faster loading)
		line<-paste(c(uniqueID,GRS_beta,real_opinion,real_age,gender,"interactive",format(Sys.time(),"%Y-%m-%d-%H-%M-%S")),collapse="\t")
		all_opinions_file<-"/home/ubuntu/misc_files/all_opinions.txt"
		if(!is.na(GRS_beta) & !is.na(real_age) & !is.na(real_opinion) & uniqueID != "id_613z86871"){ #only save if height is given and it is not the test user
		  write(line,file=all_opinions_file,append=TRUE)  
		}

    #sort for later user		
		opinions_in_data<-opinions_in_data[order(opinions_in_data[,"datestamp"],decreasing = T),]
		#use latest only 
		opinions_in_data<-opinions_in_data[!duplicated(opinions_in_data[,"uniqueID"]),]
		
		
		
		#do statistics
		models<-list()
		if(nrow(opinions_in_data)>2){
		  models[["lm"]]<-lm(real_opinion~g_opinion,data=opinions_in_data)
		  models[["lm-corrected"]]<-lm(real_opinion ~ g_opinion + gender + real_age,data=opinions_in_data)
		  models[["spearman"]]<-suppressWarnings(cor.test(opinions_in_data[,"g_opinion"],opinions_in_data[,"real_opinion"],method="spearman"))
		}
		
		#write the score to the log file
		log_function<-function(uniqueID,study_id,genotypes){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"opinions",uniqueID,GRS_beta,gender,real_age,real_opinion)
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
			real_opinion=real_opinion,
			g_opinion=GRS_beta,
			models=models,
			opinions_in_data=opinions_in_data
			))
	  }
	})
	
	
	
	
	output$plot_1 <- renderPlot({ 
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			o<-get_data()
			opinions_in_data<-o[["opinions_in_data"]]

			xlim <- range(c(opinions_in_data[,"g_opinion"],o[["g_opinion"]]),na.rm=T)
			
			#temporary fix for some clear ethnicity effects
			xlim[1]<-0
			
			ylim <- range(c(opinions_in_data[,"real_opinion"],o[["real_opinion"]]),na.rm=T)
			plot(NULL,ylim=ylim,xlim=xlim,ylab="Stated political opinion (down is 'left', up is 'right')",xlab="Genetic opinion score (right is 'right', left is 'left')")
			pch <- c(15,16)
      names(pch)<-c("1","2") #i.e. man/woman (according to plink notation)
      
      
      colRes<-100
      cols<-rev(rgb(seq(0,0.5,length.out=colRes),seq(0.5,1,length.out=colRes),seq(0,0.5,length.out=colRes)))
      names(cols) <- as.character(1:length(cols))
      cols<-cols[seq(0,length(cols),by=10)]
      xmin<- xlim[1] + (xlim[2] - xlim[1])* -0.02
      xmax<- xlim[1] + (xlim[2] - xlim[1])*0.05
      ymin <- ylim[1] + (ylim[2] - ylim[1])*0.78
      ymax <- ylim[1] + (ylim[2] - ylim[1])*0.86
      scale = (length(cols)-1)/(xmax-xmin)
      # plot(NULL,xlim=c(-4,4),ylim=c(-4,4),ylab="",xlab="",xaxt="n",yaxt="n")
      for (i in 1:(length(cols)-1)) {
        x = (i-1)/scale + xmin
        rect(x,ymin,x+1/scale,ymax, col=cols[i], border=NA)
         
      }
      text(x=xmax,y=mean(c(ymin,ymax)),"Age",adj=-0.2)
      legend("topleft",legend=c("Male","Female"),pch=pch,bty = "n",col="black")
      

						
			# plot all registered people
			points(
			  y= -opinions_in_data[,"real_opinion"],
			  x=opinions_in_data[,"g_opinion"],
			  col=cols[as.character(10*round(opinions_in_data[,"real_age"]/10))],
			  pch=pch[opinions_in_data[,"gender"]],
			  yaxt="n",xaxt="n"
			)

			#highlight main person
			points(x=o[["g_opinion"]],y=o[["real_opinion"]],pch=pch[o[["gender"]]],cex=2,col=cols[as.character(10*round(o[["real_age"]]/10))])
			abline(v=o[["g_opinion"]],lty=2)
			abline(h=o[["real_opinion"]],lty=2)
			
			
      
			
			
		}		
	})
	
	
	output$text_2 <- renderText({ 
		if(input$goButton == 0){
			return("")
		}else if(input$goButton > 0) {
		  o<-get_data()
		  
		  if(length(o[["models"]])>0){
		    model1_p<-signif(summary(o[["models"]][["lm"]])[["coefficients"]]["g_opinion","Pr(>|t|)"],2)
		    model1_percent_explained<-signif(100*summary(o[["models"]][["lm"]])$r.squared,2)
		    
		    model2_p<-signif(summary(o[["models"]][["lm-corrected"]])[["coefficients"]]["g_opinion","Pr(>|t|)"],2)
		    model2_percent_explained<-signif(100*summary(o[["models"]][["lm-corrected"]])$r.squared,2)
		    
		    spearman_p<-signif(o[["models"]][["spearman"]][["p.value"]],2)
		    spearman_rho<-signif(o[["models"]][["spearman"]][["estimate"]],2)
		    
		    n <- nrow(o[["opinions_in_data"]])
		    
		    if(model2_p < 0.005){
		      outcome1 <- "<b>is</b> a"
		      outcome2 <- "is"
		    }else if(model2_p < 0.05){
		      outcome1 <- "<b>is</b> a weak"
		      outcome2 <- "is"
		    }else{
		      outcome1 <- "<b>is not</b> any"
		      outcome2 <- "would have been"
		    }
		    
		    if(spearman_rho<0.3){
		      outcome3 <- "a fairly low association score."
		    }else if(spearman_rho < 0.5){
		      outcome3 <- "a medium association score."
		    }else{
		      outcome3 <- "a high association score."
		    }
		    
		    #handling that outlier
		    notShown<-sum(o[["opinions_in_data"]][,"g_opinion"]< 0,na.rm=T)
		    
		    if(o[["g_opinion"]]<0){
		      notShownInsert<-", <i>including yours</i>, "
		    }else{
		      notShownInsert<-" "
		    }
		    
		    
		    m<-paste0("<br>With current input from previous users that have volunteered their own political opinion, we can calculate that there ",outcome1,"  significant political opinion effect from genetics. The percent of political opinion variation that ",outcome2," explained by genetics is ",model2_percent_explained,"% when correcting for age and gender (P=",model2_p,") and ",model1_percent_explained,"% unadjusted (P=",model1_p,"). Spearman rank correlation gives rho=",spearman_rho," (P=",spearman_p,"), which is ",outcome3," Note that ",notShown," samples",notShownInsert,"are not shown because of extreme genetic values (possibly ethnicity effects). They are included in statistics however.<br>")
		  }else{
		    m <- "Statistics not calculated. Need more users."
		  }
		  
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
	    
	    #summarising allele info into single-columns
			SNPs_to_analyze[,"Risk/non-risk Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
			SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
			
			#round MAF
			SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
			SNPs_to_analyze[,"SNP"]<-rownames(SNPs_to_analyze)
			
			keep<-c("SNP","genotype","Risk/non-risk Allele","personal_score","population_score_average","effect_size","PVALUE","Major/minor Allele","minor_allele_freq")
			SNPs_to_analyze<-SNPs_to_analyze[,keep]
			colnames(SNPs_to_analyze)<-c("SNP","Your Genotype","Risk/ non-risk Allele","SNP-score","SNP-score (population normalized)","Effect Size","P-value","Major/ minor Allele","Minor Allele Frequency")
			
			
			return(SNPs_to_analyze)
		}
	},options = list(searching = FALSE,paging = FALSE))

	
	output$text_3 <- renderText({ 
		
		if(input$goButton == 0){
			m<-""
			
			
		}else{
			m<-paste0("<small><br><b>Methods</b><br>The genetics risk score was calculated using the data found in <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/24569950'>Hatemi et al</a></u>. The method used is identical to the one describes for the <u><a href='http://www.impute.me/AllDiseases/'>complex disease</a></u> module.")
								
		}
		return(m)
	})
	
		
})


