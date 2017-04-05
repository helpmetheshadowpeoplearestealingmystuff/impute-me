library("shiny")

options(shiny.sanitize.errors=F)
source("/home/ubuntu/srv/impute-me/functions.R")


# uniqueID<-"id_613z86871"

# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	output$table1 <- renderDataTable({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
	  ethnicity_group <- isolate(input$ethnicity_group)
	  
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError("Did not find a user with this id"))
		}
		
		
		#Get gender
		# gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
		
		load("/home/ubuntu/srv/impute-me/nonsenser/2015-12-16_all_coding_SNPs.rdata")
		coding_snps[,"SNP"]<-rownames(coding_snps)

		#get genotypes and calculate gheight
		genotypes<-get_genotypes(uniqueID=uniqueID,request=coding_snps,namingLabel="cached.nonsenser")
		coding_snps[,"Your genotype"]<-genotypes[rownames(coding_snps),]

		
		#new 2017-04-05, get Frequency and 'Common allele' based on 1kg data and ethnicity
		coding_snps[,"Common allele"]<-coding_snps[,"Frequency"] <- NULL #remove the old annovar derived frequency
		if(ethnicity_group == "automatic"){
		  stop(safeError("Automatic guess ethnicity not implemented yet"))
		}else if(ethnicity_group == "global"){
		  coding_snps[,"Frequency"] <- coding_snps[,paste0("AF")]
		}else{
		  #then replace the MAF with the correct superpopulation group
		  coding_snps[,"new_freq"] <- coding_snps[,paste0(ethnicity_group,"_AF")]
		}
		flips<-which(coding_snps[,"new_freq"] > 0.5)
		coding_snps[,"Common allele"]<-coding_snps[,"REF"]
		coding_snps[,"Minor allele"]<-coding_snps[,"ALT"]
		coding_snps[,"Frequency"]  <- 1 - coding_snps[,"new_freq"] 
		coding_snps[flips,"Common allele"]<-coding_snps[flips,"ALT"]
		coding_snps[flips,"Minor allele"]<-coding_snps[flips,"REF"]
		coding_snps[flips,"Frequency"]  <- coding_snps[flips,"new_freq"] 
		
		
		
		
		out<-coding_snps[,c("SNP","Your genotype","Common allele","Frequency","SIFT_pred","MutationTaster_pred","Polyphen2_HDIV_pred")]
		colnames(out)[5]<-"SIFT"
		colnames(out)[6]<-"MutationTaster"
		colnames(out)[7]<-"PolyPhen2"

		
		out[,"Heterozygote"]<-sapply(strsplit(out[,"Your genotype"],"/"),function(x){x[1] != x[2]})
		out[,"Unmeasured"]<- is.na(out[,"Your genotype"])
		
		out[,"Homozygote allele"]<-sub("/.$","",out[,"Your genotype"])
		out[out[,"Heterozygote"] & !out[,"Unmeasured"],"Homozygote allele"]<-NA
		
		out[,"Homozygote minor"]<-out[,"Homozygote allele"]!= out[,"Common allele"] & out[,"Common allele"]!=""
		out[is.na(out[,"Homozygote minor"]),"Homozygote minor"]<-FALSE
		
		type<- rep("Homozygote major?",nrow(out))
		type[out[,"Homozygote minor"]]<-"Homozygote minor?"
		type[out[,"Heterozygote"]]<-"Heterozygote"
		type[out[,"Unmeasured"]]<-"Unmeasured"
		out[,"Type"]<-factor(type,levels=c("Homozygote minor?","Heterozygote","Homozygote major?","Unmeasured"))
		
		out<-out[order(out[,"Type"], out[,"Frequency"]),]
		
		out[,"Heterozygote"]<-out[,"Unmeasured"]<-out[,"Homozygote allele"]<-out[,"Homozygote minor"]<-NULL

		
		#write the score to the log file
		log_function<-function(uniqueID){
			user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"nonsenser",uniqueID)
			m<-paste(m,collapse="\t")
			if(file.exists(user_log_file)){
				write(m,file=user_log_file,append=TRUE)
			}else{
				write(m,file=user_log_file,append=FALSE)
			}
		}
		try(log_function(uniqueID))
		
		
		#then return		
		return(out)
	})
	
})


