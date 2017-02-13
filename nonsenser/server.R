library("shiny")


source("/srv/shiny-server/gene-surfer/functions.R")


# uniqueID<-"id_57n662948"

# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	output$table1 <- renderDataTable({ 
		# Take a dependency on input$goButton
		
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
		uniqueID<-isolate(input$uniqueID)
		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		
		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop("Did not find a user with this id")
		}
		
		
		#Get gender
		gender<-read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"gender"]
		
		load("/srv/shiny-server/gene-surfer/nonsenser/2015-12-16_all_coding_SNPs.rdata")
		
		coding_snps[,"SNP"]<-rownames(coding_snps)

		#get genotypes and calculate gheight
		genotypes<-get_genotypes(uniqueID=uniqueID,request=coding_snps,namingLabel="cached.nonsenser")
		
		coding_snps[,"Your genotype"]<-genotypes[rownames(coding_snps),]

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


