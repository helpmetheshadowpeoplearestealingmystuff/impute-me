library("shiny")


shinyServer(function(input, output) {
	
  
  
	output$table1 <- DT::renderDataTable({ 
		if(input$goButton == 0){
			return(NULL)
		}else if(input$goButton > 0) {
			print(paste("Ok",input$goButton))
		}
		
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
	  ethnicity_group <- isolate(input$ethnicity_group)
	  
		if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
		if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
		if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
			Sys.sleep(3) #wait a little to prevent raw-force fishing	
			stop(safeError("Did not find a user with this id"))
		}
		
	  
	  
	  #Get vcf-class and abort module if TRUE
	  pDataFile<-paste(get_conf("data_path"),uniqueID,"/pData.txt",sep="")
	  is_vcf<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"imputation_type"]=="vcf")
	  if(!is.na(is_vcf) && class(is_vcf)!="try-error" && length(is_vcf) == 1 && is_vcf)stop(safeError("VCF-error."))
	  
		
	  #load the coding/nonsense SNPs that can reasonably be reached with imputation
		load(paste0(get_conf("code_path"),"/nonsenser/2017-04-05_all_coding_SNPs.rdata"))
		coding_snps[,"SNP"]<-rownames(coding_snps)

		#get genotypes
		genotypes<-get_genotypes(uniqueID=uniqueID,request=coding_snps,namingLabel="cached.nonsenser")
		coding_snps[,"Your genotype"]<-genotypes[rownames(coding_snps),]

		
		#new 2017-04-05, get Frequency and 'Common allele' based on 1kg data and ethnicity
		coding_snps[,"Common allele"]<-coding_snps[,"Frequency"] <- NULL #remove the old annovar derived frequency
		if(ethnicity_group == "automatic"){
		  stop(safeError("Automatic guess ethnicity not implemented yet"))
		}else if(ethnicity_group == "global"){
		  coding_snps[,"new_freq"] <- coding_snps[,paste0("AF")]
		}else{
		  #then replace the MAF with the correct superpopulation group
		  coding_snps[,"new_freq"] <- coding_snps[,paste0(ethnicity_group,"_AF")]
		}
		flips<-which(coding_snps[,"new_freq"] > 0.5)
		coding_snps[,"Common allele"]<-coding_snps[,"REF"]
		coding_snps[,"Minor allele"]<-coding_snps[,"ALT"]
		coding_snps[,"Frequency"]  <- coding_snps[,"new_freq"] 
		coding_snps[flips,"Common allele"]<-coding_snps[flips,"ALT"]
		coding_snps[flips,"Minor allele"]<-coding_snps[flips,"REF"]
		coding_snps[flips,"Frequency"]  <- 1- coding_snps[flips,"new_freq"] 
		
		
		
		
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
			user_log_file<-paste(get_conf("data_path"),uniqueID,"/user_log_file.txt",sep="")
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
	},rownames= FALSE)
	
	
	
	

	output$text1 <- renderText({ 
	  input$goButton #to update on go
	  m<-HTML("Most SNPs in the genome are not actually found within a gene: They are 'intergenic'. When talking about a gene-mutation however, as is done in popular media, most often the meaning is a SNP that alters the sequence of a gene. Because of selection pressure throughout our evolution, these are rare. Also, they are often the focus of scientific studies using DNA-sequencing technology to discover causes of rare diseases. However, interestingly many of us actually have these 'gene-breaking' SNPs while nonetheless being perfectly healthy. The imputation technology used with this site, gives the opportunity to identify a number of these based just on genotyping microarray results. If you give your unique-ID to this module a table of all measured missense and nonsense mutations will be presented. <br><br>
					 
	Interpretation of the table can be done in many ways and unlike other modules, this does not give 'one true answer'. One method is to search for SNPs where you have one or two copies of the non-common allele and then investigate the consequence using other resources such as <u><a href='http://www.ncbi.nlm.nih.gov/SNP/'>dbSnp</a></u> or <u><a href='http://exac.broadinstitute.org/'>ExAC</a></u>. Note however that the definition of 'common' is very dependent on ethnicity: in this browser common just means the allele most often found in impute.me-users. However, it is recommended to check the ethnical distribution in e.g. the <a href='http://www.1000genomes.org/'>1000 genomes browser</a>. Another help provided is the <u><a href='http://genetics.bwh.harvard.edu/pph2/'>polyphen</a></u> and <u><a href='http://sift.jcvi.org/'>SIFT</a></u>-scores, which can give an indication of the consequence. Ultimately the goal of this is to satisfy ones curiousity about the state of your functional genes. If you happen to find out that you carry two copies of completely deleterious mutations (non-sense mutation) but otherwise feel healthy, feel free to contact us. By being healthy, in spite of a specific broken gene, you'd be contributing to complete our view of genes and how they work.")
	  
	  
	  
	  #Get vcf-class and abort module if TRUE
	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
	  pDataFile<-paste(get_conf("data_path"),isolate(gsub(" ","",input$uniqueID)),"/pData.txt",sep="")
	  is_vcf<-try(read.table(pDataFile,header=T,stringsAsFactors=F,sep="\t")[1,"imputation_type"]=="vcf")
	  if(!is.na(is_vcf) && class(is_vcf)!="try-error" && length(is_vcf) == 1 && is_vcf)stop(safeError("This module has been disabled for users submitting vcf files. That's because vcf files usually are derived from DNA-sequencing, but the investigations made in this module are tailored to microarray-data. Directly analyzing the vcf file itself, outside of impute.me, is likely to be more informative for you in context of the questions asked by this module."))
	  
	  return(m)
	})
	
})


