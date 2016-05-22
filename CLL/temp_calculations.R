rm(list=ls())

source("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/functions_local.R")
SNPs_to_analyze_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/CLL/SNPs_to_analyze.txt"




# source("/srv/shiny-server/gene-surfer/functions.R")
# SNPs_to_analyze_file<-"/srv/shiny-server/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"

SNPs_to_analyze<-read.table(SNPs_to_analyze_file,sep="\t",stringsAsFactors=F,header=T,row.names=1)

#First we calculate effect-allele-frequency

SNPs_to_analyze<-SNPs_to_analyze[!is.na(SNPs_to_analyze[,"major_allele_SNAP"]),]

SNPs_to_analyze[,"orig_genotypes"]<-apply(t(apply(SNPs_to_analyze[,c("effect_allele","non_effect_allele")],1,sort)),1,paste,collapse="")
SNPs_to_analyze[,"snap_genotypes"]<-apply(t(apply(SNPs_to_analyze[,c("major_allele_SNAP","minor_allele_SNAP")],1,sort)),1,paste,collapse="")
all(SNPs_to_analyze[,"orig_genotypes"]==SNPs_to_analyze[,"snap_genotypes"])
#Good!





#Getting effect allele frequencies in a "control" (CEU)
SNPs_to_analyze[,"EAF_control"]<-SNPs_to_analyze[,"MAF_SNAP"]
SNPs_to_analyze[,"minor_is_effect"]<-SNPs_to_analyze[,"minor_allele_SNAP"] == SNPs_to_analyze[,"effect_allele"]
SNPs_to_analyze[!SNPs_to_analyze[,"minor_is_effect"],"EAF_control"] <-1-SNPs_to_analyze[!SNPs_to_analyze[,"minor_is_effect"],"MAF_SNAP"]


#Getting "effect frequencies" in case (based on OR)
for(snp in rownames(SNPs_to_analyze)){
	print(snp)
	OR<-SNPs_to_analyze[snp,"OR.M1"]
	EAF_control<-SNPs_to_analyze[snp,"EAF_control"]
	case_control_count<-strsplit(SNPs_to_analyze[snp,"cases_controls.M1"],"/")[[1]]
	case_allele_count <- 2*as.numeric(case_control_count[1])
	control_allele_count <- 2*as.numeric(case_control_count[2])
	e_allele_c_in_control<-round(EAF_control*control_allele_count)
	ne_allele_c_in_control<-round((1-EAF_control)*control_allele_count)
	odds_control <- e_allele_c_in_control/ne_allele_c_in_control
	odds_case = odds_control*OR
	e_allele_c_in_case<- round((case_allele_count*odds_case)/(1+odds_case))
	ne_allele_c_in_case <- case_allele_count - e_allele_c_in_case
	SNPs_to_analyze[snp,"EAF_case"] <- e_allele_c_in_case /case_allele_count
}


diseases<-c("CLL")



# write.table(SNPs_to_analyze,file="test.xls",sep="\t",col.names=NA)


#simulating distribution
n<-1000
out<-list()
for(disease in c("CON",diseases)){ 
	results<-matrix(nrow=nrow(SNPs_to_analyze),ncol=n,dimnames=list(rownames(SNPs_to_analyze),1:n))
	for(snp in rownames(SNPs_to_analyze)){
		if(disease=="CON"){
			maf_col<-paste0("EAF_control")	
		}else{
			maf_col<-paste0("EAF_case")	
		}
		
		maf<-SNPs_to_analyze[snp,maf_col]
		selection1<-c(rep(SNPs_to_analyze[snp,"effect_allele"],round(maf*(n*2))),rep(SNPs_to_analyze[snp,"non_effect_allele"],round((1-maf)*(n*2))))
		selection2<-sample(selection1,n)
		genotypes1<-apply(matrix(selection2,ncol=2),1,paste,collapse="/")
		results[snp,]<-genotypes1
	}
	out[[disease]] <- results
}

save(out,file="2016-05-22_genotype_examples.rdata")


GRSs<-matrix(nrow=length(diseases)*2,ncol=n,dimnames=list(c(paste0(diseases,"_case"),paste0(diseases,"_control")),as.character(1:n)))

for(person in as.character(1:n)){
	for(disease in diseases){
		
		print(paste(person,"of",n,"with",disease))
		genotypes_disease<-data.frame(out[[disease]][,person,drop=FALSE],stringsAsFactors = F)
		genotypes_control<-data.frame(out[["CON"]][,person,drop=FALSE],stringsAsFactors = F)
		colnames(genotypes_disease)<-colnames(genotypes_control)<-"genotype"
		or_column<-"OR.M1"
		SNPs_to_analyze[,"Beta"]<-log10(SNPs_to_analyze[,or_column])

			
		GRS_beta_disease <-get_GRS(genotypes=genotypes_disease,betas=SNPs_to_analyze)
		GRSs[paste0(disease,"_case"),person]<- GRS_beta_disease
		GRS_beta_control <-get_GRS(genotypes=genotypes_control,betas=SNPs_to_analyze)
		GRSs[paste0(disease,"_control"),person]<- GRS_beta_control
	}
}


save(GRSs,file="2016-05-22_GRSs_examples.rdata")




#Ok - they are all normally distributed, so go ahead and calculate means and SD
means<-matrix(nrow=length(diseases),ncol=4,dimnames=list(diseases,c("case_mean","case_sd","control_mean","control_sd")))


for(disease in diseases){
	
	case<-GRSs[paste0(disease,"_case"),]
	control<-GRSs[paste0(disease,"_control"),]
	
	
	means[disease,"case_mean"]<-mean(case)
	means[disease,"case_sd"]<-sd(case)
	means[disease,"control_mean"]<-mean(control)
	means[disease,"control_sd"]<-sd(control)
	
}

write.table(means,file="2016-05-22_means.txt",sep="\t",col.names=NA)


























