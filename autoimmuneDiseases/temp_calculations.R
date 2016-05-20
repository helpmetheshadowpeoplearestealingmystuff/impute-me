rm(list=ls())

source("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/functions_local.R")
SNPs_to_analyze_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"


# source("/srv/shiny-server/gene-surfer/functions.R")
# SNPs_to_analyze_file<-"/srv/shiny-server/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"

SNPs_to_analyze<-read.table(SNPs_to_analyze_file,sep="\t",stringsAsFactors=F,header=T,row.names=1)

#From SNP look up - we know that (except for MAF>0.45 cases etc) the A1 is always the minor allele


diseases<-c("AS","CD","PS","PSC","UC")



#simulating distribution
n<-10
out<-list()
for(disease in c("CON",diseases)){
	results<-matrix(nrow=nrow(SNPs_to_analyze),ncol=n,dimnames=list(rownames(SNPs_to_analyze),1:n))
	for(snp in rownames(SNPs_to_analyze)){
		maf_col<-paste0("MAF.",disease,".")
		maf<-SNPs_to_analyze[snp,maf_col]
		selection1<-c(rep(SNPs_to_analyze[snp,"effect_allele"],round(maf*(n*2))),rep(SNPs_to_analyze[snp,"non_effect_allele"],round((1-maf)*(n*2))))
		selection2<-sample(selection1,n)
		genotypes1<-apply(matrix(selection2,ncol=2),1,paste,collapse="/")
		results[snp,]<-genotypes1
	}
	out[[disease]] <- results
}

save(out,file="2016-05-18_genotype_examples.rdata")


GRSs<-matrix(nrow=length(diseases)*2,ncol=n,dimnames=list(c(paste0(diseases,"_case"),paste0(diseases,"_control")),as.character(1:n)))

for(person in as.character(1:n)){
	for(disease in diseases){
		
		print(paste(person,"of",n,"with",disease))
		genotypes_disease<-data.frame(out[[disease]][,person,drop=FALSE],stringsAsFactors = F)
		genotypes_control<-data.frame(out[["CON"]][,person,drop=FALSE],stringsAsFactors = F)
		colnames(genotypes_disease)<-colnames(genotypes_control)<-"genotype"
		or_column<-paste0("OR.",disease,".")
		SNPs_to_analyze[,"Beta"]<-log10(SNPs_to_analyze[,or_column])

			
		GRS_beta_disease <-get_GRS(genotypes=genotypes_disease,betas=SNPs_to_analyze)
		GRSs[paste0(disease,"_case"),person]<- GRS_beta_disease
		GRS_beta_control <-get_GRS(genotypes=genotypes_control,betas=SNPs_to_analyze)
		GRSs[paste0(disease,"_control"),person]<- GRS_beta_control
	}
}


save(GRSs,file="2016-05-18_GRSs_examples.rdata")




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

write.table(means,file="2016-05-18_means.txt",sep="\t",col.names=NA)
