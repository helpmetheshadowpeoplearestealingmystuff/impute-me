rm(list=ls())

source("/srv/shiny-server/gene-surfer/functions.R")


SNPs_to_analyze_file<-"/srv/shiny-server/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"
SNPs_to_analyze<-read.table(SNPs_to_analyze_file,sep="\t",stringsAsFactors=F,header=T,row.names=1)

#From SNP look up - we know that (except for MAF>0.45 cases etc) the A1 is always the minor allele


diseases<-c("CON","AS","CD","PS","PSC","UC")



#simulating healthy distribution
n<-500
out<-list()
for(disease in diseases){
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



GRSs<-matrix(nrow=length(diseases),ncol=n,dimnames=list(diseases,as.character(1:n)))

for(person in as.character(1:n)){
	for(disease in diseases){
		
		print(paste(person,"of",n,"with",disease))
		genotypes<-data.frame(out[[disease]][,person,drop=FALSE],stringsAsFactors = F)
		colnames(genotypes)<-"genotype"
		
		if(disease == "CON"){
			SNPs_to_analyze[,"Beta"]<-0
		}else{
			or_column<-paste0("OR.",disease,".")
			SNPs_to_analyze[,"Beta"]<-log10(SNPs_to_analyze[,or_column])
		}
		GRS_beta <-get_GRS(genotypes=genotypes,betas=SNPs_to_analyze)
		GRSs[disease,person]<- GRS_beta
	}
}


save(GRSs,file="2016-05-17_GRSs_examples.rdata")