rm(list=ls())

source("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/functions_local.R")
SNPs_to_analyze_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/SNPs_to_analyze.txt"


# source("/home/ubuntu/srv/impute-me/functions.R")
# SNPs_to_analyze_file<-"/home/ubuntu/srv/impute-me/autoimmuneDiseases/SNPs_to_analyze.txt"

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






























#then doing the same for the okada et al data

rm(list=ls())
source("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/functions_local.R")

##%¤! Pdf conversion. This took two hours to prepare
SNPs_to_analyze_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/2016-05-21_okada_et_al_S3.xlsx"

library(openxlsx)
# source("/home/ubuntu/srv/impute-me/functions.R")
# SNPs_to_analyze_file<-"/home/ubuntu/srv/impute-me/autoimmuneDiseases/SNPs_to_analyze.txt"

SNPs_to_analyze<-read.xlsx(SNPs_to_analyze_file)
SNPs_to_analyze<-SNPs_to_analyze[SNPs_to_analyze[,"Population"]%in%"Trans-ethnic",]
rownames(SNPs_to_analyze) <- SNPs_to_analyze[,"SNP"]


SNPs_to_analyze[,"case_freq"] <- as.numeric(SNPs_to_analyze[,"case_freq"])
SNPs_to_analyze[,"control_freq"] <- as.numeric(SNPs_to_analyze[,"control_freq"])

SNPs_to_analyze[,"effect_allele"]<-sub("/.+$","",SNPs_to_analyze[,"A1/A2"])
SNPs_to_analyze[,"non_effect_allele"]<-sub("^.+/","",SNPs_to_analyze[,"A1/A2"])

SNPs_to_analyze[,"OR"]<-as.numeric(sub(" .+$","",SNPs_to_analyze[,"OR.(upper.lower)"]))


SNPs_to_analyze<-SNPs_to_analyze[grep("^rs",rownames(SNPs_to_analyze)),]

#omit these that were frequently found to be missing
omitThese<-c('rs2301888','rs2228145','rs9268839','rs2736337','rs10774624','rs13330176')
SNPs_to_analyze<-SNPs_to_analyze[!rownames(SNPs_to_analyze)%in%omitThese,]


diseases<-c("RA")
colnames(SNPs_to_analyze)[2]<-"chr_name"

write.table(SNPs_to_analyze,file="2016-05-21_SNPs_to_analyse_okada.txt",sep="\t",row.names=F,col.names=T,quote=F)

#simulating distribution
n<-1000
out<-list()
for(disease in c("CON",diseases)){
	results<-matrix(nrow=nrow(SNPs_to_analyze),ncol=n,dimnames=list(rownames(SNPs_to_analyze),1:n))
	for(snp in rownames(SNPs_to_analyze)){
		head(SNPs_to_analyze)
		if(disease=="CON"){
			maf_col<-"control_freq"
		}else{
			maf_col<-"case_freq"
		}
		maf<-SNPs_to_analyze[snp,maf_col]
		selection1<-c(rep(SNPs_to_analyze[snp,"effect_allele"],round(maf*(n*2))),rep(SNPs_to_analyze[snp,"non_effect_allele"],round((1-maf)*(n*2))))
		selection2<-sample(selection1,n)
		genotypes1<-apply(matrix(selection2,ncol=2),1,paste,collapse="/")
		results[snp,]<-genotypes1
	}
	out[[disease]] <- results
}

save(out,file="2016-05-21_genotype_examples.rdata")


GRSs<-matrix(nrow=length(diseases)*2,ncol=n,dimnames=list(c(paste0(diseases,"_case"),paste0(diseases,"_control")),as.character(1:n)))

for(person in as.character(1:n)){
	for(disease in diseases){
		
		print(paste(person,"of",n,"with",disease))
		genotypes_disease<-data.frame(out[[disease]][,person,drop=FALSE],stringsAsFactors = F)
		genotypes_control<-data.frame(out[["CON"]][,person,drop=FALSE],stringsAsFactors = F)
		colnames(genotypes_disease)<-colnames(genotypes_control)<-"genotype"
		or_column<-"OR"
		SNPs_to_analyze[,"Beta"]<-log10(SNPs_to_analyze[,or_column])
		
		
		GRS_beta_disease <-get_GRS(genotypes=genotypes_disease,betas=SNPs_to_analyze)
		GRSs[paste0(disease,"_case"),person]<- GRS_beta_disease
		GRS_beta_control <-get_GRS(genotypes=genotypes_control,betas=SNPs_to_analyze)
		GRSs[paste0(disease,"_control"),person]<- GRS_beta_control
	}
}


save(GRSs,file="2016-05-21_GRSs_examples.rdata")




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

previous<-read.table("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/2016-05-18_means.txt",row.names=1,stringsAsFactors=F,header=T)

means<-rbind(previous,means)
write.table(means,file="2016-05-21_means.txt",sep="\t",col.names=NA)




#then re form the previous SNPs_to_analyze
previous_SNPs_to_analyze_file<-"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/autoimmuneDiseases/SNPs_to_analyze_ellington.txt"

previous_SNPs_to_analyze<-read.table(previous_SNPs_to_analyze_file,sep="\t",stringsAsFactors=F,header=T,row.names=1)

doubles<-intersect(rownames(previous_SNPs_to_analyze),rownames(SNPs_to_analyze))


SNPs_to_analyze_new<-SNPs_to_analyze[!rownames(SNPs_to_analyze)%in%rownames(previous_SNPs_to_analyze),]

head(SNPs_to_analyze_new)
SNPs_to_analyze_new<-SNPs_to_analyze_new[,c("Chr..Position.(bp)"),drop=F]
colnames(SNPs_to_analyze_new)<-"chr_name"

previous_SNPs_to_analyze<-previous_SNPs_to_analyze[,"chr_name",drop=F]


SNPs_to_analyze<-rbind(previous_SNPs_to_analyze,SNPs_to_analyze_new)
SNPs_to_analyze[,"SNP"]<-rownames(SNPs_to_analyze)
SNPs_to_analyze<-SNPs_to_analyze[,c("SNP","chr_name")]
write.table(SNPs_to_analyze,file="SNPs_to_analyze.txt",quote=F,col.name=T,row.name=F,sep="\t")
