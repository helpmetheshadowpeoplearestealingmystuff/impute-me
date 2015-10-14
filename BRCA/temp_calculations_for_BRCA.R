
# genes<-c("BRCA2","BRCA1")
# 
# library(biomaRt)
# gene_mart <- useMart(biomart="ENSEMBL_MART_ENSEMBL", host="grch37.ensembl.org", path="/biomart/martservice" ,dataset="hsapiens_gene_ensembl")
# attributes<-c("start_position","end_position","chromosome_name","hgnc_symbol")
# query_gene <- getBM(attributes = attributes,
# 										filters = "hgnc_symbol", values = genes,mart = gene_mart)
# query_gene<-query_gene[nchar(query_gene[,"chromosome_name"])%in%1:2,]


query_gene<-data.frame(
	start_position=c(41196312,32889611),
end_position=c(41277500,32973805),
chromosome_name=c(17,13),
row.names=c("BRCA1","BRCA2")
)


uniqueID<-"id_860342AX5"

calls<-list()
for(gene in row.names(query_gene)){
	chr<-query_gene[gene,"chromosome_name"]
	start<-query_gene[gene,"start_position"]
	end<-query_gene[gene,"end_position"]
	fileName<-paste(uniqueID,"_chr",chr,".gen",sep="")
	gen<-read.table(fileName,sep=" ",stringsAsFactors=F)
	gen<-gen[gen[,3]>start &  gen[,3]<end ,]
	calls[[gene]]<-gen
}

save(calls,file="2015-10-14_BRCA_calls.rdata")


#work with this file locally
rm(list=ls())
load("../2015-10-14_BRCA_calls.rdata")

library(biomaRt)
snpmart <- useMart("snp", dataset="hsapiens_snp")

snps<-vector()
for(gene in names(calls)){
	snps<-c(snps,calls[[gene]][,2])
	
}


consequence<- getBM(c('refsnp_id','chrom_start','chr_name',"polyphen_prediction","sift_prediction",'consequence_type_tv'),
										filters = c('snp_filter'),
										values = snps,
										mart = snpmart,verbose=F)



table(consequence[,"consequence_type_tv"])

# 3_prime_UTR_variant                5_prime_UTR_variant 
# 105                                 24 
# coding_sequence_variant            downstream_gene_variant 
# 1                                712 
# intron_variant                   missense_variant 
# 1635                                139 
# NMD_transcript_variant non_coding_transcript_exon_variant 
# 1100                                152 
# non_coding_transcript_variant              splice_region_variant 
# 698                                 14 
# start_lost                        stop_gained 
# 1                                  3 
# synonymous_variant              upstream_gene_variant 
# 53                                620 

keep1<-consequence[,"consequence_type_tv"]%in%c("stop_gained","start_lost","missense_variant","coding_sequence_variant")


table(consequence[,"polyphen_prediction"])
# benign possibly damaging probably damaging           unknown 
# 5107               121                43                43                12
keep2<-consequence[,"polyphen_prediction"]%in%"probably damaging"

table(consequence[,"sift_prediction"])
# deleterious deleterious - low confidence 
# 5111                           84                           11 
# tolerated   tolerated - low confidence 
# 114                            6 
keep3<-consequence[,"polyphen_prediction"]%in%"deleterious"


consequence<-consequence[keep1 | keep2 | keep3,]

consequence[,"polyphen_prediction"]<-factor(consequence[,"polyphen_prediction"],levels=c("","probably damaging","possibly damaging","unknown","benign"))

consequence<-consequence[order(consequence[,"polyphen_prediction"]),]


head(consequence)
colnames(consequence)[1]<-c("SNP")


duplicates<-consequence[duplicated(consequence[,"SNP"]),"SNP"]
for(duplicate in duplicates){
	print(nrow(consequence))
	w<-which(consequence[,"SNP"]%in%duplicate)
	consequence[w,]
	omit<-w[order(consequence[w,"polyphen_prediction"])[2:length(w)]]
	consequence<-consequence[!(1:nrow(consequence))%in%omit,]
	print(nrow(consequence))
	
}



write.table(consequence,file="BRCA/SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F,sep="\t")
