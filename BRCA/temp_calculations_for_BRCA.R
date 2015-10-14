
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

# calls

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



missing<-snps[!snps%in%consequence[,"refsnp_id"]]

grep("chr17:41275798",missing)
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


#but we also need to get these three that are special for the 23andme array
extras<-data.frame(
	SNP=c("i4000377","i4000378","i4000379"),
	chrom_start=c(NA,NA,NA),
	chr_name=c("input","input","input"),
	polyphen_prediction=c(NA,NA,NA),
	sift_prediction=c(NA,NA,NA),
	consequence_type_tv=c(NA,NA,NA)
)
consequence[,"chr_name"]<-as.character(consequence[,"chr_name"])
out<-rbind(extras,consequence)

write.table(out,file="BRCA/SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F,sep="\t")
















#2015-10-14 checking the normal in all current data sets
rm(list=ls())

source("/srv/shiny-server/gene-surfer/functions.R")

BRCA_table_file <-"/srv/shiny-server/gene-surfer/BRCA/SNPs_to_analyze.txt"
BRCA_table<-read.table(BRCA_table_file,sep="\t",header=T,stringsAsFactors=F)

rownames(BRCA_table)<-BRCA_table[,"SNP"]
# BRCA_table[BRCA_table[,"chr_name"]%in%13,"gene"]<-"BRCA2"
# BRCA_table[BRCA_table[,"chr_name"]%in%17,"gene"]<-"BRCA1"
# 
# BRCA_table["i4000377","gene"]<-"BRCA1"
# BRCA_table["i4000378","gene"]<-"BRCA1"
# BRCA_table["i4000379","gene"]<-"BRCA2"
# 
# BRCA_table["i4000377","consequence_type_tv"]<-"Direct from 23andme"
# BRCA_table["i4000378","consequence_type_tv"]<-"Direct from 23andme"
# BRCA_table["i4000379","consequence_type_tv"]<-"Direct from 23andme"


uniqueIDs<-list.files("/home/ubuntu/data")
for(uniqueID in uniqueIDs){
	#get genotypes and calculate gheight
	genotypes<-try(get_genotypes(uniqueID=uniqueID,request=BRCA_table))
	# BRCA_table[,"Your genotype"]<-genotypes[rownames(BRCA_table),]
	if(class(genotypes)=="try-error")next
	colnames(genotypes)[1]<-uniqueID
	BRCA_table<-cbind(BRCA_table, genotypes[rownames(BRCA_table),,drop=F])
}



people<-grep("^id_",colnames(BRCA_table),value=T)
people<-people[!people%in%"id_860342AX5"]
overview<-apply(BRCA_table[,people],1,table)


differences<-sort(sapply(overview,length))


#too high frequency to include these
omitThese<-names(differences)[differences>=2]
BRCA_table[omitThese,]


BRCA_table<-BRCA_table[!rownames(BRCA_table)%in%omitThese,]


normals<-apply(BRCA_table[,people],1,unique)
BRCA_table[,"normal"]<-unlist(lapply(normals,function(x){x[!is.na(x)]}))

BRCA_table<-BRCA_table[,grep("^id",colnames(BRCA_table),invert=T)]

BRCA_table_file <-"/srv/shiny-server/gene-surfer/BRCA/SNPs_to_analyze.txt"
BRCA_table<-write.table(BRCA_table,file="SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F,sep="\t")




