

d<-read.table("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/BasicInsilicoReport/SNPs_to_analyze.txt",sep="\t",stringsAsFactors = F)
colnames(d)<-c("category","gene","SNP","Description","Source","Status","X","link")




library(biomaRt)
snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
attributes<-c("refsnp_id","chr_name","chrom_start","allele")
listFilters(snp_mart)
query<-getBM(attributes, filters = c("snp_filter"), values = d[,"SNP"], mart = snp_mart)
query<-query[nchar(query[,"chr_name"])%in%1:2,]
rownames(query)<-query[,"refsnp_id"]

d[,"ensembl_alleles"]<-query[rownames(d),"allele"]
d[,"chr_name"]<-query[d[,"SNP"],"chr_name"]

write.table(d,"C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/gene-surfer/BasicInsilicoReport/SNPs_to_analyze.txt",sep="\t",col.names=NA)
