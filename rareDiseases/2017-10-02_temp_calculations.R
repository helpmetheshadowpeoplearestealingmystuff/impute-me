# 2017-10-02_temp_calculations


d<-read.table("rareDiseases/SNPs_to_analyze.txt",sep="\t",header=T,stringsAsFactors = F)


head(d)

library(biomaRt)
snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
attributes<-c("refsnp_id","chr_name","chrom_start")
query<-getBM(attributes, filters = c("snp_filter"), values = d[,"SNP"], mart = snp_mart)
query<-query[nchar(query[,"chr_name"])%in%1:2,]
rownames(query)<-query[,"refsnp_id"]


#replacing name
d[,"chr_name"] <- NA #all was input before
d[,"chr_name"]<-query[d[,"SNP"],"chr_name"]
d[grep("^i",d[,"SNP"]),"chr_name"]<-"input"

#check missing
d[is.na(d[,"chr_name"]),]

#these two won't be updated anyway
d["rs28939383","chr_name"] <- "input"
d["rs28934899","chr_name"] <- "input"


write.table(d,"rareDiseases/SNPs_to_analyze.txt",sep="\t",row.names=F,col.names=T,quote=F)

