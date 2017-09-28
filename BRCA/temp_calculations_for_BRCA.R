
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



#addressing the non rs-ID ones
check<-sub("\\:[ID]$","",missing)
check<-grep("^rs",check,invert=T,value=T)
cat(paste(sub("\\:"," ",check),1+as.numeric(sub("chr..\\:","",check))," ",check,"\n"))
length(check)

write.table(out,file="BRCA/SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F,sep="\t")
















#2015-10-14 checking the normal in all current data sets
rm(list=ls())

source("/home/ubuntu/srv/impute-me/functions.R")

BRCA_table_file <-"/home/ubuntu/srv/impute-me/BRCA/SNPs_to_analyze.txt"
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

BRCA_table_file <-"/home/ubuntu/srv/impute-me/BRCA/SNPs_to_analyze.txt"
BRCA_table<-write.table(BRCA_table,file="SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F,sep="\t")
















#2017-09-28 checking for overlap with other browser sources. Nice to see if the two lists have approximately same content
rm(list=ls())
d1 <- read.table("BRCA/SNPs_to_analyze.txt",sep="\t",stringsAsFactors = F,header=T,row.names=1)

snps<-c("rs80357629","rs80358145","rs80357477","rs80357558","rs80357055","rs80357069","rs80357284","rs80357590","rs41293463","rs80357823","rs80357463","rs80358150","rs80357906","rs80357925","rs45553935","rs80357975","rs80357347","rs80358089","rs80358053","rs80187739","rs80357061","rs80357623","rs80357862","rs80358086","rs80358087","rs80359876","rs80356862","rs80357641","rs80357433","rs80356988","rs80358063","rs80357389","rs80357854","rs273900730","rs80357916","rs80358027","rs80357977","rs80357981","rs80357071","rs80357787","rs80357259","rs80357804","rs80358070","rs80358178","rs80357508","rs80357711","rs80357021","rs80357318","rs80357254","rs80357889","rs80357993","rs80357848","rs80357520","rs80357868","rs80357609","rs80357162","rs80357902","rs80357729","rs80357980","rs62625308","rs80357877","rs80357509","rs80357808","rs80357018","rs80357405","rs80357945","rs80357903","rs80357624","rs80357635","rs80357511","rs80357161","rs80357846","rs80357601","rs80357829","rs80357115","rs80357832","rs80357035","rs80357717","rs80357971","rs80357596","rs80357251","rs80356925","rs80357131","rs80357607","rs80357970","rs80357669","rs80357524","rs80357664","rs80357786","rs80357583","rs80357654","rs80356875","rs80357233","rs80357880","rs80357688","rs80357853","rs80357522","rs80357355","rs80357526","rs80356898","rs80357600","rs80357662","rs80357908","rs80357888","rs80357010","rs80357801","rs273897659","rs80357714","rs80357969","rs80357597","rs80359874","rs80357612","rs80357774","rs80357569","rs80357618","rs80359872","rs80357292","rs80357844","rs80357724","rs80357321","rs80357747","rs80357941","rs80358047","rs80357887","rs80356991","rs80357604","rs80358011","rs80358061","rs80358163","rs80358042","rs80357382","rs80357637","rs80358158","rs80357498","rs80357443","rs80357783")


d2<-data.frame(matrix(ncol=ncol(d1),nrow=length(snps),dimnames=list(snps,colnames(d1))),stringsAsFactors = F)
d<-rbind(d1,d2)


#rename old columns for comparison
colnames(d) <- paste0(colnames(d),"_old")




#then re-query biomart
library(biomaRt)
snpmart <- useMart("ENSEMBL_MART_SNP", dataset="hsapiens_snp")
consequence<- getBM(c('refsnp_id','chrom_start','chr_name',"polyphen_prediction","sift_prediction",'consequence_type_tv'),
                    filters = c('snp_filter'),
                    values = rownames(d),
                    mart = snpmart,verbose=F)


#omit the least serious of double lines
order1<- c("probably damaging","possibly damaging","benign","" )
consequence[,"polyphen_prediction"] <- factor(consequence[,"polyphen_prediction"],levels=order1)
colnames(consequence)[1]<-c("SNP")
unique(consequence[,"SNP"])

duplicates<-consequence[duplicated(consequence[,"SNP"]),"SNP"]
for(duplicate in unique(duplicates)){
  w<-which(consequence[,"SNP"]%in%duplicate)
  omit<-w[order(consequence[w,"polyphen_prediction"])[2:length(w)]]
  consequence<-consequence[!(1:nrow(consequence))%in%omit,]
  print(nrow(consequence))
  
}
rownames(consequence)<-consequence[,"SNP"]




#then also query myvariant / clinvar
library(myvariant)
res <- queryVariants(q=rownames(d), scopes="dbsnp.rsid", fields="clinvar")


order2<-c('Pathogenic','Likely pathogenic','Conflicting interpretations of pathogenicity','Uncertain significance','not provided','Likely benign','Benign')


clinvar<-sapply(res[,"clinvar.rcv"],function(x,order){
  if("clinical_significance"%in%colnames(x)){
    f1<-unique(x[,"clinical_significance"])
    f1<-sub(", *not provided *","",f1)
    if(any(!f1%in%order))stop(paste0("missing value '",paste(f1,collapse="','"),"'"))
    f2<-factor(f1,levels=order)
    out<-as.character(sort(f2)[1])
  }else{
    out<-NA
  }
  return(out)
  },order2
)

names(clinvar)<-res[,"clinvar.rsid"]


save(res,consequence,file="BRCA/2017-09-29_query_backup.rdata")

d<-cbind(consequence[rownames(d),],clinvar[rownames(d)],d)


d[,"consequence_type_tv_old"] == d[,"consequence_type_tv"]
#good!


colnames(d)[colnames(d)%in%"clinvar[rownames(d)]"]<-"clinvar"
d<-d[,grep("old$",colnames(d),invert=T)]


rownames(d)[1:3] <- rownames(d1)[1:3]


d[1:3,"clinvar"] <- 'Pathogenic'
d[1:3,"chr_name"] <- 'input'



d[,"SNP"]<-rownames(d)
d<-d[, c("SNP","chrom_start" ,"chr_name", "clinvar"  ,"polyphen_prediction" ,"sift_prediction"    , "consequence_type_tv")]

BRCA_table_file <-"BRCA/SNPs_to_analyze.txt"
BRCA_table<-write.table(d,file=BRCA_table_file,col.names=T,row.names=F,quote=F,sep="\t")


head(d)

























#2017-09-28 checking the normal in all current data sets
rm(list=ls())
source("/home/ubuntu/srv/impute-me/functions.R")

BRCA_table_file <-"SNPs_to_analyze.txt"
BRCA_table<-read.table(BRCA_table_file,sep="\t",header=T,stringsAsFactors=F,row.names=T)

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

BRCA_table_file <-"/home/ubuntu/srv/impute-me/BRCA/SNPs_to_analyze.txt"
BRCA_table<-write.table(BRCA_table,file="SNPs_to_analyze.txt",col.names=T,row.names=F,quote=F,sep="\t")



