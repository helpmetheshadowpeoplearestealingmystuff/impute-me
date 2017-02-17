source("/home/ubuntu/srv/impute-me/functions.R")


# library(biomaRt)
# snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
# 
# 
# listFilters(snp_mart)
# 
# SIFT score <=
# 	PolyPhen score >=
# 	
# 	
# consequence<- getBM(c('refsnp_id','chrom_start','chr_name',"polyphen_prediction","sift_prediction",'consequence_type_tv'),
# 										filters = c('snp_filter'),
# 										values = snps,
# 										mart = snpmart,verbose=F)
# 



./annotate_variation.pl -downdb 1000g2015aug humandb -buildver hg19

./annotate_variation.pl -filter -dbtype ALL.sites.2015_08 -buildver hg19 -out ex1 example/ex1.avinput humandb/
	
	
	
	# 	
	# wget ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz
	# 
	# 
	# ./annotate_variation.pl -filter -dbtype ALL.sites.2015_08 -buildver hg19 -out ex2 humandb/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf humandb/
	# 
	# ./annotate_variation.pl -filter -dbtype vcf -buildver hg19 -out ex2 humandb/ALL.chr22.phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf humandb/
	# 	

# 	
# t<-read.table("hg19_EUR.sites.2015_08.txt",sep="\t",stringsAsFactors=F,nrow=10000)
# 
# 
# head(t[order(t[,5],decreasing=T),])
# 


#Convert from this
rs149201999     22      16050408        TT
rs146752890     22      16050612        NN
rs139377059     22      16050678        NN
rs188945759     22      16050984        CC


#to this
7 92570705 92570705 T C 7 3 43 D G SAMD9 1.56
7 98870495 98870495 G A 26 16 62 R C PTCD1 3.06
7 99835402 99835402 C T 13 6 46 P L PILRA 1.75
7 100122289 100122289 - CCT 5 3 60 EQ ERQ GIGYF1 3.98




./annotate_variation.pl -downdb 1000g2015aug humandb -buildver hg19


awk '{ print $2 "\t" $3 "\t" $3 "\t" substr($4,1,1) "\t" substr($4,2,2) "\t" $1}' id_3700776I4_chr22.23andme.txt  > test_out.txt

./annotate_variation.pl -filter -dbtype ALL.sites.2015_08 -buildver hg19 -out ex1 ~/2015-11-20_temp_nonsenser/test_out.txt humandb/
	
	
	
	
	./annotate_variation.pl -downdb -buildver hg19 1000g2012feb humandb
./annotate_variation.pl -filter -dbtype 1000g2012feb_all -out ex5 -buildver hg19 humandb/test_out.txt humandb


./annotate_variation.pl -downdb -webfrom annovar -buildver hg19 dbnsfp30a humandb

./table_annovar.pl humandb/test_out.txt humandb/ -protocol dbnsfp30a -operation f -build hg19 -nastring .


./annotate_variation.pl -filter -dbtype ljb23_pp2hvar -buildver hg19 -out ex1 example/ex1.avinput humandb/ -otherinfo








#restart
#These two seemed important to downlaod
./annotate_variation.pl -downdb -webfrom annovar -buildver hg19 dbnsfp30a humandb
./annotate_variation.pl -downdb -buildver hg19 1000g2012feb humandb

table_annovar=~/downloads/annovar/table_annovar.pl
humandb=~/downloads/annovar/humandb/
	
	for i in {22..1}
do
echo $i
awk '{ print $2 "\t" $3 "\t" $3 "\t" substr($4,1,1) "\t" substr($4,2,2) "\t" $1}' id_3700776I4_chr$i.23andme.txt  > out_chr$i.txt
$table_annovar out_chr$i.txt $humandb -protocol dbnsfp30a -operation f -build hg19 -nastring . -otherinfo
awk '{if($6 != ".") print}' out_chr$i.txt.hg19_multianno.txt  > out_short_chr$i.txt
done



for i in {22..1}
do
tail -n +2 out_short_chr$i.txt > out_short_no_head_chr$i.txt
done
head -n 1 out_short_chr22.txt > header.txt


paste("cat header.txt",paste(paste("out_short_no_head_chr",22:1,".txt",sep=""),collapse=" "),"> all_coding.txt")



coding_snps<- read.table("all_coding.txt",sep="\t",stringsAsFactors=F,header=T,na.strings=".")
coding_snps<-coding_snps[order(as.numeric(coding_snps[,"SIFT_score"],decreasing=T)),]

coding_snps[,"Alt"]<-NULL
coding_snps[,"Ref"]<-NULL
coding_snps<-coding_snps[!duplicated(coding_snps[,"Otherinfo"]),] #it's just 39
rownames(coding_snps)<-coding_snps[,"Otherinfo"]
coding_snps[,"Otherinfo"]<-NULL



save(coding_snps, file="2015-11-20_all_coding_SNPs.rdata")
table(coding_snps[coding_snps[,"SIFT_pred"]%in%"D","Chr"])

t<-coding_snps[coding_snps[,"SIFT_pred"]%in%"D" & coding_snps[,"Chr"]%in%1,]
#odd that so many are on chr1

scp lasfol@computerome.cbs.dtu.dk:/home/people/lasfol/2015-11-20_temp_nonsenser/2015-11-20_all_coding_SNPs.rdata /home/ubuntu/misc_files/
	
	
	#but now we have a list of genotyped missense and nonsense mutation. Nice.
	load("2015-11-20_all_coding_SNPs.rdata")














#restart II 2015-11-21
#These two seemed important to downlaod
qsub -I -W group_list=allelic_imbalance  -l nodes=1:ppn=1,mem=16gb,walltime=36000

./annotate_variation.pl -downdb -webfrom annovar -buildver hg19 dbnsfp30a humandb
./annotate_variation.pl -downdb -buildver hg19 1000g2012feb humandb

table_annovar=~/downloads/annovar/table_annovar.pl
humandb=~/downloads/annovar/humandb/
	
	cd /home/people/lasfol/2015-11-23_temp_nonsenser


for i in {22..1}
do
echo $i
awk '{ print $2 "\t" $3 "\t" $3 "\t" substr($4,1,1) "\t" substr($4,2,2) "\t" $1}' id_57n662948_chr$i.23andme.txt  > out_chr$i.txt
$table_annovar out_chr$i.txt $humandb -protocol dbnsfp30a -operation f -build hg19 -nastring . -otherinfo
awk '{if($6 != ".") print}' out_chr$i.txt.hg19_multianno.txt  > out_short_chr$i.txt
done



for i in {22..1}
do
tail -n +2 out_short_chr$i.txt > out_short_no_head_chr$i.txt
done
head -n 1 out_short_chr22.txt > header.txt


paste("cat header.txt",paste(paste("out_short_no_head_chr",22:1,".txt",sep=""),collapse=" "),"> all_coding.txt")



coding_snps<- read.table("all_coding.txt",sep="\t",stringsAsFactors=F,header=T,na.strings=".")
coding_snps<-coding_snps[order(as.numeric(coding_snps[,"SIFT_score"],decreasing=T)),]

coding_snps[,"Alt"]<-NULL
coding_snps[,"Ref"]<-NULL
coding_snps<-coding_snps[!duplicated(coding_snps[,"Otherinfo"]),] #it's just 39
rownames(coding_snps)<-coding_snps[,"Otherinfo"]
coding_snps[,"Otherinfo"]<-NULL

table(coding_snps[,"SIFT_pred"])
colnames(coding_snps)[1]<-"chr_name"





save(coding_snps, file="2015-11-23_all_coding_SNPs.rdata")



table(coding_snps[coding_snps[,"SIFT_pred"]%in%"D","Chr"])

t<-coding_snps[coding_snps[,"SIFT_pred"]%in%"D" & coding_snps[,"Chr"]%in%1,]
#odd that so many are on chr1

scp lasfol@computerome.cbs.dtu.dk:/home/people/lasfol/2015-11-23_temp_nonsenser/2015-11-23_all_coding_SNPs.rdata /home/ubuntu/misc_files/
	
	
	#but now we have a list of genotyped missense and nonsense mutation. Nice.
load("2015-11-20_all_coding_SNPs.rdata")







# 
# load("2015-11-20_all_coding_SNPs.rdata")
# uniqueID<-"id_57n662948"
# request<-coding_snps
# namingLabel<-"cached.nonsenser"
# 
# genotypes<-get_genotypes(uniqueID,request,namingLabel=namingLabel)
# 
#Ok main function updated. Now the main-crawler also gets a separate file with all 8000 non/missense SNPs. Nice. let's analyze them,


rm(list=ls())
source("/home/ubuntu/srv/impute-me/functions.R")

uniqueIDs<-list.files("/home/ubuntu/data/")
load("/home/ubuntu/srv/impute-me/nonsenser/2015-11-23_all_coding_SNPs.rdata")
for(uniqueID in uniqueIDs){
	g1<-get_genotypes(uniqueID,coding_snps,namingLabel="cached.nonsenser")
	g2<- g1[rownames(coding_snps),"genotype"]
	coding_snps[,paste(uniqueID,"A1",sep="_")]<-sub("/.+$","",g2)
	coding_snps[,paste(uniqueID,"A2",sep="_")]<-sub("^.+/","",g2)
}

cols<-grep("^id",colnames(coding_snps),value=T)

a<-apply(coding_snps[,cols],1,table)


normalAllele<-sapply(a, function(x){
	if(length(x)==1){
		return(names(x))
	}else if(length(x)==2){
		if(sort(x)[1] / sum(x) > 0.2){ #don't return a 'normal' if MAF is above 0.2
			return("")	
		}else{
			return(names(x)[2])
		}
	}else{stop("Whaat!")}
})


save(normalAllele,file="2015-12-16 majorAllele.rdata")








rm(list=ls())
load("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/nonsenser/2015-11-23_all_coding_SNPs.rdata")

load("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/nonsenser/2015-12-16 majorAllele.rdata")

freq<-read.table("C:/Users/FOLK/Documents/Work/Bioinformatics/2015-08-17_gene_surfer/nonsenser/2015-12-16_SNAPResults.txt",sep="\t",row.names=1,header=T)

coding_snps[,"Frequency"]<-freq[rownames(coding_snps),"MAF"]
coding_snps[,"Common allele"]<-normalAllele[rownames(coding_snps)]
save(coding_snps, file="2015-12-16_all_coding_SNPs.rdata")
