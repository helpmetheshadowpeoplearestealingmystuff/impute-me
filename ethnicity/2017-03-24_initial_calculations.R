


# qsub -I -W group_list=allelic_imbalance  -l nodes=1:ppn=1,mem=110gb,walltime=36000
rm(list=ls())
library(made4)?
basedir <-"/home/people/lasfol/downloadBulk/misc_downloads/2017-03-24_human_genome_diversity/"
d1<-read.table(paste0(basedir,"HGDP_FinalReport_Forward.txt.gz"),sep="\t",header=T,row.names=1,stringsAsFactors=FALSE)


#just remove all that have any missing
for(col in colnames(d1)){
  print(col)
  d1[d1[,col]%in%c("--","-"),col]<-NA	
}
missing<-apply(is.na(d1),1,sum)
sum(missing==0) / nrow(d1)
d3<-d1[missing==0,]


#convert to numeric
d4<-apply(d3,1,function(x){as.numeric(as.factor(x))})
rownames(d4) <- colnames(d3)
rm(list=c("d1"))

#transpose and mean-scale
d5 <- t(d4)
d6 <- d5 - apply(d5,1,mean,na.rm=T)
rm(list=c("d4","d5"))

#run PCA
d7 <- ord(d6, type = "pca")


pc_samples<-d7[["ord"]][["c1"]][,1:100]
pc_snps<-d7[["ord"]][["l1"]][,1:100]
save(pc_samples,file="2017-03-25_pca_of_HGDB_samples_large.rdata")
save(pc_snps,file="2017-03-25_pca_of_HGDB_snps_large.rdata")


pc_samples<-d7[["ord"]][["c1"]][,1:10]
pc_snps<-d7[["ord"]][["l1"]][,1:10]
save(pc_samples,file="2017-03-25_pca_of_HGDB_samples_small.rdata")
save(pc_snps,file="2017-03-25_pca_of_HGDB_snps_small.rdata")


pc_snps<-pc_snps[order(pc_snps[,1]),]

pdf("test_01.pdf")
plot(log10(pc_samples[,1]),log10(pc_samples[,2]))
dev.off()


#Ok - so we can make a fairly ok PCA of the HGVP data. Nice. Strategy is now to get the most informative SNPs into a quick signature for ancestry.


#try to just get the top-X of each of the first two components, X being say 10000 to start with (we'll cut it down later, pruning LD snps etc)
top <- 10000
pc_snps<-pc_snps[order(pc_snps[,1]),]
pc1_snps<-rownames(pc_snps)[1:top]
pc_snps<-pc_snps[order(pc_snps[,2]),]
pc2_snps<-rownames(pc_snps)[1:top]
snps<-unique(c(pc1_snps,pc2_snps))


#remove SNPs closer than 1MB (because of LD)
SNPlist<-data.frame(row.names=snps)
load("Important R-images and cel files/2016-11-22 myfunctions.rdata")
SNPlist<-fun_check_position_of_snp_20100412(SNPlist,verbose=F)
SNPlist<-SNPlist[!SNPlist[,"chr_name"]%in%c("chrX","chrNA"),] #remove unknowns and sex-chr
SNPlist<-SNPlist[order(as.numeric(sub("^chr","",SNPlist[,"chr_name"])),SNPlist[,"chrom_start"]),]
SNPlist[,"remove"] <- FALSE
for(i in 2:nrow(SNPlist)){
  notToClose<-TRUE
  j <- i
  while(notToClose){
    j <- j -1
    if(j == 0)break
    close <- abs(SNPlist[j,"chrom_start"] - SNPlist[i,"chrom_start"]) < 1e6
    if(!close){
      notToClose <- FALSE
    }else{
      if(!SNPlist[j,"remove"]){
        SNPlist[i,"remove"] <- TRUE
      }
    }
  }
}
SNPlist<-SNPlist[!SNPlist[,"remove"],]
SNPlist[,"remove"]<-NULL
#from 18k to 2k... ok



#re-calculate PC loadings on these SNPs
e1<-d6[rownames(SNPlist),]
e2 <- ord(e1, type = "pca")
e3<-e2[["ord"]][["l1"]][,1:5]
SNPlist <- cbind(SNPlist, e3[rownames(SNPlist),] )

#enter the allele types
SNPlist[,"alleles"]<-apply(d3[rownames(SNPlist),],1,function(x){paste(sort(unique(unlist(strsplit(x,"")))),collapse="/")})



#Save the PC components as well
hgvp_components<-e2[["ord"]][["c1"]][,1:5]

save(SNPlist,hgvp_components,file="2017-03-25_ethnicity.rdata")







#creating a final list of 1000 best ethnicity-PCA SNPs
# rm(list=ls())
# basedir <-"/home/people/lasfol/downloadBulk/misc_downloads/2017-03-24_human_genome_diversity/"
# load("2017-03-26_raw_data_subset.rdata")
# load("2017-03-25_ethnicity.rdata")





# 
# 
# 
# 
# testPerson <- d1[rownames(SNPlist),5,drop=FALSE]
# testPerson["rs2832017",1]<-"TT"
# testPerson["rs1320565",1]<-"CC"
# genotypes<-testPerson
# save(genotypes,testPerson,file="testPerson.rdata")
# 
# d1<-d1[rownames(SNPlist),]
# save(d1, file="2017-03-26_raw_data_subset.rdata")
# 
# 
# 
# 
# 
# 
# rm(list=ls())
# basedir <-"/home/people/lasfol/downloadBulk/misc_downloads/2017-03-24_human_genome_diversity/"
# load("2017-03-26_raw_data_subset.rdata")
# load("2017-03-25_ethnicity.rdata")
# 
# 
# 
# load("2017-03-25_pca_of_HGDB_snps_small.rdata")
# 
# 
# 
# load("testPerson.rdata")
# 
# 
# genotypes<-testPerson
# 
# who <- 5
# 
# 
# 
# d4<-apply(d1,1,function(x){as.numeric(as.factor(x))})
# rownames(d4) <- colnames(d1)
# pca <- prcomp(d4,center = TRUE, scale. = TRUE)
# 
# 
# 
# who <- 5
# pdf("test_09.pdf")
# plot(pca$x[,1],pca$x[,2])
# 
# points(pca$x[who,1],pca$x[who,2],pch=19,col="blue",cex=2)
# 
# plot(pca$x[,1],pca$x[,2])
# testPerson1<-t(d4[who,,drop=T])
# s<-scale(testPerson1, pca$center, pca$scale) %*% pca$rotation 
# points(s[1],s[2],pch=19,col="red",cex=2)
# 
# dev.off()
# 













#Do this on 1kgenomes data instead - but stick to these 1000 most ancestry informative markers from HGDP
rm(list=ls())
load("2017-03-25_ethnicity.rdata")
path_1kg<-"/home/people/lasfol/downloadBulk/annotation/1000_genomes_20130502/"


#cutting down SNPlist a little, only need top-1000, and also, don't have any use for those HGVD loadings.
for(i in c("RS1","RS2","RS3","RS4","RS5","alleles")){SNPlist[,i]<-NULL}


genotype_info<-list()

for(chr in rev(sub("chr","",sort(unique(SNPlist[,"chr_name"]))))){
  g1<-SNPlist[SNPlist[,"chr_name"]%in%paste0("chr",chr),]
  write.table(rownames(g1),file="temp_list_of_snps.txt",sep="\t",col.names=F,row.names=F,quote=F)
  
  if(chr =="X"){
    vcf_path<-paste0(path_1kg,"ALL.chrX.phase3_shapeit2_mvncall_integrated_v1b.20130502.genotypes.vcf.gz")
  }else{
    vcf_path<-paste0(path_1kg,"ALL.chr",chr,".phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz")
  }
  #bulk extract to vcf
  cmd1<-paste0("vcftools --snps temp_list_of_snps.txt --gzvcf ",vcf_path," --out chr",chr," --recode --recode-INFO-all")
  system(cmd1)

  #remove the multi-allelics  
  cmd2<-paste0("sed -i.bak '/MULTI_ALLELIC/d' chr",chr,".recode.vcf")
  system(cmd2)
  
  
  #get the 012 format files
  cmd2<-paste0("vcftools --snps temp_list_of_snps.txt --vcf chr",chr,".recode.vcf --out tab_chr",chr," --012 ")
  system(cmd2)

  #get the snp info directly
  cmd3 <- paste0("cut -f 3,4,5 chr",chr,".recode.vcf")
  snp_info<-grep("^[#I]",system(cmd3,inter=T),value=T,invert=T)
  snps<-sapply(strsplit(snp_info,"\t"),function(x){x[1]})
  SNPlist[snps,"ref"]<-sapply(strsplit(snp_info,"\t"),function(x){x[2]})
  SNPlist[snps,"alt"]<-sapply(strsplit(snp_info,"\t"),function(x){x[3]})

  genotypes<-read.table(paste0("tab_chr",chr,".012"),sep="\t",row.names=1)
  rownames(genotypes)<-read.table(paste0("tab_chr",chr,".012.indv"),sep="\t")[,1]
  colnames(genotypes)<-snps
  
  genotype_info[[chr]]<-genotypes
}


save(genotype_info,SNPlist,file="2017-03-26_partway_save.rdata")


# ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/integrated_call_samples_v3.20130502.ALL.panel

