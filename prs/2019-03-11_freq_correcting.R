#Ok fine, start over - now on compute node
# qsub -I -W group_list=ip_10000 -l nodes=1:ppn=1,mem=32gb,walltime=36000

rm(list=ls())

library(stringr)

path_1kg<-"/home/people/lasfol/downloadBulk/annotation/1000_genomes_20130502/"

for(chr in c(1:22,"X")[21:23]){
  print(chr)

  if(chr =="X"){
    vcf_path<-paste0(path_1kg,"ALL.chrX.phase3_shapeit2_mvncall_integrated_v1b.20130502.genotypes.vcf.gz")
  }else{
    vcf_path<-paste0(path_1kg,"ALL.chr",chr,".phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz")
  }
  cmd1<-paste0("vcftools --indv HG00096 --gzvcf ",vcf_path," --out chr",chr," --recode --recode-INFO-all")
  system(cmd1)
  
  cmd2 <- paste0("cut -f 3 chr",chr,".recode.vcf")
  snps<-grep("^[#I]",system(cmd2,inter=T),value=T,invert=T)
  
  cmd3 <- paste0("vcftools --vcf chr",chr,".recode.vcf --get-INFO AF --get-INFO EAS_AF --get-INFO AMR_AF --get-INFO AFR_AF --get-INFO EUR_AF --get-INFO SAS_AF --out chr",chr)
  system(cmd3)
  
  result<-read.table(paste0("chr",chr,".INFO"),sep="\t",header=T,stringsAsFactors = F)
  
  if(any(duplicated(snps))){
    print("found duplication. Fixing")
    result<-result[!duplicated(snps),]
    snps<-snps[!duplicated(snps)]
  }
  
  rownames(result)<-result[,"SNP"] <- snps
  result[,"NCHROBS"] <- 2504
  
  
  #renaming according to plink frq
  colnames(result)[colnames(result)%in%"CHROM"] <- "CHR"
  colnames(result)[colnames(result)%in%"REF"] <- "A2"
  colnames(result)[colnames(result)%in%"ALT"] <- "A1"
  colnames(result)[colnames(result)%in%"AF"] <- "GLOBAL_AF"
  
  #removing tri-allelic
  result<- result[grep(",",result[,"A1"],invert=T),]
  
  #This is really weird but plink only accepts A1 as 0
  result[,"A1"] <- "0"
  
  
  
  
  for(ethnicity in c("GLOBAL","EAS","AMR","AFR","EUR","SAS")){
    
    result[,"MAF"]<- result[,paste0(ethnicity,"_AF")]  
    out <- result[,c("CHR","SNP","A1","A2","MAF","NCHROBS")]
    
    
    
    rownames(out) <- NULL
    out <- rbind(colnames(out),out)
    
    #adding padding according to plink format    
    char_lengths <- c(4, 22,  5,  5, 13,  9)
    for(j in 1:ncol(out)){
      out[,j]<-str_pad(out[,j], char_lengths[j], side='left', pad=' ')
    }
    
    
    filename <- paste0("2019-03-11_chr",chr,"_",ethnicity,"_freq.txt.gz")  
    f<-gzfile(filename,"w")
    write.table(out,file=f,sep="",col.names=F,row.names=F,quote=F)
    close(f)
    

  }
}
  
  
  