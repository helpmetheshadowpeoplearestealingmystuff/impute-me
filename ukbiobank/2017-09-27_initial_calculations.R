


rm(list=ls())
library(openxlsx)
phenosummary_path<-"/home/people/lasfol/gitStuff/2015-08-17_imputeme/ukbiobank/phenosummary_final_11898_18597.xlsx"
phenosummary<-read.xlsx(phenosummary_path)
manifest_path <- "/home/people/lasfol/gitStuff/2015-08-17_imputeme/ukbiobank/UKBB GWAS Manifest 20170915.xlsx" 
manifest<-read.xlsx(manifest_path)

outdir <- "/home/people/lasfol/temp_ukbiobank/"

if(!file.exists(outdir))dir.create(outdir)





for(i in 1:nrow(phenosummary)){
  
  code<-phenosummary[i,"Field.code"]
  field<-phenosummary[i,"Field"]
  
  cases<- suppressWarnings(as.numeric(phenosummary[i,"N.cases"]))
  controls<- suppressWarnings(as.numeric(phenosummary[i,"N.controls"]  ))
  
  if(is.na(cases) || is.na(controls) || cases < 1000 ||  controls < 1000){
    # print(paste(i,"-",code,"was skipped because it only had",cases,"cases and",controls,"controls"))
    next
  }
  
  #get the code match (some weird field-mismatch - but this should fix it)
  w1<-which(manifest[,"Phenotype.code"]%in%code)
  if(length(w1)>1){stop("!")}
  if(length(w1)==0){
    w2<-which(manifest[,"Phenotype.code"]%in%sub("^.+_","",code))
    if(length(w2)>1){
      stop("!!")
    }else if(length(w2)==0){
      stop("!!!") 
    }else{
      w<-w2
      # print(paste(manifest[w2,"Phenotype.code"],"and",code))
      description1 <- manifest[w2,"Description"]
      description2 <- phenosummary[i,"Field"]
      if(description1 != description2)stop("!!!")
      #ok - if the descriptions are identical, I think it's safe to also use this mode of manifest file-linking
    }
  }else{
    w<-w1
  }
  
  
  #getting command and file name  
  cmd1<-manifest[w,"wget.command"]
  filename <- paste0(outdir,sub("^.+-O ","",cmd1))
  phenosummary[i,"filename"] <- basename(filename)

  #checking if file already exists (for re-running iterations to prevent download fail)
  if(file.exists(filename)){
    print(paste(i,"-",code,"filename already existed - skipping"))
    next
  }
  
  #executing
  system(cmd1)
  file.rename(sub("^.+-O ","",cmd1),filename)
  # Sys.sleep(10)
}



write.xlsx(phenosummary,phenosummary_path)


# This is how the output looks like. Not completely clear with directionality effects, but I guess one could assume that the variant ID is encoded as chr:pos:ea:nea - and so deduce, e.g. that beta for the first one is 8.56735e-03 _for the C allele_.

# variant rsid    nCompleteSamples        AC      ytx     beta    se      tstat   pval
# 5:43888254:C:T  rs13184706      337030  1.23424e+04     1.46260e+04     8.56735e-03     4.96351e-03     1.72607e+00     8.43366e-02
# 5:43888493:C:T  rs58824264      337030  2.42185e+03     2.83081e+03     -1.07358e-02    1.10639e-02     -9.70347e-01    3.31874e-01
# 5:43888556:T:C  rs72762387      337030  1.64659e+04     1.93439e+04     2.57936e-03     4.30879e-03     5.98628e-01     5.49421e-01
# 5:43888648:C:T  rs115032754     337030  1.35238e+04     1.58908e+04     -8.26102e-04    4.81967e-03     -1.71402e-01    8.63908e-01
# 5:43888690:C:G  rs147555725     337030  1.24868e+03     1.46322e+03     -3.12557e-03    1.58183e-02     -1.97592e-01    8.43365e-01
# 5:43888838:G:C  rs13185925      337030  2.33705e+04     2.75682e+04     5.12503e-03     3.63449e-03     1.41011e+00     1.58509e-01
# 5:43889057:C:T  rs13189727      337030  1.25486e+04     1.48704e+04     8.74687e-03     4.91879e-03     1.77826e+00     7.53629e-02
# 5:43889207:A:G  rs4516856       337030  6.70229e+05     7.88042e+05     1.23049e-03     8.76737e-03     1.40349e-01     8.88385e-01
# 5:43889333:G:T  rs114787943     337030  3.03187e+03     3.58296e+03     1.15600e-03     9.88024e-03     1.17002e-01     9.06859e-01





#checking when there is more than one file
if(unique(table(phenosummary[,"filename"]))!=1)stop("Serious problem with file name matching. Must audit")

#e.g. like this
phenosummary[phenosummary[,"filename"]%in%"20003_1140879760.assoc.tsv.gz",1:5]
manifest[manifest[,"File"]%in%"20003_1140879760.assoc.tsv.gz",]


#double check is file was successfully downloaded
for(i in 1:nrow(phenosummary)){
  if(!is.na(phenosummary[i,"filename"]  )){
    if(!file.exists(paste0(outdir,phenosummary[i,"filename"] ))){
     stop("re-run download loop, file missing")
    }
  }
}













#2017-09-25 trying to iterate through the data and see if we can create good SNP-signatures
#step 1 - just throw away anything that is worse than p>1e-07
rm(list=ls())
library(openxlsx)
phenosummary_path<-"/home/people/lasfol/gitStuff/2015-08-17_imputeme/ukbiobank/phenosummary_final_11898_18597.xlsx"
phenosummary<-read.xlsx(phenosummary_path)
outdir <- "/home/people/lasfol/temp_ukbiobank/"
for(i in 1:nrow(phenosummary)){
  print(paste(i,"of",nrow(phenosummary)))
  if(is.na(phenosummary[i,"filename"]))next
  filename_in <- paste0(outdir,phenosummary[i,"filename"])
  filename_out <- sub(".gz$","",paste0(outdir,sub("assoc","assoc.filter",phenosummary[i,"filename"])))
  cmd1<-paste0("zcat ",filename_in," | awk -F $'\t' '($9 < 1e-07)' - > ",filename_out)
  system(cmd1)
}



#step 2 - prune to 1 MB size (i.e take strongest SNP, then remove everything within 1 MB. Rinse repeat.)
rm(list=ls())
library(openxlsx)
phenosummary_path<-"/home/people/lasfol/gitStuff/2015-08-17_imputeme/ukbiobank/phenosummary_final_11898_18597.xlsx"
phenosummary<-read.xlsx(phenosummary_path)
outdir <- "/home/people/lasfol/temp_ukbiobank/"

f<-c('chr','pos','a1','a2','variant','rsid','nCompleteSamples','AC','ytx','beta','se','tstat','pval','code','field')
results<-data.frame(matrix(nrow=0,ncol=length(f),dimnames=list(NULL,f)),stringsAsFactors = F)

for(i in 1:nrow(phenosummary)){
  if(is.na(phenosummary[i,"filename"]))next
  print(paste(i,"of",nrow(phenosummary)))
  
  
  filename_original <-paste0(outdir,phenosummary[i,"filename"])
  filename_in <- sub(".gz$","",paste0(outdir,sub("assoc","assoc.filter",phenosummary[i,"filename"])))
  filename_out <- sub(".gz$","",paste0(outdir,sub("assoc","assoc.filter.prune",phenosummary[i,"filename"])))
  
  
  #check existance and read in
  if(!file.exists(filename_in)){
    print(paste("Skipping",i,filename_in,"because it doesn't exists"))
  }
  header <- read.table(filename_original,nrows=1,header=T)
  d<-try(read.table(filename_in,stringsAsFactors = F,header=F),silent=T)
  if(class(d)=="try-error")next
  colnames(d)<-colnames(header)
  
  #apply a formal cutoff 5×10−8 (optional though - seems like all the major papers these day show that more heritability is explained if we add in the almost-significants. So maybe just keep out and use the 1e-7 set earlier?)
  # d<-d[d[,"pval"] < 5e-8,]
  
  
  
  #splitting up the var data
  var_data<-data.frame(do.call(rbind,strsplit(d[,"variant"],":")),stringsAsFactors = F)
  colnames(var_data)<-c("chr","pos","a1","a2")
  var_data[,"pos"] <- as.numeric(var_data[,"pos"])
  
  d<-cbind(var_data,d)
  
  
  #sort by P-value
  d<-d[order(d[,"pval"]),]
  
  
  #iterate down the list and mark stuff within 1 MB for removal
  dist <- 1000000
  at_end_of_list <- FALSE
  j<-1
  while(!at_end_of_list){
    remove <- which(
      d[j,"chr"] %in% d[,"chr"] &
      abs(d[j,"pos"] - d[,"pos"]) <dist
    )
    d<-d[!1:nrow(d)%in%remove[!remove%in%j],]
    j<-j+1
    if(j > nrow(d)) at_end_of_list <- TRUE
  }
  
  
  d[,"code"]<-phenosummary[i,"Field.code"]
  d[,"field"]<-phenosummary[i,"Field"]
  
  d[,"case_count"] <- suppressWarnings(as.numeric(phenosummary[i,"N.cases"]))
  d[,"control_count"] <- suppressWarnings(as.numeric(phenosummary[i,"N.controls"]  ))
  
  results <- rbind(results,d)
  
}


save(results,file="2017-09-27_ukbiobank_snps.rdata")

#only keep codes with 5 or more SNPs in the GRS
# codes_to_keep<-names(table(results[,"code"]))[table(results[,"code"]) >= 5]
# results <- results[results[,"code"]%in%codes_to_keep,]
#actually just keep all - we'll sort them out at later point


dim(results)
# 8568   17

# 

length(unique(results[,"code"]))
# 634

#so 8456 new SNPs giving us 634 new traits


length(names(table(results[,"code"]))[table(results[,"code"]) >= 5])
# 456

#so 456 of these have 5 or more SNPs driving them at p<1e-7 -- sounds good. Design choice will be default to minimum 5 and p<5e-8 (the 'universally agreed'), but then slider based tuning under advanced settings


























#2017-09-27 inserting into the GWAS central pipeline. Should run more smoothly here actually since a lot of the info is more systematic. But we'll see.
# 
# rm(list=ls())
# load("ukbiobank/2017-09-27_ukbiobank_snps.rdata")
# data<-results
# 
# #rename to fit rest of script
# colnames(data)[colnames(data) %in% "rsid"] <- "SNPS"
# colnames(data)[colnames(data) %in% "field"] <- "DISEASE.TRAIT"
# colnames(data)[colnames(data) %in% "chr"] <- "CHR_ID"
# 
# 
# #ignore - already done on ukbiobank download (see above)
# #remove sets that are too small
# # d<-gsub(",","",gsub("[A-Za-z]","",data[,"INITIAL.SAMPLE.SIZE"]))
# # data[,"sampleSize"]<-sapply(strsplit(d," +"),function(x){sum(as.numeric(x),na.rm=T)})
# # data<-data[order(data[,"sampleSize"]),]
# # hist(data[,"sampleSize"],breaks=1000,xlim=c(0,10000),xlab="sample size",main="Sample sizes")
# #decision - remove all studies with sample size < 2000 (because, they may or may not be good)
# # data<-data[data[,"sampleSize"]> 2000,]
# 
# 
# #ignore - already consistently integrated in the Neale Lab work
# #check that the strongest SNP entry is consistent with the SNPs entry (remove otherwise)
# # data[data[,"STRONGEST.SNP.RISK.ALLELE"]%in%"rs12449664A","STRONGEST.SNP.RISK.ALLELE"]<-"rs12449664-A" #clear typo
# # s1<-gsub(" ","",sub("\\?","",sub("NR$","",sub("-.+$","",data[,"STRONGEST.SNP.RISK.ALLELE"]))))
# # sum(s1 != data[,"SNPS"]) #154
# # s2<-data[s1 != data[,"SNPS"],c("STRONGEST.SNP.RISK.ALLELE","SNPS")] 
# # #ok could probably save some of these, but on later testing it is found that all but 6 are removed by other filters anyway... so just get rid of them
# # data<-data[s1 == data[,"SNPS"],]
# 
# 
# #remove SNPs that don't have OR/beta or risk-allele indication
# # data[,"risk_allele"]<-sub("^.+-","",data[,"STRONGEST.SNP.RISK.ALLELE"])
# # sum(data[,"risk_allele"]=="?") #3242 - definetly must remove these
# # data<-data[data[,"risk_allele"]!="?",]
# 
# 
# 
# 
# #investigating number of studies per trait
# traits<-data.frame(row.names=unique(data[,"DISEASE.TRAIT"]))
# for(trait in rownames(traits)){
#   d2<-data[data[,"DISEASE.TRAIT"]%in%trait,]
#   traits[trait,"SNP"]<-nrow(d2)
#   traits[trait,"studies"]<-"ukbioank"
# }
# #conclusion - obviously only one study per trait
# 
# 
# #remove some traits because they are better handled in other modules and/or too weird/difficult to explain easily and/or conflict with module title (or perhaps just ideas for new modules?)
# # omit<-unique(c(
#   # grep("height",rownames(traits),ignore.case=T,value=T),
#   # grep("hair",rownames(traits),ignore.case=T,value=T),
#   # grep("economic",rownames(traits),ignore.case=T,value=T),
#   # grep("political",rownames(traits),ignore.case=T,value=T),
#   # grep("word reading",rownames(traits),ignore.case=T,value=T),
#   # grep("eyes",rownames(traits),ignore.case=T,value=T)
#   # social_communication_problems
#   # wine_liking
# # ))
# # data<-data[!data[,"DISEASE.TRAIT"]%in%omit,]
# 
# 
# #remove some columns that are not needed
# # col_to_remove<-c("MAPPED_GENE","UPSTREAM_GENE_ID","DOWNSTREAM_GENE_ID","SNP_GENE_IDS","UPSTREAM_GENE_DISTANCE","DOWNSTREAM_GENE_DISTANCE","PLATFORM..SNPS.PASSING.QC.","CNV","P.VALUE..TEXT.","PVALUE_MLOG","RISK.ALLELE.FREQUENCY","CONTEXT","INTERGENIC","SNP_ID_CURRENT","MERGED","STUDY","JOURNAL","DATE.ADDED.TO.CATALOG","INITIAL.SAMPLE.SIZE","REPLICATION.SAMPLE.SIZE","CHR_POS")
# # for(col in col_to_remove){data[,col]<-NULL}
# 
# 
# 
# #retrieve chr-ID (for double-check), minor allele frequency and assign effect and non-effect allele
# library(biomaRt)
# snp_mart <- useMart("ENSEMBL_MART_SNP", dataset = "hsapiens_snp",host="www.ensembl.org")
# attributes<-c("refsnp_id","chr_name","chrom_start","allele","minor_allele_freq","minor_allele")
# query<-getBM(attributes, filters = c("snp_filter"), values = unique(data[,"SNPS"]), mart = snp_mart)
# query<-query[nchar(query[,"chr_name"])%in%1:2,]
# rownames(query)<-query[,"refsnp_id"]
# 
# data[,"ensembl_alleles"]<-query[data[,"SNPS"],"allele"]
# data[,"chr_name"]<-query[data[,"SNPS"],"chr_name"]
# data[,"minor_allele_freq"]<-query[data[,"SNPS"],"minor_allele_freq"]
# data[,"minor_allele"]<-query[data[,"SNPS"],"minor_allele"]
# 
# 
# #ignore - this is actually all of them almost 8338 of 8568. But we'll anyway get MAF from 1kgenomes so no worry
# #remove the ones with no known MAF
# # sum(is.na(data[,"minor_allele_freq"]))
# # nrow(data)
# #610
# # data<-data[!is.na(data[,"minor_allele_freq"]),]
# 
# #check the two chr-names are the same
# sum(data[,"CHR_ID" ] != data[,"chr_name"],na.rm=T)
# #0
# sum(data[,"CHR_ID" ] != data[,"chr_name"],na.rm=F)
# NA
# nrow(data[is.na(data[,"chr_name"]),])
# #so 37 are not available through biomart lookup. Let's proceed script a little and see if this becomes a problem. If it does we can remove them,
# 
# 
# #perhaps remove the tri-allelics
# sum(sapply(strsplit(data[,"ensembl_alleles"],"/"),length) != 2)
# #  714 of 8568 -- these were removed in the GWAS central parsing - probably the safer option - but on the other hand this data is more stringent with clear A1 and A2 indications. I'm going to go with 'keep'
# # data<-data[sapply(strsplit(data[,"ensembl_alleles"],"/"),length) == 2,]
# 
# 
# #assign major allele from the ensembl alleles
# a1<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[1]})
# a2<-sapply(strsplit(data[,"ensembl_alleles"],"/"),function(x){x[2]})
# data[,"major_allele"] <- NA
# # data[data[,"minor_allele"]==a1,"major_allele"]<-a2[data[,"minor_allele"]==a1]
# # data[data[,"minor_allele"]==a2,"major_allele"]<-a1[data[,"minor_allele"]==a2]
#this fails - probably due to the 37 biomart missing. Ok - abort that line, and go for 1kgenomes frequency retrieval.















#2017-09-27
#This is a restart where we aim for 1000 genome frequency insertation right from the beginning
rm(list=ls())
load("ukbiobank/2017-09-27_ukbiobank_snps.rdata")
path_1kg<-"/home/people/lasfol/downloadBulk/annotation/1000_genomes_20130502/"

#rename to fit rest of script
data<-results
colnames(data)[colnames(data) %in% "rsid"] <- "SNP"
# colnames(data)[colnames(data) %in% "field"] <- "DISEASE.TRAIT"
colnames(data)[colnames(data) %in% "chr"] <- "chr_name"


output<-data.frame("CHROM"=vector(), "POS"=vector(),   "REF"=vector(),   "ALT"=vector(),   "AF"=vector(),    "EAS_AF"=vector(),"AMR_AF"=vector(),"AFR_AF"=vector(),"EUR_AF"=vector(),"SAS_AF"=vector(),stringsAsFactors = F)

for(chr in sort(unique(data[,"chr_name"]))){
  print(chr)
  g1<-data[data[,"chr_name"]%in%chr,]
  snps<-unique(g1[,"SNP"])
  
  write.table(snps,file="temp_list_of_snps.txt",sep="\t",col.names=F,row.names=F,quote=F)
  
  if(chr =="X"){
    vcf_path<-paste0(path_1kg,"ALL.chrX.phase3_shapeit2_mvncall_integrated_v1b.20130502.genotypes.vcf.gz")
  }else{
    vcf_path<-paste0(path_1kg,"ALL.chr",chr,".phase3_shapeit2_mvncall_integrated_v5a.20130502.genotypes.vcf.gz")
  }
  cmd1<-paste0("vcftools --indv HG00096 --snps temp_list_of_snps.txt --gzvcf ",vcf_path," --out chr",chr," --recode --recode-INFO-all")
  system(cmd1)
  
  cmd2 <- paste0("cut -f 3 chr",chr,".recode.vcf")
  snps<-grep("^[#I]",system(cmd2,inter=T),value=T,invert=T)
  
  cmd3 <- paste0("vcftools --vcf chr",chr,".recode.vcf --get-INFO AF --get-INFO EAS_AF --get-INFO AMR_AF --get-INFO AFR_AF --get-INFO EUR_AF --get-INFO SAS_AF --out chr",chr)
  system(cmd3)
  
  result<-read.table(paste0("chr",chr,".INFO"),sep="\t",header=T,stringsAsFactors = F)
  rownames(result)<-snps
  output <- rbind(output, result)
  
}
write.table(output,file="2017-09-27_all_frequencies.txt",sep="\t",col.names=NA)



#merge in
rm(list=ls())
all_freq<-read.table("ukbiobank/2017-09-27_all_frequencies.txt.gz",stringsAsFactors = F,header=T,row.names=1)
load("ukbiobank/2017-09-27_ukbiobank_snps.rdata")
data<-results

#set 0 to minimum (e.g. 0.001 which is otherwise the lowest value)
af_cols<-grep("AF$",colnames(all_freq),value=T)
for(col in af_cols){
  all_freq[,col]<-suppressWarnings(as.numeric(all_freq[,col]))
  all_freq[all_freq[,col]%in%0,col]<-0.001
}
data<-cbind(data,all_freq[data[,"rsid"],])


sum(is.na(data[,"AF"]))
1013
nrow(data)
8568
#ok - pity - but we need the frequency so we have to remove those 968 SNPs


#The objective now is to 
#1) make the file as similar as ('2017-02-21_semi_curated_version_gwas_central.rdata')
#2) do checkups of allele similarity etc

# #rename to fit rest of script
colnames(data)[colnames(data) %in% "rsid"] <- "SNP"
colnames(data)[colnames(data) %in% "field"] <- "DISEASE.TRAIT"
colnames(data)[colnames(data) %in% "chr"] <- "chr_name"
colnames(data)[colnames(data) %in% "a1"] <- "effect_allele"
colnames(data)[colnames(data) %in% "a2"] <- "non_effect_allele"
colnames(data)[colnames(data) %in% "beta"] <- "effect_size"
colnames(data)[colnames(data) %in% "pval"] <- "P.VALUE"
colnames(data)[colnames(data) %in% "code"] <- "study_id"
colnames(data)[colnames(data) %in% "AF"] <- "minor_allele_freq"



hist(data[,"minor_allele_freq"])
#so most are indicated as minor - I guess for consistency we should flip the ones >0.5
af_cols<-c("minor_allele_freq",grep("AF$",colnames(data),value=T))
for(i in 1:nrow(data)){
  if(is.na(data[i,"minor_allele_freq"])){
    data[i,"major_allele"]<-"?"
    data[i,"minor_allele"]<-"?"
  }else{
    if(data[i,"minor_allele_freq"]>0.5){ #then flip all
      data[i,"major_allele"]<-data[i,"ALT"]
      data[i,"minor_allele"]<-data[i,"REF"]
      for(af_col in af_cols){
        data[i,af_col]<-1-data[i,af_col]
      }
      print("flip")
    }else{
      data[i,"minor_allele"]<-data[i,"ALT"]
      data[i,"major_allele"]<-data[i,"REF"]
      
    }
  }
}






#check cases were risk allele is not found in minor or major allele
sum(!(data[,"effect_allele"] %in% data[,"minor_allele"] | data[,"effect_allele"] %in% data[,"major_allele"] ))
#0
#Nice! Much cleaner data than gwas central




#check how often minor allele is risk
sum(data[,"minor_allele"]==data[,"effect_allele"]) / nrow(data)
#0.10 - but this should be expcted, because:
sum(data[,"effect_size"]>0) / nrow(data)
#0.79



#add a 'safe_name' trait id or such - no-special characters identifier to each
data[,"study_id"] <- paste(data[,"study_id"],"ukbiobank",sep="_")
  


#ensure only standard values A G C T ? are present
table(data[,"major_allele"])
table(data[,"minor_allele"])
table(data[,"effect_allele"])
table(data[,"non_effect_allele"])
ok_values <- c("A","C","T","G","?")
for(col in c("major_allele","minor_allele","effect_allele","non_effect_allele")){
  data[!data[,col]%in%ok_values,col]<-"?"
}

#ensure match between risk/non-risk and major/minor
g1<-apply(t(apply(data[,c("major_allele","minor_allele")],1,sort,decreasing=F)),1,paste,collapse="")
g2<-apply(t(apply(data[,c("effect_allele","non_effect_allele")],1,sort,decreasing=F)),1,paste,collapse="")
have_unknown <- apply(data[,c("major_allele","minor_allele","effect_allele","non_effect_allele")]=="?",1,sum)>0
# have_unknown
sum(g1!=g2 & !have_unknown)
# 29!? - this is a quite serious error. It means that the input data had non-existing alleles, e.g. rs36143444 indicated as C as effect and A as non-effect, but 1kgenomes gives that it has C and T alleles
# These must be omitted
data<-data[!(g1!=g2 & !have_unknown),]



#re-order colnames so that the essential are first
putFirst<-c("SNP", "chr_name","effect_allele","non_effect_allele","effect_size",  "minor_allele_freq","minor_allele","major_allele")
putFirst%in%colnames(data)
data<-data[,c(putFirst,colnames(data)[!colnames(data)%in%putFirst])]
save(data, file="ukbiobank/2017-09-28_semi_curated_version_ukbiobank.rdata")




#then save a SNPs_to_analyze.txt (which just contains snp, chr, effect_allele and non_effect_allele and NO duplicate SNPs)
gwas_snps <- data[,c("SNP","chr_name","effect_allele","non_effect_allele")]
gwas_snps <- gwas_snps[!duplicated(gwas_snps[,"SNP"]),]
rownames(gwas_snps) <- gwas_snps[,"SNP"]
save(gwas_snps,file="ukbiobank/2017-09-28_all_ukbiobank_snps.rdata")






#then create an overview trait list
library(openxlsx)
phenosummary_path<-"ukbiobank/phenosummary_final_11898_18597.xlsx"
phenosummary<-read.xlsx(phenosummary_path)
rownames(phenosummary) <- phenosummary[,"Field.code"]



traits<-data.frame(row.names=unique(data[,"study_id"]), study_id=unique(data[,"study_id"]), trait=unique(data[,"DISEASE.TRAIT"]),niceName=unique(data[,"DISEASE.TRAIT"]),PMID="25826379",Author="UK biobank",stringsAsFactors=F)

all(sub("_ukbiobank","",rownames(traits))%in%rownames(phenosummary))

traits<-cbind(traits,phenosummary[sub("_ukbiobank","",rownames(traits)),c(1,3:11)])


traits<-traits[order(factor(traits[,"Field.code"],levels=phenosummary[,"Field.code"])),]


#Ad-hoc define them for now
traits[,"diagnosis"] <- traits[,"treatment"] <- traits[,"self_rep"] <- FALSE
traits[grep("Diagnoses",traits[,"trait"]),"diagnosis"]<-TRUE
traits[grep("Treatment/medication",traits[,"trait"]),"treatment"] <- TRUE
traits[grep("Non-cancer illness code, self-reported",traits[,"trait"]),"self_rep"] <- TRUE
traits[,"other"] <- apply(traits[,c("diagnosis","treatment","self_rep")],1,sum)==0
# traits[,c("diagnosis","treatment","self_rep","other")]
save(traits, file="ukbiobank/2017-09-28_trait_overoverview.rdata")


#For category sorting (later)
# write.xlsx(traits,file="ukbiobank/temp.xlsx")










