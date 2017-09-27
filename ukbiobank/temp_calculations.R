


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