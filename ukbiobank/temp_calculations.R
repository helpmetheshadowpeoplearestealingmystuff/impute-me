


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
    # print(paste(code,"was skipped because it only had",cases,"cases and",controls,"controls"))
    next
  }else{
    # print(field)
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
  if(file.exists(filename))next
  
  #executing
  system(cmd1)
  file.rename(sub("^.+-O ","",cmd1),filename)
  # Sys.sleep(10)
}



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

rm(list=ls())
library(openxlsx)
phenosummary<-read.xlsx("/home/people/lasfol/gitStuff/2015-08-17_imputeme/ukbiobank/phenosummary_final_11898_18597.xlsx" )
manifest<-read.xlsx("/home/people/lasfol/gitStuff/2015-08-17_imputeme/ukbiobank/UKBB GWAS Manifest 20170915.xlsx" )
outdir <- "/home/people/lasfol/temp_ukbiobank/"


for(i in 1:nrow(phenosummary)){
  
  code<-phenosummary[i,"Field.code"]
  field<-phenosummary[i,"Field"]
  
  cases<- suppressWarnings(as.numeric(phenosummary[i,"N.cases"]))
  controls<- suppressWarnings(as.numeric(phenosummary[i,"N.controls"]  ))
 
  #get the code match (some weird field-mismatch - but this should fix it)
  w1<-which(manifest[,"Phenotype.code"]%in%code)
  if(length(w1)>1){stop("!")}
  if(length(w1)==0){
    w2<-which(manifest[,"Phenotype.code"]%in%sub("^.+_","",code))
    if(length(w2)>1){stop("!!")
      if(length(w2)==0){
        stop("!!!") 
      }else{
        w<-w2
      }
    }
  }else{
    w<-w1
  }
  
  
  
  phenosummary
  
  
}