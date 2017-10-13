

new<-read.table("C:/Users/FOLK/Documents/Work/Bioinformatics/new/id_823J76y50.cached.all_gwas.gz",stringsAsFactors = F, header=T,row.names=1)
old<-read.table("C:/Users/FOLK/Documents/Work/Bioinformatics/old/id_823J76y50.cached.all_gwas.gz",stringsAsFactors = F, header=T,row.names=1)


r<-intersect(rownames(new),rownames(old))


d<-data.frame(row.names=r,new=new[r,],old=old[r,])


#NA-mismatch
nrow(d)
12571

sum(is.na(d[,"new"]))
248

sum(is.na(d[,"old"]))
148

sum(apply(is.na(d),1,sum)==2)
75

d[apply(is.na(d),1,sum)==1,]
#seems fairly random when it is NA in one place and when it is in another





#outright-mismatch
d1<-d[!is.na(d[,"new"]) & !is.na(d[,"old"]),]


d2<-d1[d1[,"new"] != d1[,"old"],]
nrow(d2)
455

#that's too much


# we investigate one (rs5760748)
#this is how it looks in new in gen
# 22 rs5760747 25334869 G A 1 0 0
# --- rs144293611 25334885 G A 1 0 0
# --- rs5760748 25334977 C T 1 0 0
# --- rs11913792 25335033 C T 1 0 0
# --- rs193227671 25335058 G C 1 0 0
# --- rs184935645 25335089 G A 1 0 0
# 22 rs5996794 25335093 C T 1 0 0

#this is how it looks in old in gen
# --- rs5760747 25334869 G A 0.003 0.996 0
# --- rs144293611 25334885 G A 1 0 0
# --- rs5760748 25334977 C T 0.003 0.996 0
# --- rs11913792 25335033 C T 1 0 0
# --- rs193227671 25335058 G C 1 0 0
# --- rs184935645 25335089 G A 1 0 0
# --- rs5996794 25335093 C T 0.003 0.996 0



#so it seems like two more are measured in the new?! Check rs5760747 and rs5996794

grep rs5760748 id_823J76y50_raw_data.txt 
grep 25334869 id_823J76y50_raw_data.txt 
grep 25335093 id_823J76y50_raw_data.txt 


#OK - so the verdict here is that they are NOT measured in the new data set. Maybe they have been force inserted somehow in the merge step. In other words - we need to do more checkups for that :-(






#2017-10-09 continuing evaluation after re-run with deletion filters off

# We look at the rs5760747 rs5996794 and rs5760748 in chr22 (the last SNP is just a grep-working-check)
# 

grep 'rs5760747\|rs5996794\|rs5760748\|rs13328684' step_1_id_823J76y50.map
#none found in input

grep 'rs5760747\|rs5996794\|rs5760748\|rs147574439' step_2_id_823J76y50_chr22.bim 
#none found in step2

#check this once it is ready - presumably it will give that at least 2 or 3 are found - given that we check the fam
grep 'rs5760747\|rs5996794\|rs5760748' step_2m_chr22.bim
grep 'rs5760747\|rs5996794\|rs5760748' step_2m_chr22.map

# 22      rs5760747       0       25334869        G       A
# 22      rs5996794       0       25335093        C       T
 #so the two 'found' ones are there, as expected, but not the rs5760748 imputed one - also as expected
#also bim and map seem to be the same contents: same line counts, just have the A1 and A2 also in bim. Plink probably writes both

R
map<-read.table("step_2m_chr22.bim",sep="\t",stringsAsFactors = F, header=F)
ped_raw<-read.table("step_2m_chr22.ped",sep=" ",stringsAsFactors = F, header=F)
ped<-data.frame(t(ped_raw[,7:ncol(ped_raw)]),stringsAsFactors = F)
colnames(ped) <- ped_raw[,1]
ped[,"snp"]<-rep(map[,2],each=2)


ped[ped[,"snp"]%in%"rs5760747",]
# 
# id_40SGL0789 id_499p53203 id_823J76y50 id_4125H8236 id_4527s4Kn5
# V9979            0            G            0            G            A
# V9980            0            A            0            A            A
# id_4866X9259 id_44144pUg2 id_49915K261 id_471017c50 id_6622289U4
# V9979            G            G            0            0            0
# V9980            A            A            0            0            0
# snp
# V9979 rs5760747
# V9980 rs5760747


ped[ped[,"snp"]%in%"rs5996794",]
# id_40SGL0789 id_499p53203 id_823J76y50 id_4125H8236 id_4527s4Kn5
# V9981            0            C            0            C            T
# V9982            0            T            0            T            T
# id_4866X9259 id_44144pUg2 id_49915K261 id_471017c50 id_6622289U4
# V9981            0            C            0            0            0
# V9982            0            T            0            0            0
# snp
# V9981 rs5996794
# V9982 rs5996794

#these are both correctly indicated as 0/0. WTF

#after step_4 - for real shapeit call:
grep rs5996794 step_4_chr22.haps
22 rs5996794 25335093 C T 1 1 1 0 0 0 0 1 1 1 0 1 1 0 1 1 1 0 0 1 1 1

grep rs5760747 step_4_chr22.haps
22 rs5760747 25334869 G A 1 1 1 0 0 0 0 1 1 1 0 1 1 0 1 1 1 0 0 1 1 1

#there's 22, recall that the insert-person is the first. So id_823J76y50 should be 4th, i.e. hap col 7 and 8. Which is set to 1 1 for both SNPs. Ultimately this must be the explanation of the imputation results, that there is no missing hap ability in shape it, and so the id_823J76y50 is forced into something it is not, a 1 1 hap.

#the 1M$ question is then if this is wrong or not. Perhaps 1 1 is the most likely hap, even when considering the missing data. It seems shapeit is cool with 0-coded data as input. I just don't get why it is _different_ than when we sent in the single coded person. 


#what to do

#what to do


#OK - we take three samples and rename them and also duplicate them. Then we can get a sense of how reproducible it is
uniqueIDs<-c("id_4I5w1F047","id_525Q456B2","id_55c026704")
uniqueIDs<-c("id_525Q456B2","id_55c026704")
duplicates <- 2


for(uniqueID_here in uniqueIDs){
  source <- paste0("/home/ubuntu/imputations/imputation_folder_",uniqueID_here)
  if(!file.exists(source))stop("!")
  for(i in 6:(duplicates+6)){
    new_name<-paste0(substr(uniqueID_here,1,9),"00",i)
    destination <- paste0("/home/ubuntu/imputations/imputation_folder_",new_name)
    dir.create(destination)
    files_to_copy<-list.files(source,full.names=T)
    for(f in files_to_copy){
      file.copy(f, gsub(uniqueID_here,new_name,f))
    }
    var_path<-paste0(destination,"/variables.rdata")
    o<-load(var_path)
    uniqueID <- new_name
    save( "uniqueID","email","filename", "protect_from_deletion",file=var_path)
    job_status_file<-paste(destination,"/job_status.txt",sep="")
    unlink(job_status_file)
    write.table("Job is ready",file=job_status_file,col.names=F,row.names=F,quote=F)
    
    cat(paste0("'",new_name,"',"))
  }
}


#put these in priority queue and do bulk run
# id_4I5w1F006, id_4I5w1F007, id_4I5w1F008, id_525Q45006, id_525Q45007, id_525Q45008, id_55c026006, id_55c026007, id_55c026008

#put these in priority queue and do single run
# id_4I5w1F001, id_4I5w1F002, id_525Q45001, id_525Q45002, id_55c026001, id_55c026002


#These were already run with a bulk imputaiton run:
# id_4I5w1F047, id_525Q456B2, id_55c026704

#these can be used for later priority queue runs
id_4I5w1F003, id_4I5w1F004, id_4I5w1F005, id_525Q45003, id_525Q45004, id_525Q45005, id_55c026003, id_55c026004, id_55c026005

#delete if necessary folders
rm -r imputation_folder_id_4I5w1F001 imputation_folder_id_4I5w1F002 imputation_folder_id_525Q45001 imputation_folder_id_525Q45002 imputation_folder_id_55c026001 imputation_folder_id_55c026002






#comparison
rm(list=ls())
bulk_imputation_runs<-c("id_4I5w1F006","id_4I5w1F007","id_4I5w1F008","id_525Q45006","id_525Q45007","id_525Q45008","id_55c026006","id_55c026007","id_55c026008","id_4I5w1F047","id_525Q456B2","id_55c026704")
single_imputation_runs<-c("id_4I5w1F001","id_4I5w1F002","id_525Q45001","id_525Q45002","id_55c026001","id_55c026002")

runs<-data.frame(uniqueIDs=c(bulk_imputation_runs,single_imputation_runs),type=c(rep("bulk",length(bulk_imputation_runs)),rep("single",length(single_imputation_runs))))
runs[,"source"]<-substr(runs[,"uniqueIDs"],1,9)
