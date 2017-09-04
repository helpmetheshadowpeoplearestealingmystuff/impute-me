

basedir<-"/home/ubuntu/to_fix_later/"

source("/home/ubuntu/srv/impute-me/functions.R")


d<-data.frame(row.names=sub("^.+folder_","",list.files(basedir,full.names=F)),path=list.files(basedir,full.names=T),stringsAsFactors=F)

nrow(d)
10 #10 sorted out genomes since last...


errors<-list()
for(uniqueID in rownames(d)){
  error_path<-paste0(d[uniqueID,"path"],"/error")
  if(!file.exists(error_path)){
    print(paste(uniqueID,"no error file")) 
    next
  }
  errors[[uniqueID]]<-readLines(error_path)
}


MT_any_errors <- sapply(errors,function(x){length(grep("Something odd with the MT presence reverter",x))})
MT_I_errors <- sapply(errors,function(x){length(grep("Something odd with the MT presence reverter I$",x))})
MT_II_errors <- sapply(errors,function(x){length(grep("Something odd with the MT presence reverter II",x))})

MT_II_error_ids<-names(MT_II_errors)[MT_II_errors==1 ]
MT_I_error_ids<-names(MT_I_errors)[MT_I_errors==1]
not_MT_error_ids<-names(MT_any_errors)[MT_any_errors==0]



############
#not MT error evaluation
errors[not_MT_error_ids]

#cannot open file 'step_2_chr3_shapeit_log.snp.strand': No such file or directory"    
# cannot open file 'step_2_chr10_shapeit_log.snp.strand': No such file or directory"
#
#These two look rather odd... not sure what to do going forward other than just re-run or flush-out




############
#MT I error evaluation
errors[MT_I_error_ids]

example_unique_id<-MT_I_error_ids[1]



#run the run_imputation function for this one
rawdata<-grep("raw_data.txt$",list.files(d[example_unique_id,"path"],full.names=T),value=T)
runDir=d[example_unique_id,"path"]
shapeit="/home/ubuntu/impute_dir/bin/shapeit"
plink="/home/ubuntu/impute_dir/plink"
impute2="/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2"
sample_ref="/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"



#Ok for 1 it's a matter of the X chromosome being surrounded by quotes, like this:
# "rs1454268","X","154758477","TT"
# "rs6642287","X","154780283","--"
# "rs1084451","X","154816439","AA"
# "rs5940536","X","154821956","GG"
# "rs5983743","X","154823227","CC"


# Whereas the rest is tab-sep like this. Odd
# rs5771716       22      49080775        AG
# rs5771717       22      49081164        GG
# rs6010588       22      49081194        GG
# rs130110        22      49083887        CC
#unless this is consistent, probably no need to fix going forward
#Fixed with quote remover

#For 2 it was a matter of the missing data having three --- instead of 2 --. Odd
#PROBABLY FIXED WITH --- -> -- (but still may fail multi-allelic)

#For 3 it was a matter of having XY chr's intermingled with the data. Odd -again
#FIXED WITH XY REMOVER

#For 4 it was a matter of the missing data having three --- instead of 2 --. Odd
#PROBABLY FIXED WITH --- -> -- (but still may fail multi-allelic)

#for 5 it's the same problem as 1
#Fixed with quote remover

#All in all I'm not sure it makes sense to put in catches for these exception cases. It's very unlikely they will pop up again


############
#MT II error evaluation


errors[MT_II_error_ids]



example_unique_id<-MT_II_error_ids[3]




#run the run_imputation function for this one
rawdata<-grep("raw_data.txt$",list.files(d[example_unique_id,"path"],full.names=T),value=T)
runDir=d[example_unique_id,"path"]
shapeit="/home/ubuntu/impute_dir/bin/shapeit"
plink="/home/ubuntu/impute_dir/plink"
impute2="/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2"
sample_ref="/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3.sample"



#For one, at line +702443 there was this odd entry:
# rs3888396       22      51211392        TT
# RSID    CHROMOSOME      POSITION        RESULT
# rs5939319       X       2700157 GG


#For 2, this was found a tline +341
# rs398122914     1       2160485 TT
# rs398122889     1       2160488 #N/A
# rs387907303     1       2160552 GG
#so failing because, should be --
#PROBABLY FIXED WITH --- -> -- (but still may fail multi-allelic)


#For 3, it looked like this
# "rs3131972"     "1"     "742584"        "GG"
# "rs12562034"    "1"     "758311"        "--"
# "rs12124819"    "1"     "766409"        "AA"
# "rs11240777"    "1"     "788822"        "AG"
# "rs6681049"     "1"     "789870"        "CC"

#So because of the quotes.
#Fixed with quote remover








#2017-09-04 checking broken runs 
# Living DNA customer genotype data download file version: 1.0.1
"/home/ubuntu/to_fix_later/imputation_folder_id_24W9524h9/id_24W9524h9_raw_data.txt"

#so maybe that subsetter algorithm should be up-front?
# rs61752992      X       153296077       CTGGTGGGGTCCTCGGAGCTCTCGGGCTCAGGTGGAGGTGGGGGC


#ok - put in new checks