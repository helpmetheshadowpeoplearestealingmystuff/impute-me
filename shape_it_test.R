
#More general setup
#Define places
runDir="/home/ubuntu/test_run"
rawdata="/home/ubuntu/example_output/imputation_folder_id_562283631/id_562283631_raw_data.txt"
shapeit="/home/ubuntu/impute_dir/bin/shapeit"
plink="/home/ubuntu/impute_dir/plink"
sample_ref="/home/ubuntu/misc_files/sample.reference.txt"


setwd(runDir)

#Load data using plink 1.9
cmd1<-paste(plink,"--noweb --23file",rawdata,"John Doe --recode --out step_1")
system(cmd1)



#Rscript to omit duplicates
map<-read.table('step_1.map',sep='\t',stringsAsFactors=F)
exclude<-map[duplicated(map[,4]),2]
print(paste('Removed',length(exclude),'SNPs that were duplicated'))
write.table(exclude,file='step_2_exclusions',sep='\t',row.names=FALSE,col.names=F,quote=F)



#loop over chromosomes
for(chr in 1:22){
	
	#First in loop - extract only one specific chromosome
	cmd2<-paste(plink," --file step_1 --chr ",chr," --recode --out step_2_chr",chr," --exclude step_2_exclusions",sep="")
	system(cmd2)
	
	#Then check for strand flips etc. 
	cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log shapeit_check_chr",chr,"_log",sep="")
	system(cmd3)
	
	
	#Many homozygote SNPs will fail the check, because, well - of course, they don't have the ref-allele. So we make more detailed R script for sorting them
	logFile<-read.table(paste("shapeit_check_chr",chr,"_log.snp.strand",sep=""),sep='\t',stringsAsFactors=FALSE,header=F,skip=1)
	omitMissing<-logFile[logFile[,1] %in% 'Missing',3]
	logStrand<-logFile[logFile[,1] %in% 'Strand',]
	omitNonIdentical<-logStrand[logStrand[,5] != logStrand[,6],3]
	omitBlank<-logStrand[logStrand[,5]%in%'',3]
	
	#These are super-annoying. We have to create another (fake) person with the alternative allele just for their sake. This next command takes all the homozygotes, minus the indels (which are too complicated to lift out from 23andme)
	forceHomozygoteTable<-logStrand[
		logStrand[,5] == logStrand[,6] & 
			nchar(logStrand[,9])==1 & 
			nchar(logStrand[,10])==1
		,]
	
	#This removes any duplicates there might be
	forceHomozygoteTable<-forceHomozygoteTable[!duplicated(forceHomozygoteTable[,4]),]
	map<-read.table(paste("step_2_chr",chr,".map",sep=""),sep="\t",stringsAsFactors=F)
	#This loads the ped file, and doubles it
	ped2<-ped1<-strsplit(readLines(paste("step_2_chr",chr,".ped",sep=""))," ")[[1]]
	ped2[1]<-"Temporary"
	ped2[2]<-"Non_person"
	if((length(ped1)-6) / 2 !=nrow(map))stop("mismatch between map and ped")
	replacementPos<-which(map[,2]%in%forceHomozygoteTable[,4])
	A1_pos<-7+2*(replacementPos-1)
	A2_pos<-8+2*(replacementPos-1)
	ped2[A1_pos]<-forceHomozygoteTable[,9]
	ped2[A2_pos]<-forceHomozygoteTable[,10]
	ped<-rbind(ped1,ped2)
	write.table(ped,paste("step_3_chr",chr,".ped",sep=""),sep=" ",col.names=F,row.names=F,quote=F)
	omitRemaining<-logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
	print(paste('Omitting',length(omitMissing),'because of missing',length(omitBlank),'because they are blank, and',length(omitNonIdentical),'true strand flips'))
	write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file=paste("step_3_chr",chr,"_exclusions",sep=""),sep='\t',row.names=F,col.names=F,quote=F)
	
	
	
	
	#running the shapeit command (with two people, the right one and a placeholder heterozygote)
	cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
	system(cmd4)

	
	#removing the placeholder person again
	cmd5<-paste("cut --delimiter=\  -f 1-8 step_4_chr",chr,".haps > step_5_chr",chr,".haps",sep="")
	system(cmd5)
	
	#detect max length of each chromosome
	cmd6<-paste("zcat /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2",sep="")
	maxPos<-as.numeric(system(cmd6,intern=T))
	
	
	#iterate over 5e6 chunks
	starts<-seq(0,maxPos,5e6)
	for(i in 1:length(starts)){
		start <- starts[i]
		end <- start+5e6
		
		
		cmd7<-paste("impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")
		system(cmd7)
	}
}


	
	
# 
# 
# 
# 
# # url<- "ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20110521/supporting/phase1_samples_integrated_20101123.ped"
# # download.file(url,basename(url))
# # d<-read.table(basename(url),stringsAsFactors=F,sep="\t",comment.char="", quote="",fill=T,header=T)
# # head(d)
# # d[,"group"]<-d[,"Population"]
# # d<-d[,c("Individual.ID","Population","group","Gender")]
# # colnames(d)<-c("sample","population","group","sex")
# # set.seed(42)
# # d<-d[sample(rownames(d),2184/2),]
# # write.table(d,file="sample.reference.txt",sep="\t",row.names=F,col.names=T,quote=F)
# #make a sample file
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutx2_22.ped plinkoutx2_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/test_run/sample.reference.txt \
# --output-log W_test
# 
# cut -f 4 W_test.snp.strand > exclude_snps_here
# 
# ~/impute_dir/plink --file plinkoutx2_22 \
# --recode \
# --out plinkoutxx_22 \
# --exclude exclude_snps_here
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/test_run/sample.reference.txt \
# --output-log R_test
# 
# 
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/test_run/sample.reference.txt \
# -O T_test
# 
# 
# 
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -known_haps_g T_test.haps -int 20098520 20344770 -Ne 20000 -o test_qt
# # THis worked! Wauw
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #More general setup
# #Define places
# cd ~/test_run
# rawdata=/home/ubuntu/example_output/imputation_folder_id_562283631/id_562283631_raw_data.txt
# shapeit=/home/ubuntu/impute_dir/bin/shapeit
# plink=/home/ubuntu/impute_dir/plink
# sample_ref=/home/ubuntu/misc_files/sample.reference.txt
# 
# 
# #Load data using plink 1.9
# $plink --noweb --23file $rawdata John Doe --recode --out step_1
# 
# 
# #Rscript to omit duplicates
# echo "map<-read.table('step_1.map',sep='\t',stringsAsFactors=F)" > script1
# echo "exclude<-map[duplicated(map[,4]),2]" >> script1
# echo "print(paste('Removed',length(exclude),'SNPs that were duplicated'))" >> script1
# echo "write.table(exclude,file='step_2_exclusions',sep='\t',row.names=FALSE,col.names=F,quote=F)" >> script1
# Rscript script1
# 
# 
# #loop over chromosomes
# for chr in $(seq 1 22); do
# 
# #First in loop - extract only one specific chromosome
# $plink --file step_1 \
# --chr $chr \
# --recode \
# --out step_2 \
# --exclude step_2_exclusions;
# 
# 
# #Then check for strand flips etc. 
# $shapeit -check \
# --input-ped step_2.ped step_2.map \
# -M "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr"$chr"_combined_b37.txt" \
# --input-ref "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr"$chr"_impute.hap.gz" "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr"$chr"_impute.legend.gz" $sample_ref \
# --output-log shapeit_check_log
# 
# 
# 
# #Many homozygote SNPs will fail the check, because, well - of course, they don't have the ref-allele. So we make more detailed R script for sorting them
# logFile<-read.table('shapeit_check_log.snp.strand',sep='\t',stringsAsFactors=FALSE,header=F,skip=1)
# omitMissing<-logFile[logFile[,1] %in% 'Missing',3]
# logStrand<-logFile[logFile[,1] %in% 'Strand',]
# omitNonIdentical<-logStrand[logStrand[,5] != logStrand[,6],3]
# omitBlank<-logStrand[logStrand[,5]%in%'',3]
# 
# #These are super-annoying. We have to create another (fake) person with the alternative allele just for their sake
# forceHomozygoteTable<-logStrand[
# 	logStrand[,5] == logStrand[,6] & 
# 		nchar(logStrand[,9])==1 & 
# 		nchar(logStrand[,10])==1
# 	,]
# 
# 
# forceHomozygoteTable<-forceHomozygoteTable[!duplicated(forceHomozygoteTable[,4]),]
# map<-read.table("step_2.map",sep="\t",stringsAsFactors=F)
# ped2<-ped1<-strsplit(readLines("step_2.ped")," ")[[1]]
# ped2[1]<-"Temporary"
# ped2[2]<-"Non_person"
# if((length(ped1)-6) / 2 !=nrow(map))stop("mismatch between map and ped")
# replacementPos<-which(map[,2]%in%forceHomozygoteTable[,4])
# # replacementPos<-c(1,replacementPos)
# A1_pos<-7+2*(replacementPos-1)
# A2_pos<-8+2*(replacementPos-1)
# ped2[A1_pos]<-forceHomozygoteTable[,9]
# ped2[A2_pos]<-forceHomozygoteTable[,10]
# ped<-rbind(ped1,ped2)
# write.table(ped,"step_3.ped",sep=" ",col.names=F,row.names=F,quote=F)
# omitRemaining<-logStrand[!logStrand[,4]%in%forceHomozygoteTable[,4],3]
# print(paste('Omitting',length(omitMissing),'because of missing',length(omitBlank),'because they are blank, and',length(omitNonIdentical),'true strand flips'))
# write.table(c(omitNonIdentical,omitBlank,omitMissing,omitRemaining),file='step_3_exclusions',sep='\t',row.names=F,col.names=F,quote=F)
# 
# 
# 
# 
# 
# $shapeit \
# --input-ped step_3.ped step_2.map \
# -M "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr"$chr"_combined_b37.txt" \
# --input-ref "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr"$chr"_impute.hap.gz" "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr"$chr"_impute.legend.gz" $sample_ref \
# --exclude-snp step_3_exclusions \
# -O "step_3"
# 
# 
# 
# 
# #Still need to figure out the chunk setting! Otherwise done
# 
# cut --delimiter=\  -f 1-8 step_4 > step_5
# 
# 
# 
# maxPos=`zcat "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr"$chr"_impute.legend.gz" | tail -n 1 | cut --delimiter=\  -f 2`
# 
# 
# 
# for start in $(seq 0 5000000 $maxPos); do
# end=$(($start + 5000000))
# echo $end
# start_kb=$(($start / 1000))
# impute2 \
# -m "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr"$chr"_combined_b37.txt" \
# -h "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr"$chr"_impute.hap.gz" \
# -l "/home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr"$chr"_impute.legend.gz" \
# -known_haps_g step_3.haps -int $start $end -Ne 20000 -o "step_4_chr"$chr"_"$start_kb
# 
# done
# 
# 
# 
# 
# 
# 
# 
# # url<- "ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20110521/supporting/phase1_samples_integrated_20101123.ped"
# # download.file(url,basename(url))
# # d<-read.table(basename(url),stringsAsFactors=F,sep="\t",comment.char="", quote="",fill=T,header=T)
# # head(d)
# # d[,"group"]<-d[,"Population"]
# # d<-d[,c("Individual.ID","Population","group","Gender")]
# # colnames(d)<-c("sample","population","group","sex")
# # set.seed(42)
# # d<-d[sample(rownames(d),2184/2),]
# # write.table(d,file="sample.reference.txt",sep="\t",row.names=F,col.names=T,quote=F)
# #make a sample file
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutx2_22.ped plinkoutx2_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/test_run/sample.reference.txt \
# --output-log W_test
# 
# cut -f 4 W_test.snp.strand > exclude_snps_here
# 
# ~/impute_dir/plink --file plinkoutx2_22 \
# --recode \
# --out plinkoutxx_22 \
# --exclude exclude_snps_here
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit -check \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/test_run/sample.reference.txt \
# --output-log R_test
# 
# 
# 
# 
# /home/ubuntu/impute_dir/bin/shapeit \
# --input-ped plinkoutxx_22.ped plinkoutxx_22.map \
# -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt \
# --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz /home/ubuntu/test_run/sample.reference.txt \
# -O T_test
# 
# 
# 
# 
# 
# impute2 -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr22_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr22_impute.legend.gz -known_haps_g T_test.haps -int 20098520 20344770 -Ne 20000 -o test_qt
# # THis worked! Wauw
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
