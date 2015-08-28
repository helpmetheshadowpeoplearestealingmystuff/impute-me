
run_phasing_and_imputation<-function(
	rawdata, 
	runDir, 
	shapeit="/home/ubuntu/impute_dir/bin/shapeit",
	plink="/home/ubuntu/impute_dir/plink",
	sample_ref="/home/ubuntu/misc_files/sample.reference.txt"
){
	
	
	
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
}

