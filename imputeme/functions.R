
prepare_23andme_genome<-function(path, email){
	if(class(path)!="character")stop(paste("path must be character, not",class(path)))
	if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
	if(!file.exists(path))stop(paste("Did not find file at path:",path))
	
	if(class(email)!="character")stop(paste("email must be character, not",class(email)))
	if(length(email)!=1)stop(paste("email must be lengh 1, not",length(email)))
	if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
		stop(paste("a real email adress is needed:",email))
	}
	
	
	#check for too many ongoing imputations
	s<-list.files("/home/ubuntu/imputations/")
	if(length(grep("^imputation_folder",s)) > 4)stop("More than 4 imputations are already in progress. Cannot start a new one")
	
	
	
	#set temp dir
	setwd("/home/ubuntu/imputations/")
	uniqueID <- paste("id",sample(100000000:900000000,1),sep="_")
	homeFolderShort<-paste("imputation_folder",uniqueID,sep="_")
	dir.create(homeFolderShort)
	setwd(homeFolderShort)
	homeFolder<-paste("/home/ubuntu/imputations/",homeFolderShort,"/",sep="")
	write.table("Job is not ready yet",file="job_status.txt",col.names=F,row.names=F,quote=F)
	
	
	#unzipping (or not) and moving to new place	
	newTempPath <- paste(homeFolder,paste(uniqueID,"_raw_data",sep=""),sep="/")
	newUnzippedPath <- paste(homeFolder,paste(uniqueID,"_raw_data.txt",sep=""),sep="/")
	file.copy(path, newTempPath)	
	gunzipResults<-unzip(newTempPath,exdir=homeFolder)
	if(length(gunzipResults)==1){ #then its a zip file
		file.rename(gunzipResults, newUnzippedPath)		
	}else{ #then it's probably not
		file.rename(newTempPath, newUnzippedPath)		
	}
	path <- newUnzippedPath
	
	
	testRead<-read.table(path,nrow=10,stringsAsFactors=F)
	if(ncol(testRead)!=4)stop("testRead of file didn't have 4 columns")
	if(unique(sub("[0-9]+$","",testRead[,1]))!="rs")stop("testRead didn't have rs IDs in column 1")
	
	#should probably change this to more permanent
	# system("export PATH=$PATH:/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static")
	# cmd1<-paste("perl -I /home/ubuntu/impute_dir/ -I /home/ubuntu/impute_dir/IO-zlib/share/perl5/ /home/ubuntu/impute_dir/impute_genome.pl -i",path,"-g /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ -o",uniqueID,"-p")
	# cmd1_out<-system(cmd1,intern=T)
	# imputeCommands<-grep("^impute2 ",cmd1_out,value=T)
	# mergeCommands<-grep("^cat ",cmd1_out,value=T)
	# mergeCommands<-gsub("\\.haps","_info",cmd2) #change so it takes the info files instead
	#maybe just skip the mergeCommands and make your own?
	# mergeCommands<-grep("^cat", cmd2,value=T)
	# cmd2<-c(imputeCommands,mergeCommands)
	save(uniqueID,email,file=paste(homeFolder,"variables.rdata",sep=""))
	
	unlink("job_status.txt")
	write.table("Job is ready",file="job_status.txt",col.names=F,row.names=F,quote=F)
	
	
	return(paste("Genome files succesfully uploaded and prepared for imputation. Your unique job-id is",uniqueID,"and when finished, you will receive an email to",email,"that contains download instructions."))
	
	
}




run_imputation<-function(
	rawdata, 
	runDir, 
	shapeit="/home/ubuntu/impute_dir/bin/shapeit",
	plink="/home/ubuntu/impute_dir/plink",
	impute2="/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static/impute2",
	sample_ref="/home/ubuntu/misc_files/sample.reference.txt"
){
	
	
	
	if(class(rawdata)!="character")stop(paste("rawdata must be character, not",class(rawdata)))
	if(length(rawdata)!=1)stop(paste("rawdata must be lengh 1, not",length(rawdata)))
	if(!file.exists(rawdata))stop(paste("Did not find rawdata at path:",rawdata))
	
	if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
	if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
	if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
	
	if(class(shapeit)!="character")stop(paste("shapeit must be character, not",class(shapeit)))
	if(length(shapeit)!=1)stop(paste("shapeit must be lengh 1, not",length(shapeit)))
	if(!file.exists(shapeit))stop(paste("Did not find shapeit at path:",shapeit))
	
	if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
	if(length(plink)!=1)stop(paste("plink must be lengh 1, not",length(plink)))
	if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
	
	if(class(impute2)!="character")stop(paste("impute2 must be character, not",class(impute2)))
	if(length(impute2)!=1)stop(paste("impute2 must be lengh 1, not",length(impute2)))
	if(!file.exists(impute2))stop(paste("Did not find impute2 at path:",impute2))
	
	if(class(sample_ref)!="character")stop(paste("sample_ref must be character, not",class(sample_ref)))
	if(length(sample_ref)!=1)stop(paste("sample_ref must be lengh 1, not",length(sample_ref)))
	if(!file.exists(sample_ref))stop(paste("Did not find sample_ref at path:",sample_ref))
	
	
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
				nchar(logStrand[,10])==1 &
				!logStrand[,5] %in% c("D","I") &
				!logStrand[,6] %in% c("D","I") 
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
		
		
		
		#running the shapeit command (with two people, the right one and a placeholder heterozygote
		cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
		system(cmd4)
		
		
		#removing the placeholder person again
		cmd5_1<-paste("cut --delimiter=' ' -f 1-7 step_4_chr",chr,".haps > step_5_chr",chr,".haps",sep="")
		system(cmd5_1)
		cmd5_2<-paste("head -n 3 step_4_chr",chr,".sample > step_5_chr",chr,".sample",sep="")
		system(cmd5_2)
		
		
		
		#detect max length of each chromosome
		cmd6<-paste("zcat /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz | tail -n 1 | cut --delimiter=\\  -f 2",sep="")
		maxPos<-as.numeric(system(cmd6,intern=T))
		
		
		#iterate over 5e6 chunks
		starts<-seq(0,maxPos,5e6)
		for(i in 1:length(starts)){
			start <- starts[i]
			end <- start+5e6
			
			
			cmd7<-paste(impute2," -m /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt -h /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz -l /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz -known_haps_g step_5_chr",chr,".haps -int ",start," ",end," -Ne 20000 -o step_7_chr",chr,"_",i,sep="")
			system(cmd7)
		}
	}
}


summarize_imputation<-function(
	runDir,
	uniqueID,
	destinationDir,
	gtools="/home/ubuntu/impute_dir/gtool",
	plink="/home/ubuntu/impute_dir/plink-1.07-x86_64/plink" #note, as of 2015-08-31 this must be plink 1.07, otherwise we get a bug
){
	if(class(runDir)!="character")stop(paste("runDir must be character, not",class(runDir)))
	if(length(runDir)!=1)stop(paste("runDir must be lengh 1, not",length(runDir)))
	if(!file.exists(runDir))stop(paste("Did not find runDir at path:",runDir))
	if(length(grep("/$",runDir))!=0)stop("Please don't use a trailing slash in the runDir")
	setwd(runDir)
	
	if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
	if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
	
	if(class(destinationDir)!="character")stop(paste("destinationDir must be character, not",class(destinationDir)))
	if(length(destinationDir)!=1)stop(paste("destinationDir must be lengh 1, not",length(destinationDir)))
	if(!file.exists(destinationDir))stop(paste("Did not find destinationDir at path:",destinationDir))
	if(length(grep("/$",destinationDir))!=0)stop("Please don't use a trailing slash in the destinationDir")
	
	if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
	if(length(gtools)!=1)stop(paste("gtools must be lengh 1, not",length(gtools)))
	if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
	
	if(class(plink)!="character")stop(paste("plink must be character, not",class(plink)))
	if(length(plink)!=1)stop(paste("plink must be lengh 1, not",length(plink)))
	if(!file.exists(plink))stop(paste("Did not find plink at path:",plink))
	
	if(file.exists(paste(destinationDir,"/",uniqueID)))stop("The destinationDir already exists. This is a major unforeseen error")
	
	allFiles1<-list.files(runDir)
	step7Files<-grep("^step_7_chr",allFiles1,value=T)
	step7ResultsFiles<-grep("[0-9]$",step7Files,value=T)
	chromosomes<-unique(sub("_[0-9]+$","",sub("^step_7_chr","",step7ResultsFiles)))
	
	for(chr in chromosomes){
		print(paste("Merging chunks in chromosome",chr))
		s <-grep(paste("^step_7_chr",chr,sep=""), step7ResultsFiles,value=T)
		s<-s[order(as.numeric(sub("^.+_","",s)))]
		cmd1<-paste("cat ",paste(s,collapse=" ")," > ",uniqueID,"_chr",chr,".gen",sep="")
		system(cmd1)
	}	
	
	
	
	genFiles<-paste(uniqueID,"_chr",chromosomes,".gen",sep="")
	
	#running a conversion first to plink then to 23andme	
	for(genFile in genFiles){
		
		chr <- sub("\\.gen$","",sub("^.+_chr","",genFile))
		print(paste("Simplifying in chromosome",chr))
		sampleFile<-paste("step_4_chr",chr,".sample",sep="")
		
		#make list of non-indels
		cmd2<-paste("awk -F' ' '{ if ((length($4) > 1 ) || (length($5) > 1 )) print $2 }'",genFile,">",paste("step_8_chr",chr,"_snps_to_exclude",sep=""))
		system(cmd2)
		
		#exclude indels
		cmd3 <- paste(gtools," -S --g ",genFile," --s ",sampleFile," --exclusion step_8_chr",chr,"_snps_to_exclude --og step_8_chr",chr,".gen",sep="")
		system(cmd3)
		
		#Convert to ped format
		cmd4 <- paste(gtools," -G --g step_8_chr",chr,".gen --s ",sampleFile," --chr ",chr," --snp",sep="")
		system(cmd4)
		
		#reform to plink fam/bim/bed file			
		cmd5 <- paste(plink," --file step_8_chr",chr,".gen --recode --transpose --noweb --out step_9_chr",chr,sep="")
		system(cmd5)

		#re-order to 23andme format
		cmd6<-paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' step_9_chr",chr,".tped  > ",sub("\\.gen$","",genFile),".23andme.txt",sep="")
		system(cmd6)
	}
	

	#preparing destinationDir
	dir.create(paste(destinationDir,"/",uniqueID,sep=""))
	prepDestinationDir<-paste(destinationDir,"/",uniqueID,sep="")
	
	#zipping and mvoing 23andme files
	zipFile23andme<-paste(runDir,paste(uniqueID,".23andme.zip",sep=""),sep="/")
	twentythreeandmeFiles<-paste(uniqueID,"_chr",chromosomes,".23andme.txt",sep="")
	zip(zipFile23andme, twentythreeandmeFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
	file.rename(zipFile23andme, paste(prepDestinationDir,basename(zipFile23andme),sep="/"))
	
	
	#zipping gen files
	zipFileGen<-paste(runDir,paste(uniqueID,".gen.zip",sep=""),sep="/")
	zip(zipFileGen, genFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
	file.rename(zipFileGen, paste(prepDestinationDir,basename(zipFileGen),sep="/"))
	
	
	
	returnPaths<-c(
		paste(prepDestinationDir,basename(zipFile23andme),sep="/"),
		paste(prepDestinationDir,basename(zipFileGen),sep="/")
	)
	names(returnPaths)<-c("23andme","gen")
	
	return(returnPaths)
}





