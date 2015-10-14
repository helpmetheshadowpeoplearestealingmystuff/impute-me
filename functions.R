

# read configuration file
source("/home/ubuntu/misc_files/configuration.R")
if(!exists("maxImputations"))stop("Didn't find maxImputations")
if(!is.numeric(maxImputations))stop("maxImputations not numeric")
if(length(maxImputations)!=1)stop("maxImputations not length 1")
if(!exists("maxImputationsInQueue"))stop("Didn't find maxImputationsInQueue")
if(!is.numeric(maxImputationsInQueue))stop("maxImputationsInQueue not numeric")
if(length(maxImputationsInQueue)!=1)stop("maxImputationsInQueue not length 1")
if(!exists("serverRole"))stop("Didn't find serverRole")
if(!is.character(serverRole))stop("serverRole not character")
if(length(serverRole)!=1)stop("serverRole not length 1")
if(!serverRole%in%c("Hub","Node"))stop("serverRole not Hub or Node")
if(!exists("hubAddress"))stop("Didn't find hubAddress")
if(!is.character(hubAddress))stop("hubAddress not character")
if(length(hubAddress)!=1)stop("hubAddress not length 1")







prepare_23andme_genome<-function(path, email, filename){
	library(tools)
	
	if(class(path)!="character")stop(paste("path must be character, not",class(path)))
	if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
	if(!file.exists(path))stop(paste("Did not find file at path:",path))
	
	if(class(filename)!="character")stop(paste("filename must be character, not",class(filename)))
	if(length(filename)!=1)stop(paste("filename must be lengh 1, not",length(filename)))
	
	if(class(email)!="character")stop(paste("email must be character, not",class(email)))
	if(length(email)!=1)stop(paste("email must be lengh 1, not",length(email)))
	if( email == "" | sub("[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}","",toupper(email)) != ""){
		stop(paste("a real email adress is needed:",email))
	}
	
	acceptedMails<-read.table("/home/ubuntu/misc_files/accepted_emails.txt",stringsAsFactors=F)[,1]
	if(!email%in%acceptedMails){
		m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"not_accepted_email",email,filename)
		m<-paste(m,collapse="\t")
		write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
		stop("At the current stage, the project is only open to backers. Please visit our kickstarter page at: http://kck.st/1VlrTlf - sorry for the inconvenience. Going forward the plan is to run on a more voluntary pricing basis, always as non-profit (see terms-of-use). No data was saved.")
	}
	
	
	
	#check for too many ongoing imputations
	print("check for too many ongoing imputations")
	s<-list.files("/home/ubuntu/imputations/")
	if(length(grep("^imputation_folder",s)) >= maxImputationsInQueue){
		m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"too_many_jobs",email,length(grep("^imputation_folder",s)))
		m<-paste(m,collapse="\t")
		write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
		
		stop(paste("More than",maxImputationsInQueue,"imputations are already in progress. Cannot start a new one. Limited server capacity is the reason for our kickstarter campaign. Support us and gett first in line: kck.st/1VlrTlf"))
	}
	
	
	
	
	
	
	#Create uniqueID 
	print("create uniqueID")
	setwd("/home/ubuntu/imputations/")
	uniqueID <- paste("id_",sample(1000:9000,1),sample(10000:90000,1),sep="")
	numberOfLetters<-sample(c(1,1,2,3),1)
	if(numberOfLetters>0){
		positionsToInsertLetter<-sample(5:(nchar(uniqueID)-1),numberOfLetters)
		
		l<-c(LETTERS,letters)
		l<-l[!l%in%c("o","O")] #I hate it when O is in
		for(x in positionsToInsertLetter){
			substr(uniqueID,x,x)<-sample(l,1)
		}
	}
	
	
	#create imputation folder and output data folder
	print("create imputation folder and output data folder")
	if(uniqueID%in%list.files("/home/ubuntu/data/")){
		m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"double_id",email,uniqueID)
		m<-paste(m,collapse="\t")
		write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
		stop("Problem with unique ID generation. Please re-load and try again.")
	}
	dir.create(paste("/home/ubuntu/data/",uniqueID,sep=""))
	homeFolderShort<-paste("imputation_folder",uniqueID,sep="_")
	dir.create(homeFolderShort)
	setwd(homeFolderShort)
	homeFolder<-paste("/home/ubuntu/imputations/",homeFolderShort,"/",sep="")
	write.table("Job is not ready yet",file="job_status.txt",col.names=F,row.names=F,quote=F)
	# 	
	
	# 	
	# 	#unzipping (or not) and moving to new place
	print("#unzipping (or not) and moving to new place")
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
	
	#checking if it is a consistent file
	print("checking if it is a consistent file")
	testRead<-read.table(path,nrow=10,stringsAsFactors=F)
	if(ncol(testRead)!=4)stop("testRead of file didn't have 4 columns")
	if(unique(sub("[0-9]+$","",testRead[,1]))!="rs")stop("testRead didn't have rs IDs in column 1")
	
	
	#checking if this job has not actually been run before
	print("checking if this job has not actually been run before")
	this_person_md5sum <- md5sum(path)
	otherPersons<-list.files("/home/ubuntu/data",full.names=T)
	for(otherPerson in otherPersons){
		if(!file.info(otherPerson)[["isdir"]])next
		if(!file.exists(paste(otherPerson,"pData.txt",sep="/")))next
		other_person_md5sum<-read.table(paste(otherPerson,"pData.txt",sep="/"),sep="\t",header=T,stringsAsFactors=F)[1,"md5sum"]
		if(is.null(other_person_md5sum))next
		if(this_person_md5sum == other_person_md5sum){
			m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"md5sum_match",email,this_person_md5sum)
			m<-paste(m,collapse="\t")
			write(m,file="/home/ubuntu/misc_files/submission_log.txt",append=TRUE)			
			unlink(paste("/home/ubuntu/data/",uniqueID,sep=""),recursive=T)
			unlink(homeFolder,recursive=T)
			stop("A person with this genome was already analyzed by the system. Write an email to lassefolkersen@gmail.com if you wish to clear this flag.")
		}
	}
	
	
	print("Finalize")
	save(uniqueID,email,filename,file=paste(homeFolder,"variables.rdata",sep=""))
	unlink("job_status.txt")
	write.table("Job is ready",file="job_status.txt",col.names=F,row.names=F,quote=F)
	
	
	return(paste("Genome files succesfully uploaded. Tomorrow you will receive an email to",email,"with download-instructions (look in your spam filter if not). You can close this browser window."))
	
	
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
	for(chr in c("X",as.character(1:22))){
		
		#First in loop - extract only one specific chromosome
		cmd2<-paste(plink," --file step_1 --chr ",chr," --recode --out step_2_chr",chr," --exclude step_2_exclusions",sep="")
		system(cmd2)
		
		#Then check for strand flips etc. 
		cmd3<-paste(shapeit," -check --input-ped step_2_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_2_chr",chr,"_shapeit_log",sep="")
		system(cmd3)
		
		
		#Many homozygote SNPs will fail the check, because, well - of course, they don't have the ref-allele. So we make more detailed R script for sorting them
		logFile<-read.table(paste("step_2_chr",chr,"_shapeit_log.snp.strand",sep=""),sep='\t',stringsAsFactors=FALSE,header=F,skip=1)
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
		
		#This removes any cases where there are more than to alleles involved
		forceHomozygoteTable<-forceHomozygoteTable[sapply(apply(forceHomozygoteTable[,c(5,6,9,10)],1,unique),length)==2,]
		
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
		cmd4<-paste(shapeit," --input-ped step_3_chr",chr,".ped step_2_chr",chr,".map -M /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/genetic_map_chr",chr,"_combined_b37.txt --input-ref /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.hap.gz /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ALL_1000G_phase1integrated_v3_chr",chr,"_impute.legend.gz ",sample_ref," --output-log step_4_chr",chr,"_shapeit_log --exclude-snp step_3_chr",chr,"_exclusions -O step_4_chr",chr,sep="")
		system(cmd4)
		
		
		#checking for errors and stopping if there are any. No point to continue otherwise
		log<-readLines(paste("step_4_chr",chr,"_shapeit_log.log",sep=""))
		if(substr(log[length(log)],1,5)=="ERROR"){
			m<-paste("At chr",chr," the shapeit failed. Check this file for explanation: step_4_chr",chr,"_shapeit.log",sep="")
			write.table(m,file="master_imputation_log.txt",col.names=F,row.names=F,quote=F)
			stop(m)
		}
		
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
		
		
		#The step 8 and also 9 sometime fails for no apparent reason. Probably memory. We therefore make a checkup, where
		#it is checked if the file actually exists and if not - a more complicated step splits it up in chunks.
		#It's not looking nice, but at least the split-up only needs to run in very low memory settings
		fileExists<-file.exists(paste(sub("\\.gen$","",genFile),".23andme.txt",sep=""))
		if(fileExists){
			size<-file.info(paste(sub("\\.gen$","",genFile),".23andme.txt",sep=""))["size"]
		}else{
			size<-0	
		}
		
		#	arbitraly re-run if it's less than 100 bytes (fair to assume something was wrong then)
		if(size<100 ){
			print(paste("retrying step 8-9 command for chr",chr,". Trying to split it in pieces (non-normal low memory running)"))
			cmd7 <- paste("split --verbose --lines 5000000 step_8_chr",chr,".gen step_8_extra_chr",chr,".gen",sep="")
			system(cmd7)
			chunks<-grep(paste("step_8_extra_chr",chr,".gena[a-z]$",sep=""),list.files(runDir),value=T)
			for(chunk in chunks){
				ch<-sub("^.+\\.","",chunk)
				cmd8 <- paste(gtools," -G --g ",chunk," --s ",sampleFile," --chr ",chr," --snp",sep="")
				system(cmd8)
				#reform to plink fam/bim/bed file			
				cmd9 <- paste(plink," --file ",chunk," --recode --transpose --noweb --out step_9_chr",chr,"_",ch,sep="")
				system(cmd9)
				#re-order to 23andme format
				cmd10<-paste("awk '{ print $2 \"\t\" $1 \"\t\"$4\"\t\" $5 $6}' step_9_chr",chr,"_",ch,".tped  > step_9_chr",chr,"_split_",ch,".txt",sep="")
				system(cmd10)
			}
			cmd11<-paste("cat ",paste(paste("step_9_chr",chr,"_split_",sub("^.+\\.","",chunks),".txt",sep=""),collapse=" ")," > ",sub("\\.gen$","",genFile),".23andme.txt",sep="")
			system(cmd11)			
		}
	}
	
	
	
	#preparing destinationDir
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
	
	
	#move the original file as well
	zipFileOriginal<-paste(runDir,paste(uniqueID,".input_data.zip",sep=""),sep="/")
	zip(zipFileOriginal, paste(uniqueID,"_raw_data.txt",sep=""), flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
	file.rename(zipFileOriginal, paste(prepDestinationDir,basename(zipFileOriginal),sep="/"))
	
	
	#return paths
	returnPaths<-c(
		paste(prepDestinationDir,basename(zipFile23andme),sep="/"),
		paste(prepDestinationDir,basename(zipFileGen),sep="/")
	)
	names(returnPaths)<-c("23andme","gen")
	
	return(returnPaths)
}









get_genotypes<-function(
	uniqueID,
	request,
	gtools="/home/ubuntu/impute_dir/gtool"){
	
	
	#checking data in uniqueID's home folder
	if(class(uniqueID)!="character")stop(paste("uniqueID must be character, not",class(uniqueID)))
	if(length(uniqueID)!=1)stop(paste("uniqueID must be lengh 1, not",length(uniqueID)))
	idFolder<-paste("/home/ubuntu/data",uniqueID,sep="/")
	if(!file.exists(idFolder))stop(paste("Did not find an idFolder at",idFolder))
	genZipFile<-paste(idFolder,"/",uniqueID,".gen.zip",sep="")
	if(!file.exists(genZipFile))stop(paste("Did not find a .gen file in idFolder at",idFolder))
	inputZipFile<-paste(idFolder,"/",uniqueID,".input_data.zip",sep="")
	if(!file.exists(inputZipFile))stop(paste("Did not find a .input_data file in idFolder at",idFolder))
	cachedGenotypeFile<-paste(idFolder,"/",uniqueID,".cached.gz",sep="")
	if(!file.exists(cachedGenotypeFile))print(paste("Did not find a chachedGenotypeFile file in idFolder at",idFolder,"but that's no problem"))
	
	#creating a temp folder to use
	idTempFolder<-paste("/home/ubuntu/data",uniqueID,"temp",sep="/")
	if(file.exists(idTempFolder))stop(paste("Temp folder exists, this could indicate that",uniqueID,"is already worked on. Wait a little, or write administrators if you think this is a mistake"))
	
	
	#checking other variables
	if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
	if(length(gtools)!=1)stop(paste("gtools must be lengh 1, not",length(gtools)))
	if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
	
	if(class(request)!="data.frame")stop(paste("request must be data.frame, not",class(request)))
	if(!"chr_name"%in%colnames(request))stop("request object must have a column 'chr_name'")
	if("already_exists"%in%colnames(request))print("request object had a column 'already_exists', this will be overwritten")
	if(!any(substr(rownames(request),1,2)%in%"rs")){
		if(!any(substr(rownames(request),1,1)%in%"i")){
			stop("Not a single rs id was found among the rownames of the request. Really?")
		}
	}
	
	
	#checking existence of already cached genotypes
	if(file.exists(cachedGenotypeFile)){
		cachedGenotypes<-read.table(cachedGenotypeFile,header=T,stringsAsFactors=F,row.names=1)
		snpsAlreadyCached<-rownames(cachedGenotypes)
		requestDeNovo<-request[!rownames(request)%in%snpsAlreadyCached,,drop=F]
	}else{
		requestDeNovo<-request
	}
	
	
	#If there are anything novel, extract it from zip (takes a long time)
	if(nrow(requestDeNovo)>0){
		dir.create(idTempFolder)
		chromosomes<-unique(requestDeNovo[,"chr_name"])
		contents<-unzip(genZipFile,list=T)
		
		#if input is in as a chromosome, use the 23andmefile as input
		if("input"%in%chromosomes){
			snpsFromInput<-requestDeNovo[requestDeNovo[,"chr_name"]%in%"input","SNP"]
			outZip<-unzip(inputZipFile, overwrite = TRUE,exdir = idTempFolder, unzip = "internal",)
			cmd0 <- paste("grep -E '",paste(paste(snpsFromInput,"\t",sep=""),collapse="|"),"' ",outZip,sep="")
			input_genotypes<-system(cmd0,intern=T)
			input_genotypes<-do.call(rbind,strsplit(input_genotypes,"\t"))
			input_genotypes[,4]<-sub("\r$","",input_genotypes[,4])
			if(any(nchar(input_genotypes[,4])!=2))stop("input data must have length 2 genotypes")
			input_genotypes[,4]<-paste(substr(input_genotypes[,4],1,1),substr(input_genotypes[,4],2,2),sep="/")
			genotypes<-data.frame(row.names=input_genotypes[,1],genotype=input_genotypes[,4],stringsAsFactors=F)
		}else{
			genotypes<-data.frame(genotype=vector(),stringsAsFactors=F)
		}
		
		#if any normal style chromosome names are in use the gen files
		if(any(c(as.character(1:22),"X")%in%chromosomes)){
			gensToExtract<-paste(uniqueID,"_chr",chromosomes,".gen",sep="")
			if(!all(gensToExtract%in%contents[,"Name"])){
				missing<-gensToExtract[!gensToExtract%in%contents[,"Name"]]
				stop(paste("These were missing in the zip-gen file:",paste(missing,collapse=", ")))
			}
			outZip<-unzip(genZipFile, files = gensToExtract, overwrite = TRUE,exdir = idTempFolder, unzip = "internal",)
			
			f<-file(paste(idTempFolder,"/samples.txt",sep=""),"w")
			writeLines("ID_1 ID_2 missing sex",f)
			writeLines("0 0 0 D",f)
			writeLines("John Doe 0.0 2 ",f)#gender probably doesn't matter here
			close(f)
			
			
			#looping over all chromosomes and extracting the relevant genotypes in each using gtools
			for(chr in chromosomes){
				
				#This is wrapped in a try block, because it has previously failed from unpredictble memory issues, so it's better to give a few tries
				for(tryCount in 1:5){
					print(paste("Getting ped and map file at chr",chr," - try",tryCount))
					gen<-paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen",sep="")
					snpsHere<-rownames(requestDeNovo)[requestDeNovo[,"chr_name"]%in%chr]
					write.table(snpsHere,file=paste(idTempFolder,"/snps_in_chr",chr,".txt",sep=""),quote=F,row.names=F,col.names=F)
					cmd1<-paste(gtools," -S --g " , gen, " --s ",idTempFolder,"/samples.txt --inclusion ",idTempFolder,"/snps_in_chr",chr,".txt",sep="")
					system(cmd1)
					subsetFile<-paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset",sep="")
					if(!file.exists(subsetFile)){
						print(paste("Did not find any of the SNPs on chr",chr))	
						next
					}
					cmd2<-paste(gtools," -G --g " ,subsetFile," --s ",idTempFolder,"/samples.txt --snp --threshold 0.7",sep="")
					system(cmd2)
					
					
					ped<-try(strsplit(readLines(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.ped",sep="")),"\t")[[1]],silent=T)
					map<-try(read.table(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.map",sep=""),stringsAsFactors=FALSE),silent=T)
					
					if(class(ped)!="try-error" & class(map)!="try-error"){
						ped<-ped[7:length(ped)]
						genotypes_here<-data.frame(row.names=map[,2],genotype=sub(" ","/",ped),stringsAsFactors=F)
						break
					}else{
						genotypes_here<-data.frame(row.names=vector(),genotype=vector(),stringsAsFactors=F)
					}
				}
				genotypes<-rbind(genotypes,genotypes_here)
			}
		}
		
		
		genotypes[genotypes[,"genotype"]%in%"N/N","genotype"]<-NA
		stillMissing<-rownames(requestDeNovo)[!rownames(requestDeNovo)%in%rownames(genotypes)]
		genotypes<-rbind(genotypes,data.frame(row.names=stillMissing,genotype=rep(NA,length(stillMissing),stringsAsFactors=F)))
		
		#removing temporary folder
		unlink(idTempFolder,recursive=T)
	}
	
	#merge with cachedGenotypes
	if(nrow(requestDeNovo)>0){
		if(file.exists(cachedGenotypeFile)){
			genotypes<-rbind(cachedGenotypes,genotypes)
			unlink(cachedGenotypeFile)
		}
		f<-gzfile(cachedGenotypeFile,"w")
		write.table(genotypes,file=f,sep="\t",col.names=NA)
		close(f)
	}else{
		genotypes<-cachedGenotypes
	}
	
	
	
	
	return(genotypes)
	
}
















get_GRS<-function(genotypes, betas){
	
	if(class(genotypes)!="data.frame")stop(paste("genotypes must be data.frame, not",class(genotypes)))
	if(!"genotype"%in%colnames(genotypes))stop(paste("genotypes must have a column genotypes"))
	if(unique(sub("[0-9].+$","",rownames(genotypes)))!="rs")stop("genotypes must have rownames starting with rs")
	
	if(class(betas)!="data.frame")stop(paste("genotypes must be data.frame, not",class(betas)))
	necessary_columns<-c("effect_allele","non_effect_allele","Beta")
	if(!all(necessary_columns%in%colnames(betas)))stop(paste("betas must have a column",paste(necessary_columns,collapse=", ")))
	if(unique(sub("[0-9].+$","",rownames(betas)))!="rs")stop("betas must have rownames starting with rs")
	
	# if(!all(rownames(genotypes)%in%rownames(betas)))stop("all SNPs in genotypes must be present in betas")
	if(!all(rownames(betas)%in%rownames(genotypes)))stop("all SNPs in betas must be present in genotypes")
	
	
	
	geneticRiskScore<-0
	for(snp in rownames(betas)){
		if(is.na(genotypes[snp,"genotype"]))next
		genotype<-strsplit(genotypes[snp,],"/")[[1]]
		effect_allele<-betas[snp,"effect_allele"]
		non_effect_allele<-betas[snp,"non_effect_allele"]
		all_alleles<-c(non_effect_allele,effect_allele)
		beta<-betas[snp,"Beta"]	
		geneticRiskScore <- geneticRiskScore + sum(genotype%in%effect_allele) * beta
	}
	return(geneticRiskScore)
	
}








crawl_for_snps_to_analyze<-function(uniqueIDs=NULL){
	#A function that will crawl all data directories to extract all currently worked on SNPs
	
	if(is.null(uniqueIDs)){
		uniqueIDs<-list.files("/home/ubuntu/data/")
	}else{
		if(class(uniqueIDs)!="character")stop("UniqueIDs must be of class character")
		if(!all(file.exists(paste("/home/ubuntu/data/",uniqueIDs,sep=""))))stop("Not all UniqueIDs given were found")
	}
	
	all_SNPs<-data.frame(SNP=vector(),chr_name=vector(),stringsAsFactors = F)		
	for(module in list.files("/srv/shiny-server/gene-surfer",full.names=T)){
		if(!file.info(module)["isdir"])next
		if("SNPs_to_analyze.txt" %in% list.files(module)){
			SNPs_to_analyze<-read.table(paste(module,"/SNPs_to_analyze.txt",sep=""),sep="\t",stringsAsFactors=F,header=T)
			if(!all(c("SNP","chr_name")%in%colnames(SNPs_to_analyze)))stop(paste("In",module,"a SNPs_to_analyze file was found that lacked the SNP and chr_name column"))
			SNPs_to_analyze[,"chr_name"]<-as.character(SNPs_to_analyze[,"chr_name"])
			if(!all(SNPs_to_analyze[,"chr_name"]%in%c(1:22,"X","input")))stop(paste("In",module,"a SNPs_to_analyze had a chr_name column that contained something else than 1:22 and X"))
			all_SNPs<-rbind(all_SNPs,SNPs_to_analyze[,c("SNP","chr_name")])
			
		}
	}
	
	#an extra check for non-discrepant chr info
	if(any(duplicated(all_SNPs[,"SNP"]))){
		duplicates<-all_SNPs[duplicated(all_SNPs[,"SNP"]),"SNP"]
		for(duplicate in duplicates){
			if(length(unique(all_SNPs[all_SNPs[,"SNP"]%in%duplicate,"chr_name"]))!=1)stop(paste("Found a multi-entry SNP",duplicate,"with discrepant chr info"))
		}
		all_SNPs<-all_SNPs[!duplicated(all_SNPs[,"SNP"]),]
	}
	rownames(all_SNPs)<-all_SNPs[,"SNP"]
	
	for(uniqueID in uniqueIDs){
		print(paste("Checking all requested SNPs from",uniqueID))	
		
		genotypes<-try(get_genotypes(uniqueID=uniqueID,request=all_SNPs))
		if(class(genotypes)=="try-error"){
			if(file.exists(paste("/home/ubuntu/data/",uniqueID,"/temp",sep=""))){
				next
			}else{
				print("Some other error happened in the extraction crawler, but probably no cause for alarm:")
				print(genotypes)
			}
		}
		# 		cmd1 <-	paste("sudo chown shiny /home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.gz")
		# 		system(cmd1)
	}
}














make_overview_of_samples<-function(verbose=T){
	uniqueIDs<-list.files("/home/ubuntu/data/")
	all_pData<-list()
	for(uniqueID in uniqueIDs){
		pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
		if(file.exists(pDataFile)){
			all_pData[[uniqueID]]<-read.table(pDataFile,header=T,stringsAsFactors=F)
		}else{
			if(verbose)print(paste("Didn't find a pData file for",uniqueID))	
		}
	}
	all_columns<-unique(unlist(lapply(all_pData,colnames)))
	pData<-as.data.frame(matrix(nrow=0,ncol=length(all_columns),dimnames=list(NULL,all_columns)))
	for(uniqueID in names(all_pData)){
		p<-all_pData[[uniqueID]]
		for(missing_col in all_columns[!all_columns%in%colnames(p)]){
			p[1,missing_col]<-NA
		}
		pData<-rbind(pData,p[,all_columns])
	}
	rownames(pData)<-pData[,"uniqueID"]
	return(pData)
	
	
}




