library("shiny")
# library("R.utils")
# load("data/2015-08-17 merged trans-eQTLs.rdata")


# qsub -I -W group_list=allelic_imbalance -l nodes=1:ppn=1,mem=64gb,walltime=36000


# path<-"/home/ubuntu/impute_dir/genome_Lasse_Folkersen_Full_20140731040800.txt"

# sudo less "/var/log/shiny-server/gene-surfer-shiny-20150821-132140-45236.log"




prepare_23andme_genome<-function(path="", email=""){
	# library("R.utils")
	# library("mail")
	
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
	write.table("Job is not ready yet",file="job_status.txt")

	
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
	system("export PATH=$PATH:/home/ubuntu/impute_dir/impute_v2.3.2_x86_64_static")
	
	cmd1<-paste("perl -I /home/ubuntu/impute_dir/ -I /home/ubuntu/impute_dir/IO-zlib/share/perl5/ /home/ubuntu/impute_dir/impute_genome.pl -i",path,"-g /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ -o",uniqueID,"-p")
	cmd1_out<-system(cmd1,intern=T)
	
	
	
	imputeCommands<-grep("^impute2 ",cmd1_out,value=T)
	mergeCommands<-grep("^cat ",cmd1_out,value=T)
	
	
	cmd2<-c(imputeCommands,mergeCommands)
	save(cmd2,uniqueID,email,file=paste(homeFolder,"imputation_commands.rdata",sep=""))
	
	return(paste("Genome files succesfully uploaded and prepared for imputation. Your unique job-id is",uniqueID,"and when finished, you will receive an email to",email,"that contains download instructions."))
	
	unlink("job_status.txt")
	write.table("Job is ready",file="job_status.txt")
	
}





# Define server logic for random distribution application
shinyServer(function(input, output) {
	
	
	
	output$text1 <- renderText({ 
		paste("Currently selected file is\n",input$largeFile[["name"]],"(size",round(input$largeFile[["size"]]/1000000),"MB)")
	})
	
	
	
	output$text2 <- renderText({ 
		# Take a dependency on input$goButton
		input$goButton
		path <- isolate(input$largeFile[["datapath"]])
		email <- isolate(input$email)

		if(is.null(path))return(NULL)
		
		prepare_23andme_genome(path,email)
		
		
	})
	
	
	
	
	
	
})


