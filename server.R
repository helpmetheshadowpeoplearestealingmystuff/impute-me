library("shiny")
library("R.utils")
# load("data/2015-08-17 merged trans-eQTLs.rdata")


# qsub -I -W group_list=allelic_imbalance -l nodes=1:ppn=1,mem=64gb,walltime=36000


# path<-"/home/ubuntu/impute_dir/genome_Lasse_Folkersen_Full_20140731040800.txt"

# sudo less "/var/log/shiny-server/gene-surfer-shiny-20150821-132140-45236.log"


prepare_23andme_genome_2<-function(path=""){
	setwd("/home/ubuntu/imputations/")
	uniqueID <- paste("id",sample(100000000:900000000,1),sep="_")
	# if(length(grep("^imputation_folder",list.files("~"))) > 4)stop("More than 4 imputations are already in progress. Cannot start a new one")
	
	# if(!file.exists("~/imputations"))dir.create("~/imputations")
	
	
	# homeFolder<-paste("~/imputations/imputation_folder",uniqueID,sep="_")
	# dir.create(homeFolder)
	# setwd(homeFolder)
	
	return(getwd())
}

prepare_23andme_genome<-function(path=""){
	library("R.utils")
	library("mail")
	
	if(class(path)!="character")stop(paste("path must be character, not",class(path)))
	if(length(path)!=1)stop(paste("path must be lengh 1, not",length(path)))
	if(!file.exists(path))stop(paste("Did not find file at path:",path))
	
	
	if(length(grep("^imputation_folder",list.files("/home/ubuntu/imputations/"))) > 4)stop("More than 4 imputations are already in progress. Cannot start a new one")
	
	
	setwd("/home/ubuntu/imputations/")
	uniqueID <- paste("id",sample(100000000:900000000,1),sep="_")
	homeFolderShort<-paste("imputation_folder",uniqueID,sep="_")
	dir.create(homeFolderShort)
	setwd(homeFolderShort)
	homeFolder<-paste("/home/ubuntu/imputations/",homeFolderShort,sep="")
	
	if(sub("^.+\\.","",path)=="gz"){
		gunzip(path)
		path<-sub("\\.gz$","",path)
		if(!file.exists(path))stop(paste("Very weird: Did not find file at path:",path))
	}
	
	testRead<-read.table(path,nrow=10,stringsAsFactors=F)
	if(ncol(testRead)!=4)stop("testRead of file didn't have 4 columns")
	if(unique(sub("[0-9]+$","",testRead[,1]))!="rs")stop("testRead didn't have rs IDs in column 1")
	
	
	cmd1<-paste("perl -I /home/ubuntu/impute_dir/ -I /home/ubuntu/impute_dir/IO-zlib/share/perl5/ /home/ubuntu/impute_dir/impute_genome.pl -i",path,"-g /home/ubuntu/impute_dir/ALL_1000G_phase1integrated_v3_impute/ -o",uniqueID,"-p")
	cmd1_out<-system(cmd1,intern=T)
	
	
	
	imputeCommands<-grep("^impute2 ",cmd1_out,value=T)
	mergeCommands<-grep("^cat ",cmd1_out,value=T)
	
	
	for(i in 1:length(imputeCommands)){
		print(paste("running im",i,"of",length(imputeCommands)))
		cmd_here<-imputeCommands[i]
		cmd_here_out<-system(cmd_here,intern=T)	
	}
	for(i in 1:length(mergeCommands)){
		print(paste("running",i,"of",length(imputeCommands)))
		cmd_here<-imputeCommands[i]
		cmd_here_out<-system(cmd_here,intern=T)	
	}
	
	
	print("Zipping files")
	outputFiles<-grep("[1-9]\\.gen",list.files(homeFolder,full.names=T))
	zipFileOut<-paste(homeFolder,paste(uniqueID,".zip",sep=""),sep="/")
	zip(zipFileOut, outputFiles, flags = "-r9X", extras = "",zip = Sys.getenv("R_ZIPCMD", "zip"))
	
	
	print("Moving zip files to download location and clean up")
	zipFileOthPath <- paste(homeFolder,zipFileOut,sep="")
	finalLocation <- paste("/srv/shiny-server/",zipFileOut,sep="")
	cmd3 <- system("sudo mv", zipFileOthPath, finalLocation)
	system(cmd3,intern=T)
	setwd("/home/ubuntu/imputations/")
	unlink(homeFolder,recursive = TRUE)
	
	print("Getting IP and sending mail")
	ip<-sub("\"}$","",sub("^.+\"ip\":\"","",readLines("http://api.hostip.info/get_json.php", warn=F)))
	location <- paste(ip,basename(fileOut),sep="/")
	message <- paste("For the next 24 hours you can retrieve your imputed genome at this address:\n",location)
	sendmail(recipient=to, subject=subject, message=message, password="rmail")

	print("Wait 24 hours")
	Sys.sleep(24*60*60)
	print("Delete output file")
	unlink(finalLocation)
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
		size <- isolate(input$largeFile[["size"]])
		# combinationFraction <- as.numeric(isolate(input$combinationFraction))
		# maxRows <- as.integer(isolate(input$maxRows))
		# divisions <- as.integer(isolate(input$divisions))
		# constraintCs <- isolate(input$constraint1)
		
		if(is.null(path))return(NULL)
		
		prepare_23andme_genome(path)
		
		
	})
	
	

	
		

})


