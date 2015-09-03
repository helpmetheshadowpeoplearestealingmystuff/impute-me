


uniqueID<-"id_227131719"
uniqueID<-"id_733404623"

giant_sup_path<-"/home/ubuntu/misc_files/GIANT_height_250k_Supplementary_Tables_20131030.txt"
giant_sup<-read.table(giant_sup_path,sep="\t",header=T,stringsAsFactors=F,row.names=1)
giant_sup<-giant_sup[order(abs(giant_sup[,"Beta"]),decreasing=T),]
giant_sup[,"chr_name"]<-giant_sup[,"Chr"]

set.seed(43)
set.seed(41)
request<-giant_sup[sample(rownames(giant_sup),4),]
request<-giant_sup
gtools="/home/ubuntu/impute_dir/gtool"




# genotype
# rs2633761        A/A
# rs13078528       A/A
# rs2596831        G/G
# rs2597513  TRUE/TRUE
# rs11708412       G/G
# rs9816693        G/G
# rs3915129     TRUE/G
# rs13088462    TRUE/C
# rs4256170        G/A
# rs2240919        C/G
# rs2581830     TRUE/C
# rs2034172        A/G
# rs1658351     C/TRUE
# rs6794009        A/A
# rs17806888 TRUE/TRUE
# rs2175513        A/A
# rs12330322       C/C
# rs7633464        G/A
# rs9825951        A/A
# rs1797625  TRUE/TRUE
# rs1533269        C/A
# rs1546391        C/G
# rs6439168        G/G
# rs4974480     A/TRUE
# rs6762606        C/C
# rs9880211        G/G
# rs724016         A/G
# rs11714558 TRUE/TRUE
# rs936339      C/TRUE
# rs4325879     C/TRUE
# rs6441170     TRUE/C
# rs7652177        C/G
# rs509035         G/G
# rs9858528        A/G
# rs16860216       G/A
# rs720390         G/A
# rs2300921     TRUE/C
# rs4686904  TRUE/TRUE


request<-giant_sup[c("rs2581830","rs2597513"),,drop=FALSE]

request<-giant_sup#[c("rs2581830","rs2597513","rs4686904","rs2300921"),,drop=FALSE]
# request<-request[!request[,"chr_name"]%in%6,]

get_genotypes(uniqueID,request)

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
	cachedGenotypeFile<-paste(idFolder,"/",uniqueID,".cached.gz",sep="")
	if(!file.exists(cachedGenotypeFile))print(paste("Did not find a chachedGenotypeFile file in idFolder at",idFolder,"but that's no problem"))
	
	#creating a temp folder to use
	idTempFolder<-paste("/home/ubuntu/data",uniqueID,"temp",sep="/")
	if(file.exists(idTempFolder))stop(paste("Temp folder exists, this could indicate that",uniqueID,"is already worked on. Wait a little, or write administrators if you think this is a mistake"))
	dir.create(idTempFolder)
	
	
	#checking other variables
	if(class(gtools)!="character")stop(paste("gtools must be character, not",class(gtools)))
	if(length(gtools)!=1)stop(paste("gtools must be lengh 1, not",length(gtools)))
	if(!file.exists(gtools))stop(paste("Did not find gtools at path:",gtools))
	
	if(class(request)!="data.frame")stop(paste("request must be data.frame, not",class(request)))
	if(!"chr_name"%in%colnames(request))stop("request object must have a column 'chr_name'")
	if("already_exists"%in%colnames(request))print("request object had a column 'already_exists', this will be overwritten")
	
	
	#checking existence of already cached genotypes
	if(file.exists(cachedGenotypeFile)){
		cachedGenotypes<-read.table(cachedGenotypeFile,header=T,stringsAsFactors=F,row.names=1)
		snpsAlreadyCached<-rownames(cachedGenotypes)
		requestDeNovo<-request[!rownames(request)%in%snpsAlreadyCached,]
	}else{
		requestDeNovo<-request
	}
	

	#If there are anything novel, extract it from zip (takes a long time)
	if(nrow(requestDeNovo)>0){
	chromosomes<-unique(requestDeNovo[,"chr_name"])
	contents<-unzip(genZipFile,list=T)
	
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
	
	genotypes<-data.frame(genotype=vector(),stringsAsFactors=F)
	
	#looping over all chromosomes and extracting the relevant genotypes in each using gtools
	for(chr in chromosomes){
		gen<-paste(idTempFolder,"/",uniqueID,"_chr",chromosomes,".gen",sep="")
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
		# ped<-read.table(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.ped",sep=""),stringsAsFactors=FALSE)
		ped<-strsplit(readLines(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.ped",sep="")),"\t")[[1]]
		ped<-ped[7:length(ped)]
		map<-read.table(paste(idTempFolder,"/",uniqueID,"_chr",chr,".gen.subset.map",sep=""),stringsAsFactors=FALSE)
		
		o<-data.frame(row.names=map[,2],genotype=sub(" ","/",ped),stringsAsFactors=F)
		
		genotypes<-rbind(genotypes,o)
	}
		
	genotypes[genotypes[,"genotype"]%in%"N/N","genotype"]<-NA
	stillMissing<-rownames(requestDeNovo)[!rownames(requestDeNovo)%in%rownames(genotypes)]
	genotypes<-rbind(genotypes,data.frame(row.names=stillMissing,genotype=rep(NA,length(stillMissing),stringsAsFactors=F)))
	}
	
	#merge with cachedGenotypes
	if(file.exists(cachedGenotypeFile)){
		genotypes<-rbind(cachedGenotypes,genotypes)
		unlink(cachedGenotypeFile)
	}
	f<-gzfile(cachedGenotypeFile,"w")
	write.table(genotypes,file=f,sep="\t",col.names=NA)
	close(f)

	
	#removing temporary folder
	unlink(idTempFolder,recursive=T)
	
	return(genotypes)
	
}


