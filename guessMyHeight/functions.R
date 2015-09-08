
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
	
	
	#removing temporary folder
	unlink(idTempFolder,recursive=T)
	
	return(genotypes)
	
}















get_gheight<-function(genotypes, betas){
	
	if(class(genotypes)!="data.frame")stop(paste("genotypes must be data.frame, not",class(genotypes)))
	if(!"genotype"%in%colnames(genotypes))stop(paste("genotypes must have a column genotypes"))
	if(unique(sub("[0-9].+$","",rownames(genotypes)))!="rs")stop("genotypes must have rownames starting with rs")
	
	if(class(betas)!="data.frame")stop(paste("genotypes must be data.frame, not",class(betas)))
	necessary_columns<-c("Effect..Allele","non_effect_allele","Beta")
	if(!all(necessary_columns%in%colnames(betas)))stop(paste("betas must have a column",paste(necessary_columns,collapse=", ")))
	if(unique(sub("[0-9].+$","",rownames(betas)))!="rs")stop("betas must have rownames starting with rs")
	
	if(!all(rownames(genotypes)%in%rownames(betas)))stop("all SNPs in genotypes must be present in betas")
	if(!all(rownames(betas)%in%rownames(genotypes)))stop("all SNPs in betas must be present in genotypes")
	
	
	
	gheight_score<-0
	for(snp in rownames(giant_sup)){
		if(is.na(genotypes[snp,"genotype"]))next
		genotype<-strsplit(genotypes[snp,],"/")[[1]]
		effect_allele<-giant_sup[snp,"Effect..Allele"]
		non_effect_allele<-giant_sup[snp,"non_effect_allele"]
		all_alleles<-c(non_effect_allele,effect_allele)
		beta<-giant_sup[snp,"Beta"]	
		gheight_score <- gheight_score + sum(genotype%in%effect_allele) * beta
	}
	return(gheight_score)
	
}