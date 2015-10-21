

#protect all the original files (my own family) from auto-deletion

existing<-list.files("~/data")


for(uniqueID in existing){
	pDataFile<-paste("/home/ubuntu/data/",uniqueID,"/pData.txt",sep="")
	pData<-try(read.table(pDataFile,sep="\t",header=T,stringsAsFactors=FALSE))
	if(class(pData)=="try-error"){
		print(uniqueID)
		stop()
	}
	pData[1,"protect_from_deletion"]<-TRUE	
	
	
	write.table(pData,file=pDataFile,sep="\t",col.names=T,row.names=F,quote=F)
}