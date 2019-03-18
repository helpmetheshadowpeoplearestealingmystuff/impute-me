rm(list=ls())
source("/home/ubuntu/srv/impute-me/functions.R")

library(jsonlite)




d0<-fromJSON("/home/ubuntu/data/id_10d6m84y8/id_10d6m84y8_data.json")
data<-data.frame(row.names=c("ethnicity",names(d0[["AllDiseases"]]),names(d0[["intelligence"]])))

for(folder in list.files("/home/ubuntu/data/",full.names=TRUE)){
  uniqueID <- basename(folder)
  json_path<-paste0(folder,"/",uniqueID,"_data.json")
  if(!file.exists(json_path))next
  d<-fromJSON(json_path)
  if(is.null(d[["AllDiseases"]]))next
  
  #getting all diseases
  d1a <- d[["AllDiseases"]]
  d1a[["documentation"]] <- NULL
  d1b <- try(lapply(d1a,function(x){x[["GRS"]]}),silent=T)
  if(class(d1b)=="try-error"){
    d1b<-d1a
    print(paste(uniqueID,"didn't have the GRS setup"))
  }
  d1c<-data.frame(trait=names(d1b),value=unlist(d1b),stringsAsFactors = F)
  data[rownames(d1c),uniqueID]<-d1c[,"value"]
  
  
  if(!is.null(d[["intelligence"]])){
    d2a <- d[["intelligence"]]
    d2a[["documentation"]] <- NULL
    d2b <- d2a
    d2c<-data.frame(trait=names(d2b),value=unlist(d2b),stringsAsFactors = F)
    data[rownames(d2c),uniqueID]<-d2c[,"value"]
  }
  
  ethnicity<-try(d[["ethnicity"]][["guessed_super_pop"]],silent=F)
  if(class(ethnicity)%in%c("NULL","try-error"))next
  data["ethnicity",uniqueID]<-ethnicity
  
}

save(data,file="2019-03-13_summary_stats_halfway_backup.rdata")



rm(list=ls())
load("../2019-03-13_summary_stats_halfway_backup.rdata")




dist<-table(t(data["ethnicity",])[,1])
signif(dist/sum(dist),2)
# AFR   AMR   EAS   EUR   SAS 
# 0.020 0.085 0.042 0.830 0.024
#Ok - pity, but seems like we can only really use global and EUR (more than 82% EUR users)


sort(rownames(data))

for(scope in c("ALL",names(dist))){
  print(scope)
  if(scope=="ALL"){
    data_here<-data  
  }else{
    data_here<-data[,data["ethnicity",]%in%scope ]
  }
  
  n<-100
  densities<-matrix(nrow=nrow(data)*2, ncol=n, dimnames=list(paste0(rep(rownames(data),each=2),c("_x","_y")),as.character(1:n)))
  
  
  pdf(paste0("2019-03-13_histograms_of_grs_",scope,".pdf"),width=5,height=5)  
  for(trait in rownames(data_here)){
    print(trait)
    x<-as.numeric(t(data_here[trait,])[,1])
    if(sum(!is.na(x)) < 10)next
    x<-x[!is.na(x)]
    d<-density(x,adjust=2,n=n)
    
    hist(x,xlab="GRS",main=trait,prob=T)
    lines(d)
    
    densities[paste0(trait,"_x"),] <-d[["x"]]
    densities[paste0(trait,"_y"),] <-d[["y"]]
    
  }
  dev.off()
  
  save(densities, file = paste0("2019-03-13_densities_",scope,".rdata"))
  
}

