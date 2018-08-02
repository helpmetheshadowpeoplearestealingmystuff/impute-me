rm(list=ls())
source("/home/ubuntu/srv/impute-me/functions.R")

library(jsonlite)




d0<-fromJSON("/home/ubuntu/data/id_549981r10/id_549981r10_data.json")
data<-data.frame(row.names=c("ethnicity",names(d0[["AllDiseases"]])))



for(folder in list.files("/home/ubuntu/data/",full.names=TRUE)){
  uniqueID <- basename(folder)
  json_path<-paste0(folder,"/",uniqueID,"_data.json")
  if(!file.exists(json_path))next
  d1<-fromJSON(json_path)
  d1[["AllDiseases"]][["documentation"]] <- NULL
  d2<-data.frame(trait=names(d1[["AllDiseases"]]),value=unlist(d1[["AllDiseases"]]),stringsAsFactors = F)
  data[,uniqueID]<-d2[rownames(data),"value"]
  ethnicity<-try(d1[["ethnicity"]][["guessed_super_pop"]],silent=F)
  if(class(ethnicity)%in%c("NULL","try-error"))next
  data["ethnicity",uniqueID]<-ethnicity
  
}
save(data,file="2018-07-26_summary_stats_halfway_backup.rdata")




rm(list=ls())
load("../2018-07-26_summary_stats_halfway_backup.rdata")




dist<-table(t(data["ethnicity",])[,1])
signif(dist/sum(dist),2)
# AFR   AMR   EAS   EUR   SAS 
# 0.021 0.094 0.042 0.820 0.024
#Ok - pity, but seems like we can only really use global and EUR (more than 82% EUR users)

for(scope in c("ALL",names(dist))){
  print(scope)
  if(scope=="ALL"){
    data_here<-data  
  }else{
    data_here<-data[,data["ethnicity",]%in%scope ]
  }
  
  n<-100
  densities<-matrix(nrow=nrow(data)*2, ncol=n, dimnames=list(paste0(rep(rownames(data),each=2),c("_x","_y")),as.character(1:n)))
  
  
  pdf(paste0("2018-07-26_histograms_of_grs_",scope,".pdf"),width=5,height=5)  
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
  
  save(densities, file = paste0("2018-07-26_densities_",scope,".rdata"))
  
}

