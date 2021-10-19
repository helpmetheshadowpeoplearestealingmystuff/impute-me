
poem_py_path <- paste0(get_conf("prs_dir_path"),"poetry-generator/poem.py")

export_function<-function(uniqueID){
  
  gwas_snps_path<-paste0(get_conf("data_path"),uniqueID,"/",uniqueID,".cached.all_gwas.gz")
  if(!file.exists(gwas_snps_path))stop(safeError("Didn't find a gwas cached file for this user"))
  genotypes<-read.table(gwas_snps_path,sep="\t",stringsAsFactors = F,header=T)
  if(nrow(genotypes)==0)stop(safeError("Found an empty gwas cached file for this user"))
  # check for poem stuff ready stuff. Just return empty if not found,
  # the kandinsky painting will still work. It's just that deep learning poem.
  # The files required for pre-generating auotomatized poems were not found at: 
  # check out https://github.com/schollz/poetry-generator if 
  # you want to set it up yourself. Basically just have to download the files and place
  # them in poem_py_path.
  if(!file.exists(poem_py_path))return(NULL)
  
  
  
  original_snp_set_path <- paste0(get_conf("code_path"),"kandinsky/2019-03-04_original_snp_set.txt.gz")
  original_snp_set <- read.table(original_snp_set_path,stringsAsFactors = F,sep="\t")[,1]
  if(any(!genotypes[,"X"] %in% original_snp_set)){
    genotypes<-genotypes[genotypes[,"X"] %in% original_snp_set ,]
  }
  
  #check that something is measured
  if(sum(!is.na(genotypes[,"genotype"])) < 1000)stop("Not enough genotypes measured")
  
  #remove non-measured
  genotypes<-genotypes[!is.na(genotypes[,"genotype"]),]
  
  
  #create unique string
  string<-gsub("/","",paste(genotypes[,"genotype"],collapse=""))
  # print(nchar(string)) #typically ~ 50k letters
  
  
  #get the poem (set working dirs before)
  dir_before <- getwd()
  setwd(dirname(poem_py_path))
  cmd1 <- paste0("python ",poem_py_path, " ",string)
  poem<-system(cmd1,intern = T)
  setwd(dir_before)
  
  #reform poem - remove that long input strong and add author
  poem[1] <- ""
  poem <- c(poem,"<p style='font-family:courier;'><div align='center'><i>An algorithm reading your genome, ",format(Sys.time(),"%Y-%m-%d"),"</i></div></p>")
  poem_as_html <- paste(poem,collapse="")
  
  
  #prepare output
  output <- list()
  output[["poem"]] <- poem_as_html
  
  return(output)
}




