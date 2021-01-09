library("shiny")
library("kandinsky")
library("jsonlite")

source("/home/ubuntu/srv/impute-me/functions.R")

dataFolder<-"/home/ubuntu/data/"



shinyServer(function(input, output) {
  
  get_data <- reactive({
    if(input$goButton == 0){
      return(NULL)
    }
    uniqueID<-gsub(" ","",input$uniqueID)
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError("Did not find a user with this id"))
    }      
    
    
    #json file loading
    json_file<-paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json")
    if(!file.exists(json_file))stop(safeError("Didn't find a json data file. Maybe data was from before implementation of this?"))
    d<-fromJSON(json_file)
    
    #check existence of content
    if(!"kandinsky" %in% names(d)){
      return(NULL)
    }
    
    #extract relevant data
    d1 <- d[["kandinsky"]]
    
    #return data    
    return(d1)
  })
  
  
  
  
  
  output$text_1 <- renderText({ 
    if(input$goButton == 0){
      m<-paste0("Making unique art from your genomic data is not a new idea. However, most places I've seen that offer such service actually just use very few SNPs. In this module all your trait-associated SNPs are combined to create a truly unique piece of art in the style of <u><a href='https://en.wikipedia.org/wiki/Wassily_Kandinsky'>Wassily Kandinsky</a></u>, using the beautiful code available in the <i><u><a href='http://giorasimchoni.com/2017/07/30/2017-07-30-data-paintings-the-kandinsky-package/'>kandinsky</a></u></i> R-package.<br><br>
	     You may ask if the drawing reveals deeper insight about your genome and inner self? Maybe it does, maybe it doesn't. Either way - it is guaranteed to be a unique drawing derived only from your genome. You may print it and use it as art. I do.<br>")
    }else{
      d1 <- get_data()
      if(is.null(d1)){
        m<-paste0("Making unique art from your genomic data is not a new idea. However, most places I've seen that offer such service actually just use very few SNPs. In this module all your trait-associated SNPs are combined to create a truly unique piece of art in the style of <u><a href='https://en.wikipedia.org/wiki/Wassily_Kandinsky'>Wassily Kandinsky</a></u>, using the beautiful code available in the <i><u><a href='http://giorasimchoni.com/2017/07/30/2017-07-30-data-paintings-the-kandinsky-package/'>kandinsky</a></u></i> R-package.<br><br>
	     You may ask if the drawing reveals deeper insight about your genome and inner self? Maybe it does, maybe it doesn't. Either way - it is guaranteed to be a unique drawing derived only from your genome. You may print it and use it as art. I do.<br>")
      }else{
        m<-paste0("Making unique art from your genomic data is not a new idea. However, most places I've seen that offer such service actually just use very few SNPs. In this module all your trait-associated SNPs are combined to create a truly unique piece of art in the style of <u><a href='https://en.wikipedia.org/wiki/Wassily_Kandinsky'>Wassily Kandinsky</a></u>, using the beautiful code available in the <i><u><a href='http://giorasimchoni.com/2017/07/30/2017-07-30-data-paintings-the-kandinsky-package/'>kandinsky</a></u></i> R-package.<br><br>You may ask if the drawing reveals deeper insight about your genome and inner self? Maybe it does, maybe it doesn't. Either way - it is guaranteed to be a unique drawing derived only from your genome. You may print it and use it as art. I do.<br><br>Additionally, our algorithms took a little inspiration from <u><a href='https://github.com/schollz/poetry-generator'>the Backus-Naur algorithm</a></u>, and wrote you a poem while analysing.<br><br>It's printed after the painting.<br>")
      }
      return(m)
    }
  })
  
  
  
  output$text_2 <- renderText({ 
    m<-""
    if(input$goButton > 0){
      d1 <- get_data()
      if(!is.null(d1)){
        if("poem" %in% names(d1)){
          m<-d1[["poem"]]
        }
      }
    }
    return(m)
  })
  
  
  output$plot_1 <- renderPlot({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      uniqueID<-isolate(gsub(" ","",input$uniqueID))
      if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
      if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
      if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
        Sys.sleep(3) #wait a little to prevent raw-force fishing	
        stop(safeError(paste("Did not find a user with this id",uniqueID)))
      }
      gwas_snps_path<-paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.all_gwas.gz")
      if(!file.exists(gwas_snps_path))stop(safeError("Didn't find a gwas cached file for this user"))
      genotypes<-read.table(gwas_snps_path,sep="\t",stringsAsFactors = F,header=T)
      if(nrow(genotypes)==0)stop(safeError("Found an empty gwas cached file for this user"))
      
      #making sure it's a set number of SNP, to ensure same picture even after updates
      original_snp_set_path <- "/home/ubuntu/srv/impute-me/kandinsky/2019-03-04_original_snp_set.txt.gz"
      original_snp_set <- read.table(original_snp_set_path,stringsAsFactors = F,sep="\t")[,1]
      if(any(!genotypes[,"X"] %in% original_snp_set)){
        genotypes<-genotypes[genotypes[,"X"] %in% original_snp_set ,]
      }
      
      
      kandinsky(genotypes)
      
      
      
      #write the score to the log file
      log_function<-function(uniqueID,study_id,genotypes){
        user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"kandinsky",uniqueID)
        m<-paste(m,collapse="\t")
        if(file.exists(user_log_file)){
          write(m,file=user_log_file,append=TRUE)
        }else{
          write(m,file=user_log_file,append=FALSE)
        }
      }
      try(log_function(uniqueID,study_id,genotypes))
      
    }
  })
  
  
  
  output$downloadData <- downloadHandler(
    filename = paste(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"kandinsky.pdf",sep="_"),
    content = function(file){
      
      uniqueID<-isolate(gsub(" ","",input$uniqueID))
      if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
      if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
      if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
        Sys.sleep(3) #wait a little to prevent raw-force fishing	
        stop(safeError(paste("Did not find a user with this id",uniqueID)))
      }
      
      gwas_snps_path<-paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,".cached.all_gwas.gz")
      if(!file.exists(gwas_snps_path))stop(safeError("Didn't find a gwas cached file for this user"))
      genotypes<-read.table(gwas_snps_path,sep="\t",stringsAsFactors = F,header=T)
      if(nrow(genotypes)==0)stop(safeError("Found an empty gwas cached file for this user"))
      
      #making sure it's a set number of SNP, to ensure same picture even after updates
      original_snp_set_path <- "/home/ubuntu/srv/impute-me/kandinsky/2019-03-04_original_snp_set.txt.gz"
      original_snp_set <- read.table(original_snp_set_path,stringsAsFactors = F,sep="\t")[,1]
      if(any(!genotypes[,"X"] %in% original_snp_set)){
        genotypes<-genotypes[genotypes[,"X"] %in% original_snp_set ,]
      }
      
      
      pdf(file,width=9,height=7)
      kandinsky(genotypes)
      dev.off()
      
      
    }
  )
  
  
  
  
  
  
  
  
  #The where-to-go-next-question box
  output$text_3 <- renderText({ 
    uniqueID<-gsub(" ","",input$uniqueID)
    if(uniqueID %in% c("","id_XXXXXXXXX", "id_613z86871")){return(".")}
    if(input$goButton == 0){return("..")}
    
    
    if(sample(1:3,1) != 1){return("..")}

    survey_log_file<-"/home/ubuntu/logs/submission/art_survey.txt"
    if(!file.exists(survey_log_file))system(paste("touch",survey_log_file))
    m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"art_survey",uniqueID)
    m<-paste(m,collapse="\t")
    write(m,file=survey_log_file,append=TRUE)
    
    out <- paste0("<div style='background-color: #cfc ; padding: 10px; border: 1px solid green;'>
<p>Dear user<br></p>
<p><img style='padding: 0 15px; float: right;' src='../www/small_logo.png'>We are aiming to develop this module further and are interested in your opinion. Which of these options would you find most interesting (tracked links):<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><u><a href='http://bit.ly/337AyxM'>1) Generating GAN-network-based art spanning from Rococo landscapes to modern art, then training user-impressions on genetics to provide incrementally better and better personalized precision-art<a/></u></b><br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><u><a href='http://bit.ly/323mzrB'>2) Generating downloadable Minecraft worlds based on your personal DNA, complete with walkable double helix tunnels, gene-annotations and warp-points for travelling to your favourite gene<a/></u></b><br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><u><a href='http://bit.ly/2NtbWsQ'>3) Focus on the written poetry: explore how the AI-generated poetry can be made even more relevant and personal, based on trait and ancestry predictions<a/></u></b><br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><u><a href='http://bit.ly/2JFMaQY'>4) Dont spend time on this, stick to health genetics!<a/></u></b><br><br>
                    Thank you.</p></div>")
    
    return(out)
    
    
    
  })
  
  
  
  
})
  
  
  
  
  
  