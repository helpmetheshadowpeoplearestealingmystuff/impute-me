library("shiny")
library("plotly")

# options(shiny.sanitize.errors=F)

#real
source("/home/ubuntu/srv/impute-me/functions.R")
load("/home/ubuntu/srv/impute-me/ethnicity/2017-04-03_ethnicity_snps.rdata")
load("/home/ubuntu/srv/impute-me/ethnicity/2017-04-03_ethnicity_pca.rdata")
ethnicity_desc<-read.table("/home/ubuntu/srv/impute-me/ethnicity/2017-04-03_ethnicity_descriptions.txt",sep="\t",header=T,stringsAsFactors = F,row.names=1)

#local (shiny)
# source("../functions_local.R")
# load("2017-04-03_ethnicity_snps.rdata")
# load("2017-04-03_ethnicity_pca.rdata")
# ethnicity_desc<-read.table("2017-04-03_ethnicity_descriptions.txt",sep="\t",header=T,stringsAsFactors = F,row.names=1)
# load("test_genotypes.rdata")

#local
# source("functions_local.R")
# load("ethnicity/2017-04-03_ethnicity_snps.rdata")
# load("ethnicity/2017-04-03_ethnicity_pca.rdata")
# ethnicity_desc<-read.table("ethnicity/2017-04-03_ethnicity_descriptions.txt",sep="\t",header=T,stringsAsFactors = F,row.names=1)
# load("ethnicity/test_genotypes.rdata")


# Define server logic for a template
shinyServer(function(input, output){
  output$text_1 <- renderText({ 
    if(input$goButton == 0){
      m<-paste0("There are several ways to investigate genotype-based ethnicity, many center around assigning country-of-ancestry percentage ('IBD methods'). This ethnicity module takes a different, but more simple, approach. Starting from the large <u><a href='http://www.internationalgenome.org/'>1000 genomes project</a></u>, it identifies the ~1000 SNPs that are most ethnicity dependent. The module then performs a cluster analysis (<u><a href='https://en.wikipedia.org/wiki/Principal_component_analysis'>'PCA'</a></u>) of each of the 1000 genomes-project samples, as well as your sample. You can then investigate which known ethnicity your genome is most similar to. Your genome is indicated as a slightly larger black dot in the resulting plot, you may have to zoom in to see it.<br>"
      )
      
    }else{
      m<-""
    }
    return(m)
  })
  
  
  output$mainPlot <- renderPlotly({ 
    if(input$goButton == 0){
      return(NULL)
    }
    uniqueID<-isolate(gsub(" ","",input$uniqueID))
    pc_selections<-isolate(input$pc_selections)
    ethnicities<-isolate(input$ethnicities)
    
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing
      stop(safeError("Did not find a user with this id"))
    }
    # 
    if(length(pc_selections)!=3){
      stop(safeError(paste("For a 3D plot you have to select exactly 3 principal components (PCs), not",length(pc_selections))))
    }
    if(length(ethnicities)<1){
      stop(safeError(paste("You need to choose at least one population to show")))
    }
    
      
        
    #get genotypes
    genotypes<-get_genotypes(uniqueID=uniqueID,request=ethnicity_snps, namingLabel="cached.ethnicity")
    ethnicity_snps[,"genotype"]<-genotypes[rownames(ethnicity_snps),"genotype"]
    get_alt_count <- function(x){sum(strsplit(x["genotype"],"/")[[1]]%in%x["alt"])}
    ethnicity_snps[,"alt_count"]<-apply(ethnicity_snps,1,get_alt_count)
    
    
    #quick-calculate the PCA metrics for this person
    you<-data.frame(pop="YOU", super_pop="YOU", gender=NA,stringsAsFactors = F)
    for(pc in 1:5){
      val<-sum(((ethnicity_snps[,"alt_count"] - ethnicity_snps[,"center"])/ethnicity_snps[,"scale"]) * ethnicity_snps[,paste0("rot_PC",pc)])
      you[,paste0("pos_PC",pc)]<-val
    }
    pca<-rbind(pca_data,you)
    
    
    #pick some colours for each super population (first dilute their alpha a little)
    colours <- ethnicity_desc[,"Col"]
    names(colours) <- ethnicity_desc[,"PopulationDescription"]
    
    #also get the long descriptor of each populations
    pca[,"pop_long"]<-ethnicity_desc[pca[,"pop"],"PopulationDescription"]
    
    
    #extract relevant data
    pca[,"sizes"]<-c(rep(0.5, nrow(pca)-1),2)
    pca[,"x"]<-pca[,paste0("pos_",pc_selections[1])]
    pca[,"y"]<-pca[,paste0("pos_",pc_selections[2])]
    pca[,"z"]<-pca[,paste0("pos_",pc_selections[3])]
    
    #only show relevant populations
    pca<-pca[pca[,"super_pop"]%in%c("YOU",ethnicities),]
    
    
    
    #write the score to the log file
    log_function<-function(uniqueID){
      user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"ethnicity",paste(signif(c(pca[nrow(pca),"x"],pca[nrow(pca),"y"],pca[nrow(pca),"z"]),4),collapse="-"),uniqueID,paste(pc_selections,collapse="-"),paste(ethnicities,collapse="-"))
      m<-paste(m,collapse="\t")
      if(file.exists(user_log_file)){
        write(m,file=user_log_file,append=TRUE)
      }else{
        write(m,file=user_log_file,append=FALSE)
      }
    }
    try(log_function(uniqueID))
    
    
    #Effectuate the plot
    plot_ly(pca, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", color= ~pop_long,
            colors = colours, showlegend=F, size = ~sizes, marker = list(symbol = 'circle', sizemode = 'diameter'),
            sizes = c(4, 10),hoverinfo = 'text',  text = pca[,"pop_long"]) %>%
      layout(title = 'Genotype-based ethnicity clustering',
             scene = list(xaxis = list(title = pc_selections[1],
                                       gridcolor = 'rgb(255, 255, 255)',
                                       gridwidth = 2),
                          yaxis = list(title = pc_selections[2],
                                       gridcolor = 'rgb(255, 255, 255)',
                                       gridwith = 2),
                          zaxis = list(title = pc_selections[3],
                                       gridcolor = 'rgb(255, 255, 255)',
                                       gridwith = 2)),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
              
    
    
    
    
    
    })
  
  
  
})


 