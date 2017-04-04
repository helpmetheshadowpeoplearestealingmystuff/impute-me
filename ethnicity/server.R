library("shiny")
library("plotly")
# options(shiny.error = browser)

options(shiny.sanitize.errors=F)

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
  output$mainPlot <- renderPlotly({ 
    if(input$goButton == 0){
      return(NULL)
    }
    uniqueID<-isolate(gsub(" ","",input$uniqueID))
    pc_selections<-isolate(input$pc_selections)
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
    
    
    pca[,"x"]<-pca[,paste0("~pos_",pc_selections[1])]
    pca[,"y"]<-pca[,paste0("~pos_",pc_selections[2])]
    pca[,"z"]<-pca[,paste0("~pos_",pc_selections[3])]
    plot_ly(pca, x = ~x, y = ~y, z = ~z, type = "scatter3d", mode = "markers", color= ~pop_long,
            colors = colours, showlegend=F, hoverinfo = 'name', size = ~sizes, marker = list(symbol = 'circle', sizemode = 'diameter'),
            sizes = c(4, 10)) %>%
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


 