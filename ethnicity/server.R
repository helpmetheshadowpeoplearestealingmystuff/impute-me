library("shiny")
library("plotly")

source("/home/ubuntu/srv/impute-me/functions.R")


load("/home/ubuntu/srv/impute-me/ethnicity/2017-04-03_ethnicity_snps.rdata")
load("/home/ubuntu/srv/impute-me/ethnicity/2017-04-03_ethnicity_pca.rdata")
ethnicity_desc<-read.table("/home/ubuntu/srv/impute-me/ethnicity/2017-04-03_ethnicity_descriptions.txt",sep="\t",header=T,stringsAsFactors = F,row.names=1)


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
    
    if(sum(pc_selections)!=3){
      stop(safeError(pc_selections))
    }
    
    #get genotypes
    genotypes<-get_genotypes(uniqueID=uniqueID,request=ethnicity_snps, namingLabel="cached.ethnicity")
    ethnicity_snps[,"genotype"]<-genotypes[rownames(ethnicity_snps),"genotype"]
    get_alt_count <- function(x){sum(strsplit(x["genotype"],"/")[[1]]%in%x["alt"])}
    ethnicity_snps[,"alt_count"]<-apply(ethnicity_snps,1,get_alt_count)
    
    
    #quick-calculate the PCA metrics for this person
    pc1<-sum(((ethnicity_snps[,"alt_count"] - ethnicity_snps[,"center"])/ethnicity_snps[,"scale"]) * ethnicity_snps[,"rot_PC1"])
    pc2<-sum(((ethnicity_snps[,"alt_count"] - ethnicity_snps[,"center"])/ethnicity_snps[,"scale"]) * ethnicity_snps[,"rot_PC2"])
    pc3<-sum(((ethnicity_snps[,"alt_count"] - ethnicity_snps[,"center"])/ethnicity_snps[,"scale"]) * ethnicity_snps[,"rot_PC3"])
    pc4<-sum(((ethnicity_snps[,"alt_count"] - ethnicity_snps[,"center"])/ethnicity_snps[,"scale"]) * ethnicity_snps[,"rot_PC4"])
    pc5<-sum(((ethnicity_snps[,"alt_count"] - ethnicity_snps[,"center"])/ethnicity_snps[,"scale"]) * ethnicity_snps[,"rot_PC5"])
    
    
    you<-data.frame(pop="YOU", super_pop="YOU", gender=NA,   pos_PC1=pc1,  pos_PC2=pc2,   pos_PC3=pc3,  pos_PC4=pc4 ,    pos_PC5=pc5,stringsAsFactors = F)
    pca<-rbind(pca_data,you)
    
    
    #pick some colours for each super population
    colours<-ethnicity_desc[,"Col"]
    names(colours) <- ethnicity_desc[,"PopulationDescription"]
    
    
    
    #extract relevant data
    x = signif(pca[,"pos_PC1"],4)
    y = signif(pca[,"pos_PC2"],4)
    z = signif(pca[,"pos_PC3"],4)
    col <- ethnicity_desc[,"PopulationDescription"]
    
    
    
    #plot
    plot_ly(pca, x = x, y = y, z = z, type = "scatter3d", mode = "markers", color=col,colors = colours)
    
    
    
   
    
    
    })
  
  
  
})



  # dataFile <- isolate(input$dataFile)
  # headerSelect <- isolate(input$headerSelect)
  # colourFourth <- isolate(input$colourFourth)
  # 
  # 
  # if(length(headerSelect) != 3)stop("Must select exactly three headers")
  # 
  # 
  # data<-read.csv(paste(data_folder,dataFile,sep=""))
  # colnames(data) <- standardHeaders
  # 
  # 
  # x<-data[,headerSelect[1]]
  # y<-data[,headerSelect[2]]
  # z<-data[,headerSelect[3]]
  # df <- data.frame(x,y,z)
  # 
  # if(colourFourth == "No"){
  #   plot_ly(df, x = x, y = y, z = z, type = "scatter3d", mode = "markers")
  #   
  # }else{
  #   #if colourFourth is selected we also colour the by remaining axis
  #   remainingAxis <-standardHeaders[!standardHeaders%in%headerSelect]
  #   df[,"w"]<-cut(data[,remainingAxis],breaks=4)
  #   plot_ly(df, x = x, y = y, z = z, type = "scatter3d", mode = "markers", color=df[,"w"],  colors="Blues")
  # }
  # 
  # 
  # 
  # })
  
  
  #   output$table1 <- renderTable({ 
  # 		if(input$goButton == 0){
  # 			return(NULL)
  # 		}
  # 	  uniqueID<-isolate(gsub(" ","",input$uniqueID))
  # 		if(nchar(uniqueID)!=12)stop("uniqueID must have 12 digits")
  # 		if(length(grep("^id_",uniqueID))==0)stop("uniqueID must start with 'id_'")
  # 		if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
  # 			Sys.sleep(3) #wait a little to prevent raw-force fishing	
  # 			stop("Did not find a user with this id")
  # 		}
  # 
  # 		
  # 		
  # 		
  # 		table_file <-"../template/SNPs_to_analyze.txt"
  # 		table<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)
  # 		rownames(table)<-table[,"SNP"]
  # 		#This will return a copy of the SNPs_to_analyze.txt, with the genotypes of this specific person (=uniqueID) as a new column
  # 		genotypes<-get_genotypes(uniqueID=uniqueID,request=table)
  # 		table[,"Your genotype"]<-genotypes[rownames(table),]
  # 		
  # 		
  # 		#
  # 		#
  # 		#
  # 		#
  # 		#
  # 		#(Optionally) Do any calculations necessary here. Or just return the table as is for showing on web site.
  # 		#
  # 		#
  # 		#
  # 		#
  # 		#
  # 		
  # 		return(table)
  # 		
  # 	},include.rownames = FALSE)
# 