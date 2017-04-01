library("shiny")
library("plotly")

source("/home/ubuntu/srv/impute-me/functions.R")


#Replace 'template' with name of module throughout the script



load("/home/ubuntu/2017-04-01_sjogren_pca.rdata")

# Define server logic for a template
shinyServer(function(input, output){
  output$mainPlot <- renderPlotly({ 
    # if(input$goButton == 0){
    #   return(NULL)
    # }
    uniqueID<-isolate(gsub(" ","",input$uniqueID))
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError("Did not find a user with this id"))
    }
    
    x = pc[,"CS1"]
    y = pc[,"CS2"]
    z = pc[,"CS3"]
    col <- pc[,"col"]
    # df <- data.frame(x,y,z)
    plot_ly(pc, x = x, y = y, z = z, type = "scatter3d", mode = "markers", color=col,colors = c("red","blue"),scene = list(
      xaxis = list(title = "PC1"), 
      yaxis = list(title = "PC2"), 
      zaxis = list(title = "PC3")))
  
    
    
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