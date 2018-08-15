library("shiny")
library("openxlsx")
library("jsonlite")
library("igraph")
library("visNetwork")


#for real run
source("/home/ubuntu/srv/impute-me/functions.R")
load("/home/ubuntu/srv/impute-me/diseaseNetwork/2018-02-21_igraph_object.rdata")
link_file<-"/home/ubuntu/srv/impute-me/diseaseNetwork/2018-07-25_link_file.xlsx"
link_all<-read.xlsx(link_file)  


# Define server logic for a template
shinyServer(function(input, output) {
  
  
  #Get the pre-calculated genetic data for this user
  get_json <- reactive({
    if(input$goButton == 0){
      return(NULL)
    }
    uniqueID<-isolate(gsub(" ","",input$uniqueID))
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste("/home/ubuntu/data/",uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing
      stop(safeError("Did not find a user with this id"))
    }
    
    #json file
    json_file<-paste0("/home/ubuntu/data/",uniqueID,"/",uniqueID,"_data.json")
    # json_file<-paste0("id_613z86871_data.json")
    if(!file.exists(json_file))stop(safeError("Didn't find a json data file. Maybe data was from before implementation of this?"))
    d<-fromJSON(json_file)
    
    return(d)
  })
  
  
  
  
  #Get the colour code
  get_colour_code <- reactive({
    if(input$goButton == 0){
      return(NULL)
    }
    d<-get_json()
    
    
    
    
    #get the Z-scores compiled with the ICD-code+GWAS file
    for(i in 1:nrow(link_all)){  
      module <- link_all[i,"module"]
      gwas_code <- link_all[i,"gwas_code"]
      d1 <- d[[module]]
      if(gwas_code %in% names(d1)){
        link_all[i,"score"]<-d1[[gwas_code]]
      }else{
        link_all[i,"score"]<-NA
      }
    }
    
    
    #remove all rows with no z-score.
    link_all <- link_all[!is.na(link_all[,"score"]),]
    
    #remove all copies of the same ICD-code in the rows of the file, by discriminating against the lowest Z-scores
    link_all <- link_all[order(link_all[,"score"]),]
    link_all <- link_all[!duplicated(link_all[,"ICD_code"]),]
    
    
    
    # get colour scheme - first define constants
    center_score <- "grey90"
    high_score <- "red"
    low_score <- "green"
    length_out <- 20
    max_z_score <- 2
    min_z_score <- -2
    
    
    
    #then define the scales
    r1 <- seq(from=col2rgb(center_score)[1],to=col2rgb(high_score)[1],length.out=length_out)
    g1 <- seq(from=col2rgb(center_score)[2],to=col2rgb(high_score)[2],length.out=length_out)
    b1 <- seq(from=col2rgb(center_score)[3],to=col2rgb(high_score)[3],length.out=length_out)
    r2 <- seq(from=col2rgb(low_score)[1],to=col2rgb(center_score)[1],length.out=length_out)
    g2 <- seq(from=col2rgb(low_score)[2],to=col2rgb(center_score)[2],length.out=length_out)
    b2 <- seq(from=col2rgb(low_score)[3],to=col2rgb(center_score)[3],length.out=length_out)
    
    #colours and their values
    pal <- c(rgb(r2,g2,b2,maxColorValue = 256), rgb(r1,g1,b1,maxColorValue = 256))
    bins <- (seq(min_z_score,max_z_score,length.out=length_out*2) - max_z_score/10)[1:(length_out*2)]
    bins[1]<- -Inf
    bins <- c(bins, Inf)
    
    
    
    #getting per-GWAS number
    link_all[,"score_group"]<-cut(link_all[,"score"],breaks=bins)
    link_all[,"score_number"]<-as.numeric(link_all[,"score_group"])
    link_all[,"colour"]<-pal[link_all[,"score_number"]]
    
    
    #getting strongest colours per ICD-10 code (removing duplicates)
    #ICD_link <-data.frame(ICD_code=unique(link[,"ICD_code"]))
    ICD_link<-link_all[order(link_all[,"score_number"],decreasing=T),] #sorts based on score - highest first
    ICD_link <- ICD_link[!duplicated(ICD_link[,"ICD_code"]),]
    rownames(ICD_link) <- ICD_link[,"ICD_code"] #Assign row names after ICD-code
    ICD_link[,"score_group"]<-ICD_link[,"score_number"]<-ICD_link[,"gwas_code"]<-ICD_link[,"module"] <- NULL # removes scoregroup and scorenumber columns
    colnames(ICD_link)[colnames(ICD_link)%in%"score"]<-"max_score"
    
    o<-list(
      link_all=link_all,
      ICD_link=ICD_link
    )
    return(o)
  })
  
  
  #create the network
  network_proxy_select <- reactive({
    if(is.null(input$focus_node)){
      focus_node<-"Feeling fine"
    }
    
    else{
      focus_node<-input$focus_node
    }
    
    uniqueID <- gsub(" ","",input$uniqueID)
    focus_length_out <- 2
    focus_length_in <- 1
    
    
    #first cut the igraph to show all within any distance (include small)
    
    
    i1 <- which(vertex_attr(e)[["name"]]%in%focus_node) #convert name to vertex number
    a1<-t(distances(e,v=i1,mode="out")) #tells the distance of every node from focus_node (out)
    a2<-t(distances(e,v=i1,mode="in")) #tells distance (in)
    c1<-which(a1[,1]<=focus_length_out) #extracts all nodes within the distance from focus_length_out to focus node
    c2<-which(a2[,1]<=focus_length_in)
    c3<-c(c1,c2) #merge lists
    e1<-induced_subgraph(e, c3) #makes subgraph
    
    
    #get the tooltip
    V(e1)$title <- V(e1)$niceName #to get the tooltip
    
    #get the sizes - large for center and close proximity - smaller for farther out. This has to be set within the igraph object
    dr<-range(V(e1)$distance)
    V(e1)$norm_dist <- (dr[2]-V(e1)$distance)/(dr[2]-dr[1])
    V(e1)$size <- (V(e1)$norm_dist + 0.5)*200
    
    
    #get the shape etc.     
    show_small <- which(V(e1)$distance == max(V(e1)$distance))
    V(e1)$shape <- "circle"
    V(e1)$label.cex <- 1
    V(e1)$label.cex[show_small] <- 0.4
    
    
    #getting the colour code
    o<-get_colour_code()
    if(is.null(o)){
      V(e1)$color<-"#F5F5F5"
      
      
      
    }else{
      o2<-o[["ICD_link"]]
      
      #nyt forsog
      safe_names <- V(e1)$name
      safe_names[!safe_names%in%rownames(o2)] <- NA
      V(e1)$color <- o2[safe_names,"colour"]
      
      
      
      no_info_nodes <- which(is.na(V(e1)$color))
      V(e1)$color[no_info_nodes]<-"#F5F5F5"
      E(e1)$color <- "#BDBDBD"
      
      
    }
    
    
    #create layout
    #first is the state when starting, second is when climbing the tree
    if(is.null(input$focus_node) || input$focus_node=="Feeling fine"){ 
      layout <- "layout_nicely"
      
      # > V(e1)
      # + 24/24 vertices, named, from bed099e:
      #   [1] Heading to Hospital Infections          Cancer
      # [4] Blood-related       Diabetes            Psychiatric
      # [7] Nervous system      Eye diseases        Ear diseases
      # [10] Heart and vessels   Lung diseases       Gut and intestinal
      # [13] Skin diseases       Muscle diseases     Genital diseases
      # [16] Pregnancy-related   Birth-related       Congenital
      # [19] Other symptoms      Injury              External
      # [22] Other factors       Special codes       Feeling fine
      # 
      x <- c(0, rep(seq(-1,1,length.out=6),4)[1:22], 0)
      y <- -c(-0.3,rep(c(1,0.75,0.5,0.25),each=6)[1:22], -1)
      x[20:23] <- x[20:23] + 0.4
      V(e1)$x <- x
      V(e1)$y <- y
      
      
    }else{ #
      layout <- "layout_as_tree"
    }
    
    
    
    
    #then create the visNetwork from this igraph object    
    a<-visIgraph(e1)%>%
      visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: wrap;font-family: arial;font-size:18px;font-color:black;') %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = FALSE) %>%
      visEvents(select = "function(nodes) {
            Shiny.onInputChange('focus_node', nodes.nodes);
            ;}") %>%
      visIgraphLayout(layout = layout,randomSeed = 42 )
      

    
    return(a)
  })
  
  
  
  
  #function to get ID-hover working (I think)
  observe({
    nodes_selection <- input$selnodes
    visNetworkProxy("network_proxy_select") %>%
      visSelectNodes(id = nodes_selection) 
  })
  
  
  
  #getting a table of hits in a bubble
  output$table1 <- renderTable({ 
    uniqueID <- gsub(" ","",input$uniqueID)
    o<-get_colour_code()
    
    if(is.null(o) | input$goButton == 0 | is.null(input$focus_node)){
      return(NULL)
    }
    focus_node <- input$focus_node
    
    #write the score to the log file
    log_function<-function(uniqueID,focus_node){
      user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"diseaseNetworks",uniqueID,focus_node)
      m<-paste(m,collapse="\t")
      if(file.exists(user_log_file)){
        write(m,file=user_log_file,append=TRUE)
      }else{
        write(m,file=user_log_file,append=FALSE)
      }
    }
    try(log_function(uniqueID,focus_node))
    
    
    
    o1<-o[["link_all"]]
    
    o1<- o1[o1[,"ICD_code"] %in% focus_node,]
    
    if(nrow(o1)==0)return(NULL)
    
    #calculate percentage    
    # o1[,"percentage"] <- round(pnorm(o1[,"score"])*100)
    # o1[o1[,"percentage"]==100,"percentage"]<-99
    # o1[,"percentage"]<-paste0(o1[,"percentage"],"%")
    
    
    #rename genetics study
    w <- which(o1[,"module"]%in%"AllDiseases")
    n <- o1[w,"gwas_code"]
    n <- sub("([0-9]+)$","(PMID \\1)",gsub("_"," ",n)) #remove underscore and add PMID
    n <- paste(toupper(substring(n, 1,1)), substring(n, 2),sep="", collapse=" ") #capitalize first letter
    o1[w,"gwas_code"] <- n #re-insert
    
    
    # Translate module names
    niceNames <- c("GWAS calculator","Drug response")
    names(niceNames) <- c("AllDiseases","precisionMedicine")
    o1[,"module"] <- niceNames[o1[,"module"]]

    #insert disease name
    o1[,"disease"]<- paste0(V(e)[o1[,"ICD_code"]]$niceName," (",o1[,"ICD_code"],")")
    
    
    #rename and re-order columns
    select <- c("disease","gwas_code","module","score")
    names(select) <- c("Disease (code)","Genetic study","Further details in this Module","Z-score")
    if(!all(select%in%colnames(o1)))stop("Not all columns found")
    
    o1 <- o1[,select]
    colnames(o1)<-names(select)
    
    

    
    
    
    return(o1)
  })
  
  #show the network with focus based on input
  output$plot1 <- renderVisNetwork({
    network <- network_proxy_select()
    return(network)
  })
  
  
  output$text_1 <- renderText({
    if(input$goButton == 0){
      m<-paste0("
<b>Background</b><br><br>Except for the few strong-effect cases, the 'rare disease' or 'mendelian' genetics, much of what we can learn from our genomes does not have a particularly  high impact on our health. If you are a healthy adult, the impact of common disease genetics is likely to be minimal. However, the chance that such knowledge is useful increases if you are anyway being evaluated for sets of symptoms that include a given disease. For example, an increased genetic risk of leukemia may mean very little in a general population, but for patients with systemic joint pain it could be the difference between a wrongful investigation for rheumatoid arthritis or correct investigation for leukemia.<br><br>

This is the purpose of the Disease Network module. By forcing browsing into a pre-defined set of disease-paths, the algorithm provides you only with relevant genetic information. Nothing more, nothing less. In the root of the tree we find 'feeling fine', which is always a neutral colour: People who feel fine don't need to worry about their genetic risk scores. However, when selecting 'heading to hospital', climbing up the tree, the genetic risk scores are revealed as they become relevant. More of the thinking behind this module is explained in <u><a href='https://www.youtube.com/watch?v=ecGL2r28UuA'>this short animation-video from 2017</a></u>.<br><br><br><br>"
      )
    }else{
      m<-""
    }
    return(m)
    
  })
  
  

})


