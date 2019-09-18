library("shiny")

source("/home/ubuntu/srv/impute-me/functions.R")
source("/home/ubuntu/srv/impute-me/prsExplainer/support_functions.R")


load("/home/ubuntu/srv/impute-me/prsExplainer/2019-07-27_commonmind_test_data.rdata")



prs_available <- data.frame(
  row.names=c("SCZ Cases\nAll-SNP\nSCZ score","SCZ Cases\nTop-SNP\nSCZ score","BP Cases\nAll-SNP\nBP score"),
  niceNames= c("Schizophrenia All-SNP","Schizophrenia Top-SNP","Bipolar Disorder All-SNP"),
  variability_explained = c(0.094,0.043,0.102),
  twin_heritability = c(0.8,0.8,0.7)
)


shinyServer(function(input, output) {
  
  output$plot_basic_bell_curve <- renderPlot({ 
    genetic_z_score<-input$genetic_z_score
    
    reference_mean<-0
    reference_sd<-1
    xlim<-c(reference_mean - reference_sd*3, reference_mean + reference_sd*3)
    reference_x<-seq(xlim[1],xlim[2],length.out=100)
    reference_y<-dnorm(reference_x,mean=reference_mean,sd=reference_sd)
    ylim <- range(reference_y)
    
    #draw curve
    plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n")
    par(mai=c(0.2,0.1,0.1,0.1))
    lines(x=reference_x,y=reference_y,lty=1,col="blue",lwd=2)
    
    #fill in shading
    if(!all(!reference_x<genetic_z_score)){
      max_GRS_i<-max(which(reference_x<genetic_z_score))
      upper_x<-reference_x[1:max_GRS_i]
      upper_y<-reference_y[1:max_GRS_i]
      x_lines <- c(upper_x,genetic_z_score,genetic_z_score,xlim[1])
      y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
      polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
    }
    
    #draw the main line
    abline(v=genetic_z_score,lwd=3)
    
    
  })
  

  output$plot_heritability <- renderPlot({ 
    trait_label <- input$trait_label
    known = prs_available[trait_label,"variability_explained"]
    total = prs_available[trait_label,"twin_heritability"]
    
    
    
    #set colours  
    colours <- c("#006680FF","#0088AAFF","#2AD4FFFF")
    names(colours) <- c("known","unknown","environment")
    
    #initialize plot
    plot(NULL,xlim=c(0,1),ylim=c(0,2),xaxt="n",yaxt="n",xlab="Variability explained",ylab="",frame=F)
    par(mai=c(0.2,0.1,0.0,0.1))
    
    #plot known genetics
    x1 <- known / 2
    symbols(x=x1,y=1.5,rectangles=matrix(c(known,1),ncol=2),add=TRUE,bg=colours["known"],inches=FALSE)
    
    #plot unknown genetics
    x2 <- known + (total-known) /2
    symbols(x=x2,y=1.5,rectangles=matrix(c(total-known,1),ncol=2),add=TRUE,bg=colours["unknown"],inches=FALSE)
    
    #plot environment
    x3 <- 1 - (1 - total)/2
    symbols(x=x3,y=1.5,rectangles=matrix(c(1-total,1),ncol=2),add=TRUE,bg=colours["environment"],inches=FALSE)
    
    
    #get position of 'genetics text
    x4 <- total / 2
    
    
    #plot on-top of boxes texts (afterwards so they are not overwritten)
    text(x=x1,y=1.5,label="This\nscore",srt=0)
    if(total - known > 0.2){
      text(x=x2,y=1.5,label="Unknown\ngenetics",srt=0,col="grey30")  
    }
    
    
    #plot under-boxes texts (afterwards so they are not overwritten)
    text(x=x4,y=0.5,label="Genetics",srt=0)
    text(x=x3,y=0.5,label="Environment",srt=0)
    
    #plot horizontal braces for genetics
    k <- 0.01
    lines(x=c(0+k,total-k),y=c(0.8,0.8),col="grey60")
    lines(x=c(0+k,0),y=c(0.8,0.9),col="grey60")
    lines(x=c(total-k,total),y=c(0.8,0.9),col="grey60")
    
    
    #plot horizontal braces for environment
    k <- 0.01
    lines(x=c(total+k,1-k),y=c(0.8,0.8),col="grey60")
    lines(x=c(total+k,total),y=c(0.8,0.9),col="grey60")
    lines(x=c(1-k,1),y=c(0.8,0.9),col="grey60")
    
    
  })
  
  
    
  output$text_basic_bell_curve <- renderText({
    genetic_z_score<-input$genetic_z_score
    if(genetic_z_score>0){
      signed_score<-paste0("+",genetic_z_score)
    }else{
      signed_score<-as.character(genetic_z_score)
    }
    
    #wordify the number
    if(genetic_z_score< -1.9){
      m1 <- "a lot below the average"
    }else if(genetic_z_score< -1.2){
      m1 <- "quite a bit below the average"
    }else if(genetic_z_score< -0.5){
      m1 <- "a bit below the average"
    }else if(genetic_z_score< 0.5){
      m1 <- "pretty average"
    }else if(genetic_z_score< 1.2){
      m1 <- "a bit above the average"
    }else if(genetic_z_score< 1.9){
      m1 <- "quite a bit above the average"
    }else{
      m1 <- "a lot above the average"
    }
    
    m2 <- paste0("This is the basic plot shown for any trait in the complex diseases module (and other modules). It shows the distribution of scores in everybody*. ",
                 "Having a genetic score of ",signed_score, " simply indicates your place on this distribution, i.e. you are ",m1," with regards to known genetic risk. What that means in terms of overall disease risk depends on many other factors as further illustrated in the other examples on this page. The dark-blue part in the bar below, labelled 'This score', shows how much variability the calculated genetic risk explains.",
                 "<br><small>*(everybody here means a representative group of healty people of the same ancestry as you).<small><br><br>")
    
    return(m2)
  })
  
  output$plot_example_results <- renderPlot({ 
    genetic_z_score<-input$genetic_z_score
    
    trait_label <- input$trait_label
    control_label<-sub("[A-Z]+ Cases","Controls",trait_label)
    
    #subset to relevant trait
    d1<-data[data[,"group"]%in%c(trait_label,control_label),]
    d1[,"group"]<-factor(d1[,"group"],levels=c(control_label,trait_label))
    
    #get counts
    control_above<-which(d1[,"group"]%in%control_label & d1[,"value"] > genetic_z_score)
    case_above<-which(d1[,"group"]%in%trait_label & d1[,"value"] > genetic_z_score)
    control_below<-which(d1[,"group"]%in%control_label & d1[,"value"] <= genetic_z_score)
    case_below<-which(d1[,"group"]%in%trait_label & d1[,"value"] <= genetic_z_score)
    
    #colour entries controls above level differently
    d1[,"col"] <-"dodgerblue"
    d1[control_above,"col"] <-"blue2"
    d1[case_above,"col"] <-"blue4"
    
    #do plot
    fun_plot_groupwise_expression_data_20100830(
      d1,pointPch=19,
      combinations = NULL,main="",
      ylab="Genetic Z-score",horizontalScatterSpacing=0.1,plotAxis=F,plotSummary=NULL)
    axis(1,at=c(1,2),label=c("Healthy","Not healthy"))
    abline(h=genetic_z_score,lwd=4,col="blue4")
    
    
  })
  
  
  output$text_example_results <- renderText({
    genetic_z_score<-input$genetic_z_score
    trait_label <- input$trait_label
    control_label<-sub("[A-Z]+ Cases","Controls",trait_label)
    
    if(genetic_z_score>0){
      signed_score<-paste0("+",genetic_z_score)
    }else{
      signed_score<-as.character(genetic_z_score)
    }
    
    
    #subset to relevant trait
    d1<-data[data[,"group"]%in%c(trait_label,control_label),]
    d1[,"group"]<-factor(d1[,"group"],levels=c(control_label,trait_label))
    
    #get counts
    control_above<-which(d1[,"group"]%in%control_label & d1[,"value"] > genetic_z_score)
    case_above<-which(d1[,"group"]%in%trait_label & d1[,"value"] > genetic_z_score)
    control_below<-which(d1[,"group"]%in%control_label & d1[,"value"] <= genetic_z_score)
    case_below<-which(d1[,"group"]%in%trait_label & d1[,"value"] <= genetic_z_score)
    
    #for calculations      
    ppv <- signif(length(case_above) / (length(case_above) + length(control_above)),3)
    npv <- signif(length(control_below) / (length(case_below) + length(control_below)),3)
    fpr <- round((length(control_above) / (length(control_above) + length(control_below)))*100)
    tpr<- round((length(case_above) / (length(case_above) + length(case_below)))*100)
    
    disease<-sub(" .+$","",trait_label)
    score_name <- prs_available[trait_label,"niceNames"]
    
    m<- paste0("To illustrate what a genetic risk score means, we analyzed ",nrow(d1)," individuals with or without disease in the pipeline for the ",score_name," disease-score. ",
               "Of the healthy individuals ",length(control_above)," of the ",length(control_above)+length(control_below)," (",fpr,"%) had a score of ",signed_score," or higher. ",
               "Of the individuals with disease ",length(case_above), " of the ",length(case_above)+length(case_below)," (",tpr,"%) had a score of ", signed_score, " or higher. The difference between these two groups directly corresponds to the 'Variability explained' in the blue bar shown in <i>Basic reporting</i>. <br><br>")
    
    return(m)
  })
  
  
  
  output$plot_burden_jar_1 <- renderPlot({ 
    genetic_z_score<-input$genetic_z_score
    environment_z_score<-input$environment_z_score
    
    genetic_burden <- as.integer(genetic_z_score + 3)
    environment_burden <- as.integer(environment_z_score + 3)
    
    
    
    draw_jar(environment_burden,genetic_burden)    
    
  })
  
  
  
  
  
  output$text_burden_jar <- renderText({
    genetic_z_score<-input$genetic_z_score
    
    
    m <- paste0("This 'disease-jar' illustration shows the concept from the book <u><a href='https://www.amazon.com/Families-About-Genetics-Psychiatric-Illness/dp/0393705498'>How to Talk with Families About Genetics and Psychiatric Illness</a></u>. It illustrates how overall disease risk is a combination of genetic effects ('our DNA') combined with the effects of our environment ('everything else, including life-choices, events and where and how we live').<br><br>")
    
    return(m)
  })
  
  
  
  
  output$plot_burden_jar_2 <- renderPlot({ 
    library("png")  
    genetic_z_score<-input$genetic_z_score
    environment_z_score<-input$environment_z_score
    
    #define genetics and environment proportions    
    max_total_score <- 4 #corresponding to sliders being from -2 Z-score to +2 Z-score
    min_total_score <- -4
    balls_start <- 0
    balls_end <- (genetic_z_score+2) / (max_total_score - min_total_score) + 0
    triangles_start <- balls_end
    triangles_end <- (environment_z_score+2) / (max_total_score - min_total_score) + triangles_start
    
    #define paths of images
    jar_path <-('/home/ubuntu/srv/impute-me/prsExplainer/jar.png')
    ball_path <-('/home/ubuntu/srv/impute-me/prsExplainer/yellowball.png')
    triangle_path_1 <-('/home/ubuntu/srv/impute-me/prsExplainer/orangetriangle1.png')
    triangle_path_2 <-('/home/ubuntu/srv/impute-me/prsExplainer/orangetriangle2.png')
    triangle_path_3 <-('/home/ubuntu/srv/impute-me/prsExplainer/orangetriangle3.png')
    
    #load images    
    jar = readPNG(jar_path, native=T) 
    ball = readPNG(ball_path, native=T) 
    triangle1 = readPNG(triangle_path_1, native=T)
    triangle2 = readPNG(triangle_path_2, native=T) 
    triangle3 = readPNG(triangle_path_3, native=T) 
    
    #get resolutions
    jar_res <- dim(jar) #409 209
    ball_res <- dim(ball)
    triangle_res1 <- dim(triangle1)
    triangle_res2 <- dim(triangle2)
    triangle_res3 <- dim(triangle3)
    
    
    
    #create a pack-maps 
    side_edge <- 45
    top_edge <- 130
    bottom_edge <- 50
    rows<-7
    columns <- 4
    sidewards<-seq(from=side_edge,to=jar_res[2]-side_edge,length.out=columns)
    upwards<-seq(from=bottom_edge,to=jar_res[1]-top_edge,length.out=rows)
    grid_points <- data.frame(x=rep(sidewards,times=rows),y=rep(upwards,each=columns))
    
    #add two by two points on top in the jar opening
    grid_points <- rbind(grid_points,data.frame(
      x=c(sidewards[2],sidewards[3],sidewards[3],sidewards[2]),
      y=c(upwards[rows]+38,upwards[rows]+38,upwards[rows]+76,upwards[rows]+76)
    ))
    
    
    #sort grid points first by Y-pos, then by mid position (so it fills from middle)
    grid_points<-grid_points[order(grid_points[,"y"],-abs(grid_points[,"x"]-jar_res[2]/2)),]
    
    
    #create some jitter  
    jitter <- 4
    grid_points[,"x"]<- grid_points[,"x"] + rnorm(nrow(grid_points),mean=0,sd=jitter)
    grid_points[,"y"]<- grid_points[,"y"] + rnorm(nrow(grid_points),mean=0,sd=jitter)
    
    
    
    #plot the jar
    plot(1,1,xlim=c(1,jar_res[2]),ylim=c(1,jar_res[1]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    rasterImage(jar,1,1,jar_res[2],jar_res[1])
    
    #plot the contents
    for(i in 1:nrow(grid_points)){
      #ball or triangle
      if(i / nrow(grid_points) > balls_start & i / nrow(grid_points) < balls_end){ #ball
        image <- ball   
        res <- ball_res
      }else if(i / nrow(grid_points) >= triangles_start & i / nrow(grid_points) < triangles_end){ #triangle
        j <- sample(1:3,1)
        image <- get(paste0("triangle",j))
        res <- get(paste0("triangle_res",j))
      }else{ #nothing
        next
      }
      
      rasterImage( image=image,
                   xleft=grid_points[i,"x"]-res[2]/2,
                   ybottom=grid_points[i,"y"]-res[1]/2,
                   xright=grid_points[i,"x"]+res[2]/2,
                   ytop=grid_points[i,"y"]+res[1]/2)  
      
    }
    
    
    #plot a legend
    x1 <- x2 <- -80
    y1 <- 130
    y2 <- 40
    x_offset <- 0
    y_offset <- 30
    
    text(x=x1,y=y1,label="Environment")
    rasterImage( image=triangle1,
                 xleft=x1+x_offset-triangle_res1[2]/4,
                 ybottom=y1+y_offset-triangle_res1[1]/4,
                 xright=x1+x_offset+triangle_res1[2]/4,
                 ytop=y1+y_offset+triangle_res1[1]/4)  
    
    
    text(x=x1,y=y2,label="Genes")
    rasterImage( image=ball,
                 xleft=x1+x_offset-ball_res[2]/4,
                 ybottom=y2+y_offset-ball_res[1]/4,
                 xright=x1+x_offset+ball_res[2]/4,
                 ytop=y2+y_offset+ball_res[1]/4)  
    
    
    
    
  })
  
  
  
  output$plot_full_stats <- renderPlot({ 
    genetic_z_score<-input$genetic_z_score
    
    trait_label <- input$trait_label
    control_label<-sub("[A-Z]+ Cases","Controls",trait_label)
    
    #subset to relevant trait
    d1<-data[data[,"group"]%in%c(trait_label,control_label),]
    d1[,"group"]<-factor(d1[,"group"],levels=c(control_label,trait_label))
    
    #create the ROC
    control_values <- d1[d1[,"group"] %in% control_label,"value"]
    case_values <- d1[d1[,"group"] %in% trait_label,"value"]
    roc<-data.frame(row.names=1:nrow(d1),cutoff=sort(d1[,"value"]))
    for(i in 1:nrow(roc)){
      roc[i,"tpr"] <- sum(control_values > roc[i,"cutoff"]) / length(control_values)
      roc[i,"fpr"] <- sum(case_values > roc[i,"cutoff"]) / length(case_values)
    }
    #Plot the ROC
    plot(
      x=roc[,"tpr"],
      y=roc[,"fpr"] ,
      type="l",
      xlab="False Positive Rate (1 - Specificity)",
      ylab="True Positive Rate (Sensitivity)")
    abline(a=0,b=1,lty=2)
    
    #Plot the currently selected genetic score cutoff
    if(sum(roc[,"cutoff"]<genetic_z_score)>0){
      x<-roc[sum(roc[,"cutoff"]<genetic_z_score),"tpr"]
      y<-roc[sum(roc[,"cutoff"]<genetic_z_score),"fpr"]
      points(x,y)
      text(x,y,label="Prediction\nat this score",srt=-45,adj=-0.2)
    }
    
    
    
    #Calculate prediction metrics at currently selected genetic score cutoff
    control_above<-which(d1[,"group"]%in%control_label & d1[,"value"] > genetic_z_score)
    case_above<-which(d1[,"group"]%in%trait_label & d1[,"value"] > genetic_z_score)
    control_below<-which(d1[,"group"]%in%control_label & d1[,"value"] <= genetic_z_score)
    case_below<-which(d1[,"group"]%in%trait_label & d1[,"value"] <= genetic_z_score)
    
    #Do the classical prediction metrics      
    ppv <- signif(length(case_above) / (length(case_above) + length(control_above)),2)
    npv <- signif(length(control_below) / (length(case_below) + length(control_below)),2)
    tpr<- signif((length(case_above) / (length(case_above) + length(case_below))),2)
    fpr <- signif((length(control_above) / (length(control_above) + length(control_below))),2)
    sensitivity <- tpr
    specificity <- 1 - fpr
    
    #plot them in the lower right of the figure
    text(x=0.6,y=0.5,
         label=paste0(
           "Sensitivity: ",sensitivity,"\n",
           "Specificity: ",specificity,"\n",
           "PPV: ",ppv,"\n",
           "NPV: ",npv,"\n",
           "TPR: ",tpr,"\n",
           "FPR: ",fpr,"\n"
                      ),
         adj=c(0,1))
    
    
  })
  
  
  output$text_full_stats <- renderText({  
    trait_label <- input$trait_label
    score_name <- prs_available[trait_label,"niceNames"]
    
    m<-paste0("This plot shows more advanced prediction statistics for the ",score_name," risk score. PPV = Positive Predictive Value, NPV = Negative Predictive Value, TPR = True Positive Rate (or sensitivity or recall), FPR = False Positive Rate (or fall-out or 1-specificity).")    
    return(m)    
  })
  
  
})





