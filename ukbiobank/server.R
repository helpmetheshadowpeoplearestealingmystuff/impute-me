library("shiny")

source("/home/ubuntu/srv/impute-me/functions.R")
dataFolder<-"/home/ubuntu/data/"
snps_file<-"/home/ubuntu/srv/impute-me/ukbiobank/2017-09-28_semi_curated_version_ukbiobank.rdata"
trait_file<-"/home/ubuntu/srv/impute-me/ukbiobank/2017-09-28_trait_overoverview.rdata"



ethnicities_labels<-c("Automatic guess","global","African","Ad Mixed American","East Asian","European","South Asian")
names(ethnicities_labels)<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")

#preload
load(snps_file)
load(trait_file)

shinyServer(function(input, output) {
  
  output$text_1 <- renderText({ 
    
    if(input$goButton == 0){
      m<-paste0("A polygenic genetic score is a value that gives a summary of a large number of different SNPs each of which contribute a little to disease risk. The higher the value, the higher the risk of developing disease. More details of its interpretation, calculation and limitations can be found in the <u><a href='https://ec2-54-218-116-201.us-west-2.compute.amazonaws.com/AllDiseases/'>complex disease module</a></u>, with the added caveat that only automated curation have been performed on these thousands of additional studies.<br><br>"
      )
      
    }else{
      m<-""
    }
    return(m)
  })
  
  
  
  get_data <- reactive({
    
    #initial UI data gathering and user-check
    study_id<-input[[paste0("trait_",input$trait_group)]]
    
    uniqueID<-gsub(" ","",input$uniqueID)
    ethnicity_group<-input$ethnicity_group
    # real_dist<-input$real_dist
    snp_p_value <- input$snp_p_value
    
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste(dataFolder,uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError(paste("Did not find a user with this id",uniqueID)))
    }
    
    
    #getting the relevant trait name, pmid and SNPs to analyze
    trait<-traits[study_id,"trait"]
    # pmid<-traits[study_id,"PMID"]
    # if(!pmid%in%data[,"PUBMEDID"])stop(paste("PMID",pmid,"was not found in system"))
    if(!trait%in%data[,"DISEASE.TRAIT"])stop(paste("trait",trait,"was not found"))
    SNPs_to_analyze<-data[data[,"study_id"]%in%study_id ,]
    
    #setting P-value treshold
    SNPs_to_analyze<-SNPs_to_analyze[SNPs_to_analyze[,"P.VALUE"] < 10^ -snp_p_value,]
    
    
    #setting up back-ground frequency sources
    #The default behavior is to try to guess ethnicity. If this fails it should revert to 'global' distribution but prepare to make a note of it in the methods.
    ethnicity_explanation_text <- "All scaling was done using the minor-allele frequency (MAF) for each SNP, as taken from the 1000 genomes project v3, using a _CHOICE_ frequency distribution."
    
    if(ethnicity_group == "automatic"){
      json_path<-paste0(dataFolder,uniqueID,"/",uniqueID,"_data.json")
      if(!file.exists(json_path))stop(safeError("Automatic guess of ethnicity not possible (json not found)"))
      library(jsonlite)
      d1<-fromJSON(json_path)
      e<-try(d1[["ethnicity"]][["guessed_super_pop"]],silent=F)
      if(is.null(e) || is.na(e) ||  !e %in% c("AFR", "AMR", "EAS", "EUR", "SAS")){
        ethnicity_explanation_text<-paste0(ethnicity_explanation_text," This was done because we could not automatically guess your ethnicity, but you can use the advanced tab to set it yourself.")
        ethnicity_group<-"global"
      }else{
        ethnicity_explanation_text<-paste0(ethnicity_explanation_text," This was done based on an automated guess.")
        ethnicity_group <- e
      }
    }
    if(ethnicity_group == "global"){
      #do nothing. Note the density curve location.
      # densityCurvePath<-"/home/ubuntu/srv/impute-me/ukbiobank/2017-07-01_densities_ALL.rdata"
    }else{
      #then replace the MAF with the correct superpopulation group
      SNPs_to_analyze[,"minor_allele_freq"] <- SNPs_to_analyze[,paste0(ethnicity_group,"_AF")]
      #note the density curve location
      # densityCurvePath<-paste0("/home/ubuntu/srv/impute-me/ukbiobank/2017-07-01_densities_",ethnicity_group,".rdata")
    }
    #then explain which choice was made
    ethnicity_explanation_text <- sub("_CHOICE_",ethnicities_labels[ethnicity_group],ethnicity_explanation_text)
    
    
    
    #gathering some background info for the study		
    sampleSize_case<-unique(SNPs_to_analyze[,"case_count"])
    sampleSize_control<-unique(SNPs_to_analyze[,"control_count"])
    textToReturn <- paste0("Retrieved ",nrow(SNPs_to_analyze)," SNPs from the <u><a href='http://www.ukbiobank.ac.uk/'>UK biobank</a></u>, which were reported to be associated with the trait <i>'",trait,"'</i> (field code: ",sub("_ukbiobank$","",study_id),").")
    textToReturn <- paste0(textToReturn," The summary statistics were calculated by <u><a href='http://www.nealelab.is/blog/2017/7/19/rapid-gwas-of-thousands-of-phenotypes-for-337000-samples-in-the-uk-biobank'>Neale lab</a></u> and reports a total sample size of ",sampleSize_case," cases and ", sampleSize_control," controls as downloaded on 2017-09-15.")
    
    
    
    #if any of the SNPs are duplicated we have to merge them (this by the way is an odd situation
    #why would a SNP be listed twice in the results for the same trait - let's aim to merge only in GRS
    #but list all in table for the sake of transparency)
    if(any(duplicated(SNPs_to_analyze[,"SNP"]))){
      #Handle the duplications and make a proper warning
      duplicates<-sort(unique(SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"SNP"]))
      warnForDiscrepancyInBeta<-FALSE
      for(duplicate in duplicates){
        s1<-SNPs_to_analyze[SNPs_to_analyze[,"SNP"]%in%duplicate,]
        if(
          length(unique(s1[,"effect_allele"]))!=1|
          length(unique(s1[,"non_effect_allele"]))!=1|
          length(unique(s1[,"effect_size"]))!=1){
          warnForDiscrepancyInBeta<-TRUE
        }
      }
      duplicates_example<-paste(duplicates[1:min(c(5,length(duplicates)))],collapse=", ")			
      if(warnForDiscrepancyInBeta){
        textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNP(s) were entered twice for this GWAS, and the effect-size and direction was <b>not consistent</b>. The beta from the first listed SNP was used, but please cross-check the results table with the original study carefully for details (e.g. ",duplicates_example,"). Duplicates are indicated in the end of table but don't contribute to results.")
      }else{
        textToReturn <- paste0(textToReturn," Note ",length(duplicates)," SNP(s) were entered twice for this GWAS, but the effect-size and direction was consistent (e.g. ",duplicates_example,"). Duplicates are indicated in the end of table but don't contribute to results.")
      }
      
      #then we handle it so that the extra lines are removed and inserted in the end instead
      SNPs_to_analyze_duplicates<-SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),]
      rownames(SNPs_to_analyze_duplicates) <- paste0("duplicate_",1:nrow(SNPs_to_analyze_duplicates))
      for(col in c("genotype","personal_score","population_score_average","population_score_sd","score_diff")){
        SNPs_to_analyze_duplicates[,col]<-NA
      }
      SNPs_to_analyze<-SNPs_to_analyze[!duplicated(SNPs_to_analyze[,"SNP"]),]
    }else{
      SNPs_to_analyze_duplicates<-SNPs_to_analyze[0,]
    }
    rownames(SNPs_to_analyze)<-SNPs_to_analyze[,"SNP"]
    genotypes<-get_genotypes(uniqueID=uniqueID,request=SNPs_to_analyze, namingLabel="cached.all_gwas")
    SNPs_to_analyze[,"genotype"] <- genotypes[rownames(SNPs_to_analyze),"genotype"]
    SNPs_to_analyze <-get_GRS_2(SNPs_to_analyze,mean_scale=T, unit_variance=T)
    population_sum_sd<-sqrt(sum(SNPs_to_analyze[,"population_score_sd"]^2,na.rm=T))
    GRS <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T) / population_sum_sd
    

    
    #Note for non-available SNPs (seems more important in the ukbiobank setup)
    non_measured <- sum(is.na(SNPs_to_analyze[,"genotype"]))
    if(non_measured>0){
      
      #if more than half are missing we bold the warning
      if(non_measured/2 > nrow(SNPs_to_analyze)){
        textToReturn <- paste0(textToReturn," <b>Note that ",non_measured," SNPs were not available</b> in your genotype data at all, because the current imputation algorithm was not able to guess them. This will reduce the accuracy of the risk score a lot.")  
      }else{
        textToReturn <- paste0(textToReturn," Note that ",non_measured," SNPs were not available in your genotype data at all, because the current imputation algorithm was not able to guess them.")  
      }
    }
    
    
    #inset a warning if less than 5 SNPs are analyzable
    analyzable_snps<-sum(!is.na(SNPs_to_analyze[,"score_diff"]))
    if(analyzable_snps==0){
      stop(safeError("No SNPs were analyzable for this trait"))
    }else if(analyzable_snps==1){
      textToReturn <- paste0(textToReturn," Overall, <b>only a single SNP was analyzable. This is definetly too little for a genetic risk score and the results should not be trusted</b>.")  
    }else if(analyzable_snps<5){
      textToReturn <- paste0(textToReturn," Overall, <b>only ",analyzable_snps," SNPs were analyzable. This is too little for a genetic risk score and the results should not be trusted very much</b>.")  
    }
    
    
    #check for question marks in risk-allele
    c1<-apply(SNPs_to_analyze[,c("minor_allele","major_allele","effect_allele","non_effect_allele")]=="?",1,sum)
    if(sum(c1>0)){
      textToReturn <- paste0(textToReturn," Also note that ",sum(c1>0)," SNP(s) had missing or discrepant allele information, meaning that risk-allele or minor/major allele could not be correctly assigned. This is indicated with a '?' in the results table and causes the SNP to be omitted from the GRS-calculation.")
    }
    

    
        
    
    #add the overall population SD value
    textToReturn <- paste0(textToReturn," For you, we calculated an ethnicity-corrected trait Z-score of ",signif(GRS,2),".")
    
    
    
    #add the final summary
    percentage<-floor(pnorm(GRS,mean=0,sd=1)*100)
    if(percentage < 20){
      summary <- " This is a low score."
    }else if(percentage > 90){
      summary <- " This is a high score. But keep in mind that additional calculation is necessary to determine a real life-time risk. For example having a very high genetic score for something that is not very heritable may make very little difference. These additional calculations typically require further studies, not always available."
    }else{
      summary <- " This is a fairly average score."
    }
    textToReturn <- paste0(textToReturn," This means that your genetic risk score for this trait will be <b>higher than ",percentage,"% of the general population</b>.",summary)
    

    #get some misc trait information
    ukbiobank_notes<-traits[study_id,"Notes"]
    # phesant_notes<-traits[study_id,"PHESANT.notes" ]
    
    
    #write the methods text
    methodsToReturn<-paste0("<small><br><b>Methods</b><br>GWAS data was obtained from the <u><a href='http://www.ukbiobank.ac.uk/'>UK biobank</a></u> (with summary stats calculated by <u><a href='http://www.nealelab.is/blog/2017/7/19/rapid-gwas-of-thousands-of-phenotypes-for-337000-samples-in-the-uk-biobank'>Neale lab</a></u>). Data was downloaded and filtered by proximity and P-values (according to slide-scale in advanced settings). Then a per-SNP score was calculated by counting the risk-alleles multiplied by the effect size (OR or beta as reported in original paper). This was centered so that the average score in the general population would be zero ('population normalized'). This means, that if a person is homozygote for a very rare risk variant this will result in a very high Z-score, conversely if the SNP is common, the Z-score will be less extreme. The sum of these normalized SNP-scores are calculated to get a trait-wide genetic risk score (GRS). This GRS was additionally scaled so that standard-deviation in the general population is 1 (unit-variance), effectively making the scores <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-scores</a></u>. ", ethnicity_explanation_text, " Further details of the calculation can be found in the <u><a href='https://github.com/lassefolkersen/impute-me/blob/56813bf071d7fa4c0a658c90d2ebee196a781e8a/functions.R#L1166-L1326'>source code</a></u>. 
                            
                            <br><br>The advantage of this approach is that it does not require further data input than MAF, effect-size and genotype.  This makes the calculation fairly easy to implement. To perform a double check of this theoretical distribution, switch on the 'plot real distribution' option in the advanced options sections (Only implemented for GWAS calculator, not UK-biobank. yet).
                            
                      <br><br>Using UK-biobank compared to the GWAS calculator data has the advantage of being a highly systematic and (often) more well-powered approach. The trade-off is that less per-trait curation has been applied at the GWAS calculation step and that results are <i>not</i> peer-reviewed. The UK-biobank trait-note for this trait was: <i>",ukbiobank_notes,"</i>. Further, keep in mind that the UK-biobank consists of individuals of European ethnicity. Even though the normalization scheme can be changed here, it still may not always translate well. For further information on how such may translate between ethnicities, refer to e.g. the work of <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=24390342'>Okada et al</a></u> for a comparison of Asian and European ethnicity (extended data figure 3).
                           
                            </small>")
    
    
    
    #add in the (damn) duplicates
    SNPs_to_analyze<-rbind(SNPs_to_analyze,SNPs_to_analyze_duplicates)
    
    
    #if asked for (experimental) then get the distribution
    # if(real_dist){
    # load(densityCurvePath)
    # if(!paste0(study_id,"_y") %in% rownames(densities))stop(safeError(paste("This",study_id,"trait was not found to have density-plotting available")))
    # 
    # distributionCurve<-list(x=densities[paste0(study_id,"_x"),],y=densities[paste0(study_id,"_y"),])
    # }else{
    # distributionCurve<-NULL
    # }
    
    
    
    #write the score to the log file
    log_function<-function(uniqueID,study_id,genotypes){
      user_log_file<-paste("/home/ubuntu/data/",uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"ukbiobank",uniqueID,study_id,GRS,ethnicity_group)
      m<-paste(m,collapse="\t")
      if(file.exists(user_log_file)){
        write(m,file=user_log_file,append=TRUE)
      }else{
        write(m,file=user_log_file,append=FALSE)
      }
    }
    try(log_function(uniqueID,study_id,genotypes))
    
    #then return the overall list		
    return(list(
      SNPs_to_analyze=SNPs_to_analyze,
      textToReturn=textToReturn,
      methodsToReturn=methodsToReturn,
      GRS=GRS))#,
      # distributionCurve=distributionCurve))
  })
  
  
  
  
  output$plot_1 <- renderPlot({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      o<-get_data()
      SNPs_to_analyze<-o[["SNPs_to_analyze"]]
      GRS_beta<-o[["GRS"]]
      # distributionCurve <- o[["distributionCurve"]]
      
      if(is.na(GRS_beta))stop("Could not calculate overall GRS because all SNPs in the signature were missing information about either risk-allele, effect-size or minor-allele-frequency.")
      
      control_mean<-0
      control_sd<-1
      xlim<-c(control_mean - control_sd*3, control_mean + control_sd*3)
      
      x<-seq(xlim[1],xlim[2],length.out=100)
      y_control<-dnorm(x,mean=control_mean,sd=control_sd)
      plot(x,y_control,type="l",col="blue",ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n",lwd=2)
      
      
      #fill the controls
      if(all(!x<GRS_beta))stop(safeError("GRS too low to plot"))
      max_GRS_i<-max(which(x<GRS_beta))
      upper_x<-x[1:max_GRS_i]
      upper_y<-y_control[1:max_GRS_i]
      x_lines <- c(upper_x,GRS_beta,GRS_beta,xlim[1])
      y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
      polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
      
      #add control text
      # prop<-signif(pnorm(GRS_beta,mean=control_mean,sd=control_sd),2)
      # x_text<-upper_x[round(length(upper_x)/2)]
      # y_text<-upper_y[round(length(upper_y)/2)] / 2
      # text(x_text,y_text,paste0(prop*100,"%"),col="blue")
      
      #draw the main line
      abline(v=GRS_beta,lwd=3)
      
      
      #optionally add real distribution curve
      # if(!is.null(distributionCurve)){
      #   real_x <- distributionCurve[["x"]]
      #   real_y <- distributionCurve[["y"]]
      #   adj_y<-real_y * (max(y_control) / max(real_y))
      #   lines(x=real_x,y=adj_y,lty=2)
      #   
      # }
      
      legend("topleft",legend=c("Population distribution","Your genetic risk score"),lty=c(1,1),lwd=c(2,3),col=c("blue","black"))
      
    }		
  })
  
  
  output$text_2 <- renderText({ 
    if(input$goButton == 0){
      return("")
    }else if(input$goButton > 0) {
      o<-get_data()
      m<-paste0("<br><br>",o[["textToReturn"]],"<br><br>")
      
    }
    return(m)
  })
  
  
  output$table1 <- renderDataTable({ 
    if(input$goButton == 0){
      return("")
    }else if(input$goButton > 0) {
      
      #getting data
      o<-get_data()
      SNPs_to_analyze<-o[["SNPs_to_analyze"]]
      GRS<-o[["GRS"]]
      
      
      #summarising allele info into single-columns
      SNPs_to_analyze[,"Effect/non-effect Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
      SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
      
      # col_to_remove<-c("DATE","sampleSize","ensembl_alleles","LINK","FIRST.AUTHOR","PUBMEDID","chr_name","CHR_POS","effect_allele","non_effect_allele","major_allele","minor_allele","DISEASE.TRAIT")
      # for(col in col_to_remove){SNPs_to_analyze[,col]<-NULL}
      
      
      
      
      #rounding MAF
      SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
      
      #removing duplicate GRS
      # SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"GRS"]<-""
      
      #shortening the reported gene count
      # SNPs_to_analyze[,"Reported Gene"]<-sapply(strsplit(SNPs_to_analyze[,"REPORTED.GENE.S."],", "),function(x){
        # paste(x[1:min(c(2,length(x)))],collapse=", ")
      # })
      
      
      #marking duplicates
      for(col in c("genotype","personal_score","score_diff")){
        SNPs_to_analyze[is.na(SNPs_to_analyze[,col]),col] <- ""
      }
      
      
      #round P.VALUE and effect size
      SNPs_to_analyze[,"effect_size"] <- signif(SNPs_to_analyze[,"effect_size"],2)
      SNPs_to_analyze[,"P.VALUE"] <- signif(SNPs_to_analyze[,"P.VALUE"],2)
      
      keep<-c("SNP","genotype","Effect/non-effect Allele","personal_score","score_diff"
              ,"effect_size","P.VALUE","Major/minor Allele","minor_allele_freq") 
      SNPs_to_analyze<-SNPs_to_analyze[,keep]
      colnames(SNPs_to_analyze)<-c("SNP","Your Genotype","Effect/ non-effect Allele","SNP-score","SNP-score (population normalized)","Effect Size","P-value","Major/ minor Allele","Minor Allele Frequency")
      
      return(SNPs_to_analyze)
    }
  },options = list(searching = FALSE,paging = FALSE))
  
  
  output$text_3 <- renderText({ 
    
    if(input$goButton == 0){
      methodsToReturn<-""
      
      
    }else{
      o<-get_data()
      methodsToReturn<-o[["methodsToReturn"]]
    }
    return(methodsToReturn)
  })
  
  
})


