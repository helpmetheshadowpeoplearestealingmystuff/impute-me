library("shiny")
library("jsonlite")

#load functions and define paths of reference files and data directory

snps_file<-paste0(get_conf("code_path"),"AllDiseases/2021-01-28_snp_weights.rdata")
trait_file<-paste0(get_conf("code_path"),"AllDiseases/2021-01-28_trait_overview.xlsx")
all_snp_trait_file <- paste0(get_conf("code_path"),"prs/2021-02-11_study_list.xlsx")



#defining 1000 genomes populations
ethnicities_labels<-c("Automatic guess","global","African","Ad Mixed American","East Asian","European","South Asian")
names(ethnicities_labels)<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")



#preload data
library(openxlsx)
load(snps_file)
traits <- read.xlsx(trait_file,rowNames=T)
all_snp_traits<-read.xlsx(all_snp_trait_file,rowNames=T)


#Start main Shiny scripts
shinyServer(function(input, output) {
  
  
  #The top text shown before any analysis is run  
  output$text_1 <- renderText({ 
    if(input$goButton == 0){
      m<-paste0("A polygenic risk score is a value that gives a summary of a large number of different SNPs - each of which contribute a little to disease risk. The higher the value, the higher the risk of developing disease. Of course the interpretation of this risk depends a lot on other factors as well: How heritable the disease is. How much of this heritability we can explain with known SNPs. And not least, what would the risk of disease be for you otherwise, i.e. without taking the genetic component into account. <br><br>Because the polygenic risk score is only a risk-modifier, knowledge of these three other values are all required if you want to know what your overall risk is, i.e. what's the chance in percent. This calculator cannot provide that. But it can provide a view of the <i>known genetic</i> component of your disease risk, based on all the SNPs that we know are associated with the disease. This, we believe, makes it a better choice for complex diseases than the typical one-SNP-at-the time analysis typically seen in consumer genetics.<br><br>"
      )
    }else{
      m<-""
    }
    return(m)
  })
  
  
  #The main data gathereing function, defined as reactive because it's used in several different calls
  get_data <- reactive({
    
    #initial UI data gathering 
    if(input$only_show_newest){
      ui_selector <- paste0("trait_",input$trait_group,"_newest")  
    }else{
      ui_selector <- paste0("trait_",input$trait_group)
    }
    study_id<-input[[ui_selector]]
    uniqueID<-gsub(" ","",input$uniqueID)
    ethnicity_group<-input$ethnicity_group
    use_all_snp_score <- input$use_all_snp_score
    plot_heritability <- input$plot_heritability
    real_dist<-input$real_dist
    

    
    #If trait is not available in all_snp_traits we override the use_all_snp_score and set to FALSE
    #(this would be the most frequent case actually. For now)
    if(!study_id %in% rownames(all_snp_traits)){
      use_all_snp_score <- FALSE
    }else{
      if(is.na(all_snp_traits[study_id,"file_to_read"])){ #also revert it to FALSE if there's no actual score-file
        use_all_snp_score <- FALSE
      }
    }
    
    
    #If use_all_snp_score is TRUE real_dist must be TRUE as well, so override that
    if(use_all_snp_score){
      real_dist<-TRUE
    }
    
    
    
    #user check
    if(nchar(uniqueID)!=12)stop(safeError("uniqueID must have 12 digits"))
    if(length(grep("^id_",uniqueID))==0)stop(safeError("uniqueID must start with 'id_'"))
    if(!file.exists(paste(get_conf("data_path"),uniqueID,sep=""))){
      Sys.sleep(3) #wait a little to prevent raw-force fishing	
      stop(safeError(paste("Did not find a user with this id",uniqueID)))
    }
    
    
    
    
    #getting the relevant trait name, pmid and SNPs to analyze
    trait<-traits[study_id,"trait"]
    pmid<-traits[study_id,"pmid"]
    if(!pmid%in%data[,"pmid"])stop(paste("PMID",pmid,"was not found in system"))
    SNPs_to_analyze<-data[data[,"study_id"]%in%study_id ,]


    #Reading in the json file     
    json_path<-paste0(get_conf("data_path"),uniqueID,"/",uniqueID,"_data.json")
    if(!file.exists(json_path))stop(safeError("Missing essential files for this uniqueID (So cannot do automatic reference population guess)"))
    d1<-fromJSON(json_path)
    
    
    #setting up back-ground frequency sources
    #The default behavior is to try to guess ethnicity. If this fails it should revert to 'global' distribution but prepare to make a note of it in the methods.
    if(ethnicity_group == "automatic"){
      e<-try(d1[["ethnicity"]][["guessed_super_pop"]],silent=F)
      if(is.null(e) || is.na(e) ||  !e %in% c("AFR", "AMR", "EAS", "EUR", "SAS")){
        ethnicity_group<-"global"
      }else{
        ethnicity_group <- e
      }
    }
    
    #select correct densityCurvePath
    if(ethnicity_group == "global"){
      if(use_all_snp_score){
        densityCurvePath<-paste0(get_conf("code_path"),"prs/2021-02-11_densities_ALL.rdata")
      }else{
        densityCurvePath<-paste0(get_conf("code_path"),"AllDiseases/2021-02-12_densities_ALL.rdata")
      }
    }else{
      if(use_all_snp_score){
        densityCurvePath<-paste0(get_conf("code_path"),"prs/2021-02-11_densities_",ethnicity_group,".rdata")
      }else{
        densityCurvePath<-paste0(get_conf("code_path"),"AllDiseases/2021-02-12_densities_",ethnicity_group,".rdata")  
      }
      
      #Also,replace the MAF with the correct superpopulation group
      SNPs_to_analyze[,"minor_allele_freq"] <- SNPs_to_analyze[,paste0(ethnicity_group,"_AF")]
    }
    
    #only actually load the distributions if asked for them
    if(real_dist ){
      load(densityCurvePath)
      if(use_all_snp_score){ #should standardize this naming later
        x_name<-paste0(all_snp_traits[study_id,"file_to_read"],"_x")
        y_name<-paste0(all_snp_traits[study_id,"file_to_read"],"_y")
      }else{
        x_name<-paste0(study_id,"_x")
        y_name<-paste0(study_id,"_y")
      }
      if(!all(c(x_name,y_name)%in% rownames(densities)))stop(safeError(paste("This",study_id,"trait was not found to have density-plotting available")))
      distributionCurve<-list(x=densities[x_name,],y=densities[y_name,])
    }else{
      distributionCurve<-NULL
    }
    
    
    
    
    
    #get SNP count
    if(!use_all_snp_score){
      snp_count<-nrow(SNPs_to_analyze)
    }else{
      snp_count<-all_snp_traits[study_id,"variant_count"]
    }
    
    

    #gathering some background info for the study		
    link<-paste0("www.ncbi.nlm.nih.gov/pubmed/",traits[study_id,"pmid"])
    author<-traits[study_id,"first_author"]
    sampleSize<-traits[study_id,"sampleSize"]
    publication_date<-traits[study_id,"publication_date"]
    textToReturn <- paste0("Retrieved ",snp_count," SNPs from <u><a target='_blank' href='http://",link,"'>",author," et al (PMID ",pmid,")</a></u>, which were reported to be associated with ",tolower(trait),".")
    textToReturn <- paste0(textToReturn," This study reports a total sample size of ",sampleSize,", as entered on date ",publication_date,".")
    
    #get PGS_name (if applicable)
    if(use_all_snp_score){
      if(!is.na(all_snp_traits[study_id,"PGS_id"])){
        PGS<-all_snp_traits[study_id,"PGS_id"]
        PGS_source<-paste0(" It was implemented from the <u><a href='http://www.pgscatalog.org'>PGS-catalog</a></u> ID ",PGS,".")
      }else{
        PGS_source<-""
      }
    }
    
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
    
    
    #calculate the polygenic risk score using get_GRS_2 (since the first step is tabulated and shown, it's done regardless whether we use_all_snp_score)
    SNPs_to_analyze <-get_GRS_2(SNPs_to_analyze,mean_scale=T, unit_variance=T)
    if(!use_all_snp_score){
      population_sum_sd<-sqrt(sum(SNPs_to_analyze[,"population_score_sd"]^2,na.rm=T))
      if(population_sum_sd == 0)stop(safeError("For some reason we couldn't analyse this particular trait from your genomic data."))
      GRS <-sum(SNPs_to_analyze[,"score_diff"],na.rm=T) / population_sum_sd
    }

    
    #extract the polygenic risk score from json (if using use_all_snp_score)
    if(use_all_snp_score){
      if(!study_id %in% rownames(all_snp_traits))stop(safeError("All SNP trait data not available for this study. Try to switch off the all-SNP scoring in advanced options."))
      file_to_read <- all_snp_traits[study_id,"file_to_read"]
      #check and load prs
      if(!"prs"%in%names(d1))stop(safeError("No all-SNP scores were available for this sample. It was probably uploaded before implementation or the all-SNP PRS files where not available. Try switching off 'show all-SNP scores' in advanced options."))
      d2<-d1[["prs"]]
      if(!file_to_read%in%names(d2))stop(safeError("No all-SNP scores were available for this study for this sample. It was probably uploaded before implementation or the all-SNP PRS files where not available."))
      d3<-d2[[file_to_read]]
      if(!all(c("SCORESUM_PLINK_2_0_DOSAGE_MATRIX","PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT")%in%names(d3)))stop(safeError("The correct all-SNP scores were not available for this study for this sample. It was probably uploaded before implementation."))
      GRS <- signif(d3[["SCORESUM_PLINK_2_0_DOSAGE_MATRIX"]] * d3[["PLINK_2_0_DOSAGE_MATRIX_NMISS_ALLELE_CT"]]  ,3)
    }
    
    
    #check for question marks in risk-allele
    if(!use_all_snp_score){
      c1<-apply(SNPs_to_analyze[,c("minor_allele","major_allele","effect_allele","non_effect_allele")]=="?",1,sum)
      if(sum(c1>0) & !use_all_snp_score){
        if(sum(c1>0)==1){snp_or_snps<-"SNP"}else{snp_or_snps<-"SNPs"}
        textToReturn <- paste0(textToReturn," Also note that ",sum(c1>0)," ",snp_or_snps," had missing or discrepant allele information, meaning that risk-allele or minor/major allele could not be correctly assigned. This is indicated with a '?' in the results table and causes the SNP to be omitted from the calculation. This is likely due to strand-reporting issues, and may be fixable by checking the original study.")
      }
    }
    
    
    
    
    #Scale distribution if they are requested as use_all_snp_score
    if(use_all_snp_score){
      #scale to zero-mean and unit-variance
      x <-distributionCurve[["x"]]
      y <- distributionCurve[["y"]]
      mean <- x[order(y,decreasing=T)[1]]
      proportion <- cumsum(y[order(y,decreasing=T)]) / sum(y)
      one_sd_range_index<-range(as.numeric(names(proportion[proportion < 0.6827])))
      sd <- mean(abs(x[one_sd_range_index] - mean))
      distributionCurve[["x"]] <- (distributionCurve[["x"]] - mean) / sd
      GRS <- (GRS - mean) / sd
    }
    
    
    
    #Add that table is just an example if it is an use_all_snp_score case
    if(use_all_snp_score){
      textToReturn <- paste0(textToReturn," Note that the table below shows the most significant SNPs only as a calculation example - the score is based on more than what is shown.")
    }
    
    
    #add that breast cancer PRS is no substitute for a BRCA sequencing
    if(length(grep("breast_cancer_",study_id))>0){
      textToReturn<-paste0(textToReturn," Note that this breast cancer PRS <b>is no substitute for a BRCA-gene sequencing</b>. It only includes info from common variation, and in breast cancer there are particularly strong nonsense-mutations in the BRCA-genes (etc) that have high impact on risk, but are not detectable with microarray data.")
      
    }
    
    #add that COVID19-severity PRS is no substitute for a mask
    if(length(grep("covid19hg",study_id))>0){
      textToReturn<-paste0(textToReturn," Note that this genetic covid-19 severity score <b>is no substitute for</b> careful behaviour. The best way to avoid severe Covid-19 disease is to avoid getting infected with Sars-cov-2 by e.g. keeping distance.")
      textToReturn<-sub("http://www.ncbi.nlm.nih.gov/pubmed/00000000","https://www.covid19hg.org/results/r5/",textToReturn)
      textToReturn<-sub("PMID 00000000","freeze 5, A2 ALL",textToReturn)
    }
    
    
    
    
    #add the overall population SD value
    if(!use_all_snp_score | use_all_snp_score){
      textToReturn <- paste0(textToReturn,"<br><br>For you, we calculated an ancestry-corrected trait Z-score of ",signif(GRS,2),".")  
    }
    
    
    #add the final summary 
    percentage<-floor(pnorm(GRS,mean=0,sd=1)*100)
    if(percentage < 20){
      summary <- " This is a lower score than the average person."
    }else if(percentage > 90){
      summary <- " This is a higher score than the average person. <br><br>But keep in mind that additional calculation is necessary to determine a real life-time risk. For example having a very high genetic score for something that is not very heritable may make very little difference. These additional calculations typically require further studies, not always available. If you would like someone to discuss these findings with we recommend <u><a href='https://www.greygenetics.com/'>greygenetics.com</a></u>."
      summary <- paste0(summary,"<br><br><div style='background-color: #cfc ; padding: 10px; border: 1px solid green;'><p>PLEASE NOTE:<br>The information provided in this module:<br>1) CANNOT definitively tell you whether or not you currently have - or whether you will develop - any of the traits for which information is provided.<br>2) is about RISK for the conditions listed. There are genetic variations that may contribute to risk that are NOT included in the scores provided. Remember that behavior, experience, and lifestyle factors may contribute to your risk, and these are not included in the scores provided.<br></div>")
    }else{
      summary <- " This is a fairly average score."
    }
    
    
    
    #return a read-out
    if(percentage > 50){
      textToReturn <- paste0(textToReturn," This means that your genetic risk score for <b>this trait is higher than ",percentage,"% of and lower than ",100-percentage,"% of the general population</b>.",summary)
    }else{
      textToReturn <- paste0(textToReturn," This means that your genetic risk score for <b>this trait is lower than ",100-percentage,"% of and higher than ",percentage,"% of the general population</b>.",summary)
    }

    
    
    #write the methods text for GWAS-significant hits
    if(!use_all_snp_score){
      methodsToReturn<-paste0("<small><br><b>Methods</b><br>The polygenic risk score is calculated by combining your genotype data with trait association data from ",author," et al (PMID ",pmid,"). This is done by counting how many risk-alleles you have for each SNP (column <i>'Your genotype'</i>) and multiplying that count by the reported effect-size of the SNP (column <i>'Effect Size'</i>). This gives a SNP-score for each row (column <i>'SNP-score'</i>). The SNP-score is then centered so that the <i>average</i> score in the general population would be zero (column <i>'SNP-score population normalized'</i>). All of these population normalized values are then summarized to get an overall score. This sum is further scaled so that its standard-deviation in the general population would be equal to 1 ('unit-variance'). This effectively makes it a <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-score</a></u>. The scaling and centering is based on the minor-allele frequencies (MAF) taken from the 1000 genomes project, using the ",ethnicities_labels[ethnicity_group]," frequency distribution. This gives an ethnicity-specific standard deviation of ",signif(population_sum_sd,2),". If you summarize the population normalized SNP-score column and divide by this number, you too will obtain the reported Z-score of ",signif(GRS,2),", illustrating how your score is a combination of many SNPs working together. Further details of all calculations can be found in the <u><a href='https://github.com/lassefolkersen/impute-me/blob/03c51c63b262f600d509469e361db35bd2a8a5fb/functions.R#L1295-L1455'>source code</a></u>. 

                            <br><br>The advantage of this approach is that it only requires a list of GWAS-significant SNPs, their frequency, effect-size and effect-alleles.  This makes it possible to implement the calculation systematically for many diseases and traits. 
                            
                            <br><br>One weakness in this approach is that it assumes that individuals from the 1000 genomes project are a reasonably normal reference group. For some traits or diseases this may not be true. As an alternative, you can select the <i>'plot user distribution'</i> option in the advanced options sections. This will overlay the plot with distribution of all ethnicity-matched impute.me users. The weakness of that approach, however, is the assumption that most users of impute.me are reasonably normal. Another potential issue is that in some cases the term genetic <i>risk</i> score may be unclear. For example in the case of GWAS of biological quantities where it is not clear if higher values are <i>more</i> or <i>less</i> risk-related, e.g. HDL-cholesterol or vitamin-levels. In most cases, higher score means high level - but it is recommended to consult with the original GWAS publication if there is any doubt. Thirdly, it is important to note that many of these scores only explain very small proportions of the overall risk. How much is illustrated in the blue pie chart below the bell curve. The more dark blue, the more predictive the score is. In the <u><a href='https://www.impute.me/prsExplainer'>PRS-explanatory module</a></u> you can further explore what this predivtiveness means in a sandbox setting.
                            
                            <br><br>Finally, instead of scrolling through all the alphabetical entries here, then check out the <u><a href='https://www.impute.me/diseaseNetwork/'>Precision-medicine module</a></u>. The data in that module is based on the calculations made here, but the information is instead given as a view of scores relevant only to a specific disease-scope. The intention is to give relevant information for a given context, while avoiding risk-sorted lists bound to produce spurious and wrongful observations (see <u><a href='https://github.com/lassefolkersen/impute-me/issues/8'>discussion</a></u> here).</small>")		
    }
    
    #write the methods text for all-SNP scores
    if(use_all_snp_score){
      methodsToReturn<-paste0("<small><br><b>Methods</b><br>The all-SNP polygenic risk score is calculated by combining your genotype data with complete trait association data from <u><a href='",link,"'>",author," et al</a></u>.",PGS_source," This is done by counting how many risk-alleles you have for each SNP (column <i>'Your genotype'</i>) and multiplying that count by the reported effect-size of the SNP (column <i>'Effect Size'</i>). This gives a SNP-score for each row (column <i>'SNP-score'</i>). Note that the table only shows the most significant SNPs as an example, even though a total of ",snp_count," SNPs are used in the calculation. This is done according to the <u><a href='https://www.cog-genomics.org/plink2'>plink</a></u> <i>score</i> method. For missing SNPs, the average frequency for ",ethnicities_labels[ethnicity_group]," ethnicity is used, based on data from the 1000 genomes project.  Further details of all calculations can be found in the <u><a href='https://github.com/lassefolkersen/impute-me/blob/03c51c63b262f600d509469e361db35bd2a8a5fb/prs/export_script.R'>source code</a></u>. 

                              <br><br>The all-SNP polygenic risk score works as an add-on to each existing score at impute.me. You can switch it off by un-selecting <i>'Show all-SNP score'</i> in advanced options. The main advantage of all-SNP scores is that they explain more of the risk variation than the default score types that are based only on GWAS-significant 'top' SNPs. You can see the difference as how much dark-blue there is in the pie-chart of variability explained. The difference can also be further explored in the sandbox <u><a href='https://www.impute.me/prsExplainer'>PRS-explanatory module</a></u>.

                              <br><br>A main disadvantage of the all-SNP score is that it is difficult to implement it systematically for many diseases and traits, which is the reason it is only available for selected studies. This is likely to change in the future as more and more studies release their full summary-stats without access-conditions. Remaining disadvantages mainly relate to how we implement the calculations. For example, the current implementation only have the option to compare to previous ethnicity-matched users of impute.me. This is something we are working actively on.</small>")		
    }
    
    
    #add in the (damn) duplicates
    SNPs_to_analyze<-rbind(SNPs_to_analyze,SNPs_to_analyze_duplicates)
    
    
    
    #special for sanogenetics collaboration - decide if they are shown message
    #or not. Logic: always show on matching genotype, otherwise randomly show
    #at a low(er) percentage, to make sure the banner doesn't reveal
    #deterministic genotype information to anyone who doesn't want to know.
    show_sanogenetics_banner <- FALSE
    snp1 <- "rs17580" 
    snp2 <- "rs28929474"
    cheapconstantchecksum<-sum(as.numeric(unlist(strsplit(gsub("[a-zA-Z]","",sub("^id_","",uniqueID)),"")))) #keep choice constant for one uniqueID
    if(uniqueID=="id_613z86871"){
      show_sanogenetics_banner <- FALSE
    }else if(cheapconstantchecksum==28){
      show_sanogenetics_banner <- TRUE
    }else{
      r1<-try(d1[["rareDiseases"]][["all_findings"]])
      if(class(r1)!="try-error" && !is.null(r1)){
        if(
          r1[r1[,"SNP"] %in% snp1,"Your genotype"] %in% c("A/A", "T/A") |
          r1[r1[,"SNP"] %in% snp2,"Your genotype"] %in% c("C/T","T/T")
        ){
          show_sanogenetics_banner<-TRUE
        }
      }
    }
  
    
        
    
    #write the score to the log file
    log_function<-function(uniqueID,study_id,genotypes){
      user_log_file<-paste(get_conf("data_path"),uniqueID,"/user_log_file.txt",sep="")
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"AllDisease",uniqueID,study_id,GRS,ethnicity_group,use_all_snp_score,real_dist,plot_heritability)
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
      GRS=GRS,
      distributionCurve=distributionCurve,
      study_id=study_id,
      use_all_snp_score=use_all_snp_score,
      ethnicity_group=ethnicity_group,
      show_sanogenetics_banner=show_sanogenetics_banner))
  })
  
  
  
  
  #The main gaussian distribution plot  
  output$plot_1 <- renderPlot({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      o<-get_data()
      SNPs_to_analyze<-o[["SNPs_to_analyze"]]
      GRS<-o[["GRS"]]
      distributionCurve <- o[["distributionCurve"]]
      use_all_snp_score <- o[["use_all_snp_score"]]
      
      if(is.na(GRS))stop("Could not calculate overall GRS because all SNPs in the signature were missing information about either risk-allele, effect-size or minor-allele-frequency.")
      
      
      
      #plotting for top-hit scores
      if(!use_all_snp_score){
        
        #get xlim, ylim and x and y
        #since scores are mean=0 and SD=1 we draw a reference curve
        reference_mean<-0
        reference_sd<-1
        xlim<-c(reference_mean - reference_sd*3, reference_mean + reference_sd*3) #ok, I suppose I could just write -3 to 3, but whatever
        reference_x<-seq(xlim[1],xlim[2],length.out=100)
        reference_y<-dnorm(reference_x,mean=reference_mean,sd=reference_sd)
        ylim <- range(reference_y)
        
        #draw curve
        plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n")
        par(mai=c(0.2,0.1,0.1,0.1))
        lines(x=reference_x,y=reference_y,lty=1,col="blue",lwd=2)
        
        #fill in shading
        if(!all(!reference_x<GRS)){
          max_GRS_i<-max(which(reference_x<GRS))
          upper_x<-reference_x[1:max_GRS_i]
          upper_y<-reference_y[1:max_GRS_i]
          x_lines <- c(upper_x,GRS,GRS,xlim[1])
          y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
          polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
        }
        
        #draw the main line
        abline(v=GRS,lwd=3)
        
        #optionally add real distribution curve
        if(!is.null(distributionCurve)){
          real_x <- distributionCurve[["x"]]
          real_y <- distributionCurve[["y"]]
          adj_y<-real_y * (max(reference_y) / max(real_y))
          lines(x=real_x,y=adj_y,lty=2)
        }
        
        #add legend depending on real-dist score
        if(is.null(distributionCurve)){
          legend("topleft",legend=c("Population distribution","Your genetic risk score"),lty=c(1,1),lwd=c(2,3),col=c("blue","black"))
        }else{
          legend("topleft",legend=c("Population distribution","Impute.me user distribution","Your genetic risk score"),lty=c(1,2,1),lwd=c(2,2,3),col=c("blue","black","black"))
        }
        
        #plotting for all-SNP scores
      }else{
        #get distributions
        distributionCurve <- o[["distributionCurve"]]
        real_x <- distributionCurve[["x"]]
        real_y <- distributionCurve[["y"]]
        
        
        #get xlim, ylim and x and y
        xlim<-range(c(-3,3,GRS))
        ylim <- c(0,max(real_y))
        
        
        #draw curve
        plot(NULL,xlim=xlim,ylim=ylim,ylab="Number of people with this score",xlab="Genetic risk score",yaxt="n")
        par(mai=c(0.2,0.1,0.1,0.1))
        lines(x=real_x,y=real_y,lty=2,col="black",lwd=1)
        
        #fill in shading
        if(!all(!real_x<GRS)){
          max_GRS_i<-max(which(real_x<GRS))
          upper_x<-real_x[1:max_GRS_i]
          upper_y<-real_y[1:max_GRS_i]
          x_lines <- c(upper_x,GRS,GRS,xlim[1])
          y_lines <- c(upper_y,upper_y[length(upper_y)],0,0)
          polygon(x=x_lines, y = y_lines, density = NULL, angle = 45,border = NA, col = rgb(0,0,1,0.3), lty = par("lty"))
        }
        
        #draw the main line
        abline(v=GRS,lwd=3)
        
        legend("topleft",legend=c("Impute.me user distribution","Your genetic risk score"),lwd=c(1,3),col=c("black","black"),lty=c(2,1))
      }
      
      
      
    }		
  })
  
  
  
  #The pie-chart version of the heritability plot
  output$plot_2 <- renderPlot({ 
    if(input$goButton == 0){
      return(NULL)
    }
    o<-get_data()
    study_id<-o[["study_id"]]
    use_all_snp_score <- o[["use_all_snp_score"]]
    ethnicity_group <- o[["ethnicity_group"]]
    
    #get variables
    known<-traits[study_id,"known_heritability"]
    # total <-traits[study_id,"total_heritability"]
    
    
    #if using all-SNP prs, then overwrite the data
    if(use_all_snp_score){
      all_snp_traits <- read.xlsx(all_snp_trait_file,rowNames=T)
      if(!study_id %in% rownames(all_snp_traits))stop(safeError("All SNP trait data not available for this study"))
      known<-all_snp_traits[study_id,"known_heritability"]
      # total <-all_snp_traits[study_id,"total_heritability"]
    }
    
    #if either of these are NA, we'll have to plot 'not registered'    
    # if(is.na(known) |  is.na(total)){
    #change to only 'known' have to be non-NA
    if(is.na(known)){
      plot(NULL,xlim=c(0,1),ylim=c(0,2),xaxt="n",yaxt="n",xlab="",ylab="",frame=F)
      text(x=0.5,y=1,label="Heritability not registered for this trait",col="grey80")
      
    }else{
      #define the plotting parameters (major is 'this score', minor is the others like 'unknown')
      major_text_r <- 0.6
      major_text_cex <- 0.8
      minor_text_r <- 0.4
      minor_text_cex <- 0.8
      
      if(ethnicity_group == "EUR"){
        major_text_col <- "grey60"
        minor_text_col <- "grey30"
        known_col <- "#006680FF"
        unknown_col <- "#0088AAFF"
      }else{
        major_text_col <- "grey80"
        minor_text_col <- "grey60"
        known_col <- "#AEAEAE"
        unknown_col <- "#D8D8D8"
        
      }
      
      #creating the JA-pie chart
      x1 <- known  #plot known genetics
      x2 <- 1-known
      
      #commented out parts for not showing twin-heritability
      # x2 <- total-known #plot unknown genetics
      # x3 <- 1 - total #plot environment
      #showing twin heritability as well (switched off)
      # pie( c(x1,x2,x3),labels=c("","",""),init.angle=init_angle_rad*(180/pi),col = c("#006680FF", "#0088AAFF", "#2AD4FFFF"),border=F)
      # x3_angle_rad <- init_angle_rad + x1*pi*2 + x2*pi*2 + x3*pi
      # text(x=minor_text_r*cos(x3_angle_rad),y=minor_text_r*sin(x3_angle_rad),label="environment",cex=minor_text_cex,col=minor_text_col)
      
      init_angle_rad <-  0.5*pi - (x1*pi)
      par(mai=c(0,0,0,0))
      pie(
        c(x1,x2),
        labels=c("",""),
        init.angle=init_angle_rad*(180/pi),
        col = c(known_col, unknown_col),
        border=F)
      
      #create labels
      x1_angle_rad <- init_angle_rad + x1*pi
      text(x=major_text_r*cos(x1_angle_rad),y=major_text_r*sin(x1_angle_rad),label="this score",cex=major_text_cex,col=major_text_col)
      x2_angle_rad <- init_angle_rad + x1*pi*2 + x2*pi
      text(x=minor_text_r*cos(x2_angle_rad),y=minor_text_r*sin(x2_angle_rad),label="unknown",cex=minor_text_cex,col=minor_text_col)
      
      #create (optional?) lines up to distribution plot
      lines(x=c(0.8*cos(init_angle_rad),1),y=c(0.8*sin(init_angle_rad),1))
      x1_end_rad <- init_angle_rad + x1*pi*2
      lines(x=c(0.8*cos(x1_end_rad),-1),y=c(0.8*sin(x1_end_rad),1))
      
      #subtitle or add warning for wrong variability in non-european
      if(ethnicity_group == "EUR"){
        mtext("Variablity explained",side=1,line=-2)    
      }else{
        text(
          x=0,y=0,
          label="Variability explained for this trait is uncertain\nfor your ancestral background.",
          cex=1,col="black")
      }
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
  
  
  #The table of SNPs and their effects
  output$table1 <- DT::renderDataTable({ 
    if(input$goButton == 0){
      return(NULL)
    }else if(input$goButton > 0) {
      
      #getting data
      o<-get_data()
      SNPs_to_analyze<-o[["SNPs_to_analyze"]]
      GRS<-o[["GRS"]]
      
      
      #summarising allele info into single-columns
      SNPs_to_analyze[,"Risk/non-risk Allele"]<-paste(SNPs_to_analyze[,"effect_allele"],SNPs_to_analyze[,"non_effect_allele"],sep="/")
      SNPs_to_analyze[,"Major/minor Allele"]<-paste(SNPs_to_analyze[,"major_allele"],SNPs_to_analyze[,"minor_allele"],sep="/")
      
      
      
      
      #rounding MAF and effect_size
      SNPs_to_analyze[,"minor_allele_freq"] <- signif(SNPs_to_analyze[,"minor_allele_freq"], 2)
      SNPs_to_analyze[,"effect_size"] <- signif(SNPs_to_analyze[,"effect_size"], 3)
      
      #removing duplicate GRS
      # SNPs_to_analyze[duplicated(SNPs_to_analyze[,"SNP"]),"GRS"]<-""
      
      #shortening the reported gene count
      SNPs_to_analyze[,"reported_genes"]<-sapply(strsplit(SNPs_to_analyze[,"reported_genes"],", "),function(x){
        paste(x[1:min(c(2,length(x)))],collapse=", ")
      })
      
      
      #marking duplicates
      for(col in c("genotype","personal_score","score_diff")){
        SNPs_to_analyze[is.na(SNPs_to_analyze[,col]),col] <- ""
      }
      
      
      keep<-c("SNP","genotype","Risk/non-risk Allele","effect_size","personal_score","score_diff"
              ,"p_value","Major/minor Allele","minor_allele_freq","reported_genes")
      SNPs_to_analyze<-SNPs_to_analyze[,keep]
      colnames(SNPs_to_analyze)<-c("SNP","Your Genotype","Risk/ non-risk Allele","Effect Size","SNP-score","SNP-score (population normalized)","P-value","Major/ minor Allele","Minor Allele Frequency","Reported Gene")
      return(SNPs_to_analyze)
    }
  },options = list(searching = FALSE,paging = FALSE),rownames= FALSE)
  
  
  
  #The long methods section written after the SNPs-table
  output$text_3 <- renderText({ 
    
    if(input$goButton == 0){
      methodsToReturn<-""
      
      
    }else{
      o<-get_data()
      methodsToReturn<-o[["methodsToReturn"]]
    }
    return(methodsToReturn)
  })
  
  
  
  #The Vancouver-study collaboration box
  output$text_4 <- renderText({ 
    minimum_level <- 5
    
    
    #Mention all UI elements to make sure this renderText object is triggered. Other than that these following 10 lines don't do anything
    if(input$only_show_newest){
      ui_selector <- paste0("trait_",input$trait_group,"_newest")  
    }else{
      ui_selector <- paste0("trait_",input$trait_group)
    }
    study_id<-input[[ui_selector]]
    ethnicity_group<-input$ethnicity_group
    use_all_snp_score<-input$use_all_snp_score
    plot_heritability <- input$plot_heritability
    real_dist<-input$real_dist
    
    
    if(input$goButton == 0){return("")}
    
    uniqueID<-gsub(" ","",input$uniqueID)
    if(uniqueID == "id_613z86871"){return("")}
    
    user_log_path <- paste0(get_conf("data_path"),uniqueID,"/user_log_file.txt")
    if(!file.exists(user_log_path))return("")
    
    
    
    
    
    cmd1 <- paste0("wc -l ",user_log_path)
    total_entries <- as.numeric(sub(" .+$","",system(cmd1,intern=T)))
    
    #to make sure we don't slow down the program too much for "old" users
    if(total_entries > 1000)return("")
    if(total_entries < minimum_level)return("")
    
    q<-grep("\tAllDisease\t",readLines(user_log_path),value=T)
    if(length(q)<minimum_level)return("")
    
    
    ######################
    #This is the logic for the Propeciahelp
    #
    ###################
    show_on_these_study_ids<- c("prostate-specific_antigen_levels_conditioned_on_lead_snps_28139693","prostate-specific_antigen_levels_28139693","male-pattern_baldness_30573740","male-pattern_baldness_29146897","male-pattern_baldness_28272467","male-pattern_baldness_28196072","male-pattern_baldness_22693459","dehydroepiandrosterone_sulphate_levels_21533175","androgen_levels_22936694","remission_after_ssri_treatment_in_mdd_or_neuroticism_29559929","remission_after_ssri_treatment_in_mdd_or_openness_29559929","response_to_ssri_in_mdd_or_openness_29559929","depressive_symptoms_ssri_exposure_interaction_25649181")
    if(study_id %in% show_on_these_study_ids){
      survey_log_file<-paste0(get_conf("submission_logs_path"),"propecia_survey.txt")
      if(!file.exists(survey_log_file))system(paste("touch",survey_log_file))
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"propecia_survey",uniqueID,study_id)
      m<-paste(m,collapse="\t")
      write(m,file=survey_log_file,append=TRUE)
      out <- paste0("<div style='background-color: #cfc ; padding: 10px; border: 1px solid green;'>
<p>Dear user<br></p>
<p><img style='padding: 0 15px; float: right;' src='../www/propeciahelplogo.png'>Since you are interested in this trait, perhaps you would also be interested in the The Propeciahelp study, run by William Gallaher. Propeciahelp is a Post-Finasteride Syndrome (PFS) patient organisation. PFS describes serious mental, physical and sexual side effects that persist – or more commonly deleteriously progress – after discontinuation of Finasteride. You can read more about it at this link, as well as participate:<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><u><a href='https://www.propeciahelp.com/'>https://www.propeciahelp.com</a></u></b><br><br>
                    The study involves (re)-upload of your raw DNA data outside of the impute-me site, there is no data transfer from here.</p></div>")
      return(out)
    }
    
    
    
    ######################
    #This is the logic for the personality genie
    #
    ###################
    if(sample(1:5,1)==1){
      show_on_these_study_ids<- c("agreeableness_21173776","conscientiousness_21173776","extroversion_21173776","neuroticism_21173776","openness_to_experience_21173776","anger_24489884","conscientiousness_27918536","feeling_fed-up_29500382","feeling_guilty_29500382","feeling_hurt_29500382","feeling_lonely_29500382","feeling_miserable_29500382","feeling_nervous_29500382","feeling_tense_29500382","feeling_worry_29500382","gambling_22780124","life_satisfaction_27089181","irritable_mood_29500382","worry_29942085","worry_too_long_after_an_embarrassing_experience_29500382","openness_to_experience_21173776","extroversion_21173776","self-reported_risk-taking_behaviour_30181555","self-reported_risk-taking_behaviour_30271922","temperament_22832960","eudaimonic_well-being_30279531","hedonic_well-being_30279531","subjective_well-being_mtag_29292387","subjective_well-being_multi-trait_analysis_29292387","subjective_well-being_27089181","subjective_well-being_29292387")
      if(study_id %in% show_on_these_study_ids){
        survey_log_file<-paste0(get_conf("submission_logs_path"),"personalitygenie_survey.txt")
        if(!file.exists(survey_log_file))system(paste("touch",survey_log_file))
        m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"personalitygenie_survey",uniqueID,study_id)
        m<-paste(m,collapse="\t")
        write(m,file=survey_log_file,append=TRUE)
        out <- paste0("<div style='background-color: #cfc ; padding: 10px; border: 1px solid green;'>
<p>Dear user<br></p>
<p><img style='padding: 0 15px; float: right;' src='../www/personalitygenielogo.png'>Since you are interested in this trait, perhaps you would also be interested in the Personality Genie study, run by Dr. Denise Cook. The study investigates human personality and the potential genetic associations linked with it. You can read more about it at this link, as well as participate:<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><u><a href='https://www.personalitygenie.com/'>www.personalitygenie.com</a></u></b><br><br>
                    The study involves (re)-upload of your raw DNA data outside of the impute-me site, there is no data transfer from here.</p></div>")
        return(out)
      }
    }
    
    
    ###################
    #This is the logic for the sanogenetics study. 
    show_box <- FALSE
    if(length(q) >= minimum_level & length(q)<10) show_box <- TRUE
    if(length(q)>=15 & length(q)<20) show_box <- TRUE
    if(length(q)>=25 & length(q)<30) show_box <- TRUE
    if(length(q)>=40 & length(q)<45) show_box <- TRUE
    if(length(q)>=60 & length(q)<75) show_box <- TRUE
    if(length(q)>=80 & length(q)<85) show_box <- TRUE
    if(length(q)>=100 & length(q)<105) show_box <- TRUE
    if(length(q)>=120 & length(q)<125) show_box <- TRUE
    if(length(q)>=150) show_box <- TRUE
    o<-get_data()
    if(o[["show_sanogenetics_banner"]] & show_box){
      survey_log_file<-paste0(get_conf("submission_logs_path"),"sanogenetics_survey.txt")
      if(!file.exists(survey_log_file))system(paste("touch",survey_log_file))
      m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"sanogenetics_survey",uniqueID,length(q))
      m<-paste(m,collapse="\t")
      write(m,file=survey_log_file,append=TRUE)
      out <- paste0("<div style='background-color: #cfc ; padding: 10px; border: 1px solid green;'>
<p><img style='padding: 0 15px; float: right;' src='../www/sano_logo.png'>Dear user<br>
You may be eligible to take part in a new clinical trial, supported by Sano Genetics. Why take part?<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#8226;Help contribute to the development a potential new treatment for a rare disease called alpha-1-antitrypsin deficiency<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#8226;Receive payment of £2,600 for completing the study<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&#8226;Travel expenses will be reimbursed or arranged by the trial-site on your behalf<br><br>

It’s easy to find out if you’re eligible to take part - click the button below to sign up to Sano Genetics and see if you qualify for next steps (it takes less than 10 minutes).<br><br>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u><a href='https://sanogenetics.com/land/zf874?utm_source=imputeme&utm_medium=referral&utm_campaign=zf874'>See if I qualify</a></u><br><br>
<i>The study involves (re)-upload of your raw DNA data outside of the impute-me site, there is no data transfer from here and the link is not tracked with your ID and this is not an affiliate link, since impute.me is not paid.</i></p></div>")
      return(out)
    }
    
    
    
    
    ###################
    #This is the logic for the UBC study. It's after the personality genie because only few traits
    #will go to the personality genie box.
    #now we can go to plot/no-plot logic, because each step will be more rare and won't slow down so much
#     show_box <- FALSE
#     if(length(q) >= minimum_level & length(q)<10) show_box <- TRUE
#     if(length(q)>=15 & length(q)<20) show_box <- TRUE
#     if(length(q)>=25 & length(q)<30) show_box <- TRUE
#     if(length(q)>=40 & length(q)<45) show_box <- TRUE
#     if(length(q)>=60 & length(q)<75) show_box <- TRUE
#     if(length(q)>=80 & length(q)<85) show_box <- TRUE
#     if(length(q)>=100 & length(q)<105) show_box <- TRUE
#     if(length(q)>=120 & length(q)<125) show_box <- TRUE
#     if(length(q)>=150) show_box <- TRUE
#     if(show_box){
#       survey_log_file<-"/home/ubuntu/logs/submission/ubc_survey.txt"
#       if(!file.exists(survey_log_file))system(paste("touch",survey_log_file))
#       m<-c(format(Sys.time(),"%Y-%m-%d-%H-%M-%S"),"ubc_survey",uniqueID,length(q))
#       m<-paste(m,collapse="\t")
#       write(m,file=survey_log_file,append=TRUE)
#       out <- paste0("<div style='background-color: #cfc ; padding: 10px; border: 1px solid green;'>
# <p>Dear user<br></p>
# <p><img style='padding: 0 15px; float: right;' src='../www/ubc_logo.png'>In collaboration with the University of British Columbia, we are currently conducting a study on motivations, perceptions and reactions in consumer genomics. We would therefore like to invite you to participate:<br><br>
# &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<b><u><a href='https://rc.bcchr.ca/redcap/surveys/?s=LAP43CYXTL'>Survey-link</a></u></b><br><br>
# The results of this survey is intended for scientific publication and by participating you will therefore help by contributing important knowledge to the genetics field.</p></div>")
#       return(out)
#     }
    
    
    
    
    
    
    
  })
  
  
  
  
  
})


