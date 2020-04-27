source("../uifunctions.R")
initialize('gmh',TRUE)


#load functions and define paths of reference files and data directory
library(openxlsx)
trait_file<-"/home/ubuntu/srv/impute-me/AllDiseases/2020-04-02_trait_overview.xlsx"
traits <- read.xlsx(trait_file,rowNames=F)
rownames(traits) <- traits[,"study_id"]
traits<-traits[!is.na(traits[,"omit"]) & !traits[,"omit"],]


#traits to omit ad-hoc (because they don't work or because they are actively selected against)
traits<-traits[!traits[,"omit"],]


#defining 1000 genomes populations
ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")

#Define the trait groups
trait_groups<-c("all","disease","biometrics","biomarker","response","other")
names(trait_groups)<-c("All","Disease","Biometrics","Biomarker","Response","Other")


#create overview of what is selectable in the UI - 
#note there wil be one object per trait group (the 6 groups defined above), 
#and each of the will be duplicated into newest and all
selections_all<-traits[,"study_id"]
names(selections_all)<-traits[,"niceName"]

selections_disease<-traits[traits[,"disease"],"study_id"]
names(selections_disease)<-traits[traits[,"disease"],"niceName"]

selections_biometrics<-traits[traits[,"biometrics"],"study_id"]
names(selections_biometrics)<-traits[traits[,"biometrics"],"niceName"]

selections_biomarker<-traits[traits[,"biomarker"],"study_id"]
names(selections_biomarker)<-traits[traits[,"biomarker"],"niceName"]

selections_response<-traits[traits[,"response"],"study_id"]
names(selections_response)<-traits[traits[,"response"],"niceName"]

selections_other<-traits[traits[,"other"],"study_id"]
names(selections_other)<-traits[traits[,"other"],"niceName"]

selections_all_newest<-traits[traits[,"most_recent"],"study_id"]
names(selections_all_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"most_recent"],"niceName"])

selections_disease_newest<-traits[traits[,"disease"] & traits[,"most_recent"],"study_id"]
names(selections_disease_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"disease"] & traits[,"most_recent"],"niceName"])

selections_biometrics_newest<-traits[traits[,"biometrics"] & traits[,"most_recent"],"study_id"]
names(selections_biometrics_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"biometrics"] & traits[,"most_recent"],"niceName"])

selections_biomarker_newest<-traits[traits[,"biomarker"] & traits[,"most_recent"],"study_id"]
names(selections_biomarker_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"biomarker"] & traits[,"most_recent"],"niceName"])

selections_response_newest<-traits[traits[,"response"] & traits[,"most_recent"],"study_id"]
names(selections_response_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"response"] & traits[,"most_recent"],"niceName"])

selections_other_newest<-traits[traits[,"other"] & traits[,"most_recent"],"study_id"]
names(selections_other_newest)<-sub(" [PMID [0-9]+]$","",traits[traits[,"other"] & traits[,"most_recent"],"niceName"])




#Start main Shiny scripts
shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Complex diseases: The GWAS Calculator"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Thousands of genome-wide association studies (GWAS) have been performed. This module allows the calculation of genetic risk score for any of them.<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	
	conditionalPanel(
	  condition = "input.trait_group == 'all' & !input.only_show_newest",
	  selectInput("trait_all", "Traits:", choices = selections_all)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'disease' & !input.only_show_newest",
	  selectInput("trait_disease", "Traits:", choices = selections_disease)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'biometrics' & !input.only_show_newest",
	  selectInput("trait_biometrics", "Traits:", choices = selections_biometrics)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'biomarker' & !input.only_show_newest",
	  selectInput("trait_biomarker", "Traits:", choices = selections_biomarker)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'response' & !input.only_show_newest",
	  selectInput("trait_response", "Traits:", choices = selections_response)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'other' & !input.only_show_newest",
	  selectInput("trait_other", "Traits:", choices = selections_other)
	),


	
	conditionalPanel(
	  condition = "input.trait_group == 'all' & input.only_show_newest",
	  selectInput("trait_all_newest", "Traits:", choices = selections_all_newest)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'disease' & input.only_show_newest",
	  selectInput("trait_disease_newest", "Traits:", choices = selections_disease_newest)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'biometrics' & input.only_show_newest",
	  selectInput("trait_biometrics_newest", "Traits:", choices = selections_biometrics_newest)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'biomarker' & input.only_show_newest",
	  selectInput("trait_biomarker_newest", "Traits:", choices = selections_biomarker_newest)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'response' & input.only_show_newest",
	  selectInput("trait_response_newest", "Traits:", choices = selections_response_newest)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'other' & input.only_show_newest",
	  selectInput("trait_other_newest", "Traits:", choices = selections_other_newest)
	),
	
	
	
		
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  checkboxInput("only_show_newest", label ="Only show newest study", value = TRUE),
	  radioButtons("trait_group", "Trait categories:", trait_groups, selected = "disease"),
	  radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "automatic", inline = FALSE,width = NULL),
	  checkboxInput("use_all_snp_score", label ="Show all-SNP score if possible (experimental)", value = TRUE),
	  checkboxInput("plot_heritability", label ="Plot variability explained", value = TRUE),
	  checkboxInput("real_dist", label ="Plot user distribution (experimental)", value = FALSE)
	  
	),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	# h2("Genetic risk score:"),
	htmlOutput("text_4"),
	
	htmlOutput("text_1"),
	plotOutput("plot_1"),
	conditionalPanel(
	  condition = "input.plot_heritability",
	  plotOutput("plot_2",height = "200px")
	),
	htmlOutput("text_2"),
	DT::dataTableOutput("table1"),
	htmlOutput("text_3"),
	
	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







