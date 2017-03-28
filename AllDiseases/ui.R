
source("../uifunctions.R")
initialize('gmh',TRUE)



load("/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_trait_overoverview.rdata")

#testing
# load("AllDiseases/2017-02-21_trait_overoverview.rdata")

#defining 1000 genomes populations
ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")

#Trait groups
trait_groups<-c("all","disease","biometrics","biomarker","response","other")
names(trait_groups)<-c("All","Disease","Biometrics","Biomarker","Response","Other")


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

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("GWAS Calculator"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Thousands of GWAS studies have been performed. This module allows the calculation of genetic risk score for any of them.<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	
	conditionalPanel(
	  condition = "input.trait_group == 'all'",
	  selectInput("trait_all", "Traits:", choices = selections_all)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'disease'",
	  selectInput("trait_disease", "Traits:", choices = selections_disease)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'biometrics'",
	  selectInput("trait_biometrics", "Traits:", choices = selections_biometrics)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'biomarker'",
	  selectInput("trait_biomarker", "Traits:", choices = selections_biomarker)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'response'",
	  selectInput("trait_response", "Traits:", choices = selections_response)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'other'",
	  selectInput("trait_other", "Traits:", choices = selections_other)
	),
	
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  radioButtons("trait_group", "Trait categories:", trait_groups, selected = "all"),
	  radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "global", inline = FALSE,width = NULL)
	  
	),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	# h2("Genetic risk score:"),
	htmlOutput("text_1"),
	plotOutput("plot_1"),
	htmlOutput("text_2"),
	dataTableOutput("table1"),
	htmlOutput("text_3"),
	
	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







