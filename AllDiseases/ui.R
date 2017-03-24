
source("../uifunctions.R")
initialize('gmh',TRUE)



load("/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_trait_overoverview.rdata")

#testing
# load("AllDiseases/2017-02-21_trait_overoverview.rdata")

#defining 1000 genomes populations
ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")


selections<-traits[,"study_id"]
names(selections)<-traits[,"niceName"]

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("GWAS Calculator"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Thousands of GWAS studies have been performed. This module allows the calculation of genetic risk score for any of them.<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	selectInput("trait", "Traits:", choices = selections),
	
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	
	conditionalPanel(
	  condition = "input.advanced",
	  radioButtons("ethnicity_group", label="Ethnicity group (1000 genomes):", choices=ethnicities, selected = "Global average", inline = FALSE,width = NULL)
	  
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







