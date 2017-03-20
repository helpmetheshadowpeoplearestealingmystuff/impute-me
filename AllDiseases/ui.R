
source("../uifunctions.R")
initialize('gmh',TRUE)



load("/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_trait_overoverview.rdata")

#testing
# load("AllDiseases/2017-02-21_trait_overoverview.rdata")


ethnicities<-c("Automatic Guess","Global average","EAS, CHB","EAS, JPT","EAS, CHS","EAS, CDX","EAS, KHV","EUR, CEU","EUR, TSI","EUR, FIN","EUR, GBR","EUR, IBS","AFR, YRI","AFR, LWK","AFR, GWD","AFR, MSL","AFR, ESN","AFR, ASW","AFR, ACB","AMR, MXL","AMR, PUR","AMR, CLM","AMR, PEL","SAS, GIH","SAS, PJL","SAS, BEB","SAS, STU","SAS, ITU")

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
	  textInput("ethnicity_group", "Ethnicity group (1000 genomes):",value=""),
	  radioButtons("ethnicity_group", label="ethnicity_group", choices=ethnicities, selected = "Global average", inline = FALSE,
	               width = NULL)
	  
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







