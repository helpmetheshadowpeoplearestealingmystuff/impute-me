
source("../uifunctions.R")
initialize('gmh',TRUE)



load("AllDiseases/AllDiseases/2017-02-12_trait_overoverview.rdata")


head(traits)
selections<-traits[,"trait_pmid"]
names(selections)<-traits[,"niceName"]

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Autoimmune disease risk"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Thousands of GWAS studies have been performed. This module allows the calculation of genetic risk score for any of them.<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	selectInput("trait", "Traits:", choices = selections),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	h2("Genetic risk score:"),
	htmlOutput("text_1"),
	plotOutput("plot_1"),
	htmlOutput("text_2"),
	
	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







