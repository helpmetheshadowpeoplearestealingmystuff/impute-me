
source("../uifunctions.R")
initialize('sti',TRUE)
library(plotly)

options(shiny.error = browser)

ethnicities<-c('AFR','AMR','EAS','EUR','SAS')
names(ethnicities)<-c('African','Ad Mixed American','East Asian','European','South Asian'))

shinyUI(bootstrapPage(
	head(),
	navigation(),
	# titlePanel("Ethnicity and race"),
	titlePanel("Ethnicity Profile"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This browser investigates the ethnicity profile. You can use the ID id_613z86871 to test the function"),

	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	# 
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  checkboxGroupInput("pc_selections", "Principal components:", paste0("PC",as.character(1:5)), selected = c("PC1","PC2","PC3")),
	  checkboxGroupInput("ethnicities", "Populations to show:", ethnicities, selected = ethnicities)
	  
	),
	
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	# h2("All "),
	plotlyOutput("mainPlot"),
	
	endPanel(),
	
	
	
	endPage(),
	footer()
))



