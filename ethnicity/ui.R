
source("../uifunctions.R")
initialize('sti',TRUE)
library(plotly)


cb_in<-as.character(1:5)
names(cb_in)<-

shinyUI(bootstrapPage(
	head(),
	navigation(),
	# titlePanel("Ethnicity and race"),
	titlePanel("Ethnicity Profile"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This browser investigates the ethnicity profile. You can use the ID id_613z86871 to test the function"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_613z86871"),
	# radioButtons("filtering", "filtering", c("None"="None","P<0.05"="0.05","P<0.01"="0.01","P<0.005"="0.005"), selected = "None"),
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  checkboxInput("pc_selections", "Principal components:", paste0("PC",as.character(1:5)), selected = c("PC1","PC2","PC3"))
	  
	),
	
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	h2("Plotly 3D plot of principal components"),
	
	endPanel(),
	
	# Optional alternative for when you want to show a plot
	plotlyOutput("mainPlot"),
	
	endPage(),
	footer()
))






