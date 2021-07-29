
source("../uifunctions.R")
initialize('sti',TRUE)


options(shiny.error = browser)


   if(!require("plotly"))stop(safeError("Unfortunately the 3D-plotting function in plotly is not configured correctly. This is likely because impute-me is running in a docker. You can still access the calculations from this module programmatically at e.g. http://localhost:3838/www/<uniqueID>_data.json. See issue #6 at http://www.github.com/lassefolkersen/impute-me/issues for more information."))

ethnicities<-c('AFR','AMR','EAS','EUR','SAS')
names(ethnicities)<-c('African','Ad Mixed American','East Asian','European','South Asian')

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Ancestry Profile"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This module investigates the ancestry profile. You can use the ID id_613z86871 to test the function"),

	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	# 
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  checkboxGroupInput("pc_selections", "Principal components:", paste0("PC",as.character(1:5)), selected = c("PC1","PC2","PC3")),
	  checkboxGroupInput("ethnicities", "Populations to show:", ethnicities, selected = ethnicities),
	  checkboxInput("scale_size", "Size-scale dots (remove for safari-browsers):", value = TRUE)
	),
	
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	htmlOutput("text_1"),
	
	plotlyOutput("mainPlot"),
	
	endPanel(),
	
	
	
	endPage(),
	footer()
))



