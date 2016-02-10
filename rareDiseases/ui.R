
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Rare diseases"),
	beginPage(),
	beginPanel('1/3'),
	HTML("Users with 23andme data had a special 'inherited conditions' analysis tool. However, this was taken off-line leaving only ancestry information availble. In this module the original 'inhereted conditions' are re-analysed:"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	
	endPanel(),
	HTML("This table shows the variants which are known to confer a range of severe inherited conditions. They are all fairly rare conditions. Slightly more common, however, is the case of being a carrier of these conditions. Being a carrier means that a person have one copy of a disease-causing allele, but will not be affected because the condition only manifests if both copies are of te disease-causing type."),
	tableOutput("table1"),
	# dataTableOutput("table1"),
	
	
	endPage(),
	footer()
	
)	
)







