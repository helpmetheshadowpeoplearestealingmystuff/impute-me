
source("../uifunctions.R")
initialize('sti',TRUE)


trait_choices<-list.files("/home/ubuntu/prs_dir")

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("LDpred score"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This module analysis polygenic risk scores using the most extensive set of all SNPs possible.<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	selectInput("trait_choice", "Trait:", choices = trait_choices),
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	htmlOutput("text"),
	tableOutput("table1"),
	
	
	endPanel(),
	

	endPage(),
	footer()
))






