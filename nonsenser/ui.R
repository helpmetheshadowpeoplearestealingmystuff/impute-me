source("../uifunctions.R")
initialize('ns',TRUE)

ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Gene mutations"),

	beginPage(),	
	beginPanel('1/3'),
	HTML("This module investigates rare coding SNPs, many of the only available with imputation. You can use the ID id_613z86871 to test the function"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "global", inline = FALSE,width = NULL)
	  ),
	
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	htmlOutput("text1"),
	endPanel(),
	DT::dataTableOutput("table1"),
	endPage(),
	footer()

))






