
source("../uifunctions.R")
initialize('gmh',TRUE)




# selections<-traits[,"study_id"]
# names(selections)<-traits[,"niceName"]

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("GWAS Calculator"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Participate in a sanity check asking if genetics-predicted political opinion match with real opinion:<br><br>To run analysis input your user-id, or use the test-value of id_613z86871 (but please don't input new political-opinions for the test-user, because that's me):<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	# selectInput("trait", "Traits:", choices = selections),
	
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







