
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
	HTML("Participate in a sanity check asking if genetics-predicted political opinion match with real opinion:<br><br>To run analysis input your user-id, and your political opinion on a scale from far-left/liberal to far-right/conservative. We also need to know your age, since age has a strong effect on political opinion.<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	sliderInput("real_opinion", "Your political opinion (left=liberal, right=conservative)",min=-3, max=3, value=0,step =0.1),
	textInput(inputId="real_age", label = "Your age", value = ""),
	
	actionButton("goButton","Run analysis"),
	HTML("<br><br><small>You can use the test-value of id_613z86871, but please don't change my political opinion too much</small>"),
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







