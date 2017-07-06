source("../uifunctions.R")
initialize('sta',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Status"),
	beginPage(),
	plotOutput("load1"),
	actionButton("goButton","Generate report"),
	textOutput("text1"),
	HTML("<br><br><br>"),
	textInput(inputId="email", label = "Fast forward email", value = ""),
	actionButton("insertFastEmail","Insert"),
	textOutput("text2"),
	HTML("<br><br><br>"),
	endPage(),
	footer()	
	
))










