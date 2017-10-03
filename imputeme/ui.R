# stop(getwd())
setwd("/home/ubuntu/srv/impute-me/imputeme")
source("../uifunctions.R")
# source("/home/ubuntu/srv/impute-me/uifunctions.R")
initialize('hc',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Initiate genome analysis"),
	beginPage(),
	beginPanel('1/3'),
	fileInput("largeFile", "Upload genome data", multiple = FALSE, accept = NULL),
	textInput(inputId="email", label = "Email", value = ""),
	HTML("<u><a href='https://www.impute.me/www/terms_of_use.html'>Terms of use.</a></u>"),
	checkboxInput("delete2weeks", "Delete data after two weeks", value = TRUE, width = NULL),
	actionButton("goButton","Start imputation"),

	
	endPanel(),
	beginPanel('2/3'),
	HTML("Imputation is a cutting edge computational technology that takes genetic measurements such as those provided by 23andme or ancestry.com and proceeds to impute - or 'guess' millions of additional genetic variations that were not measured in the original data. This is done based on overall knowledge of human ethnicity and ancestry and is further explained in the video of this <u><a href='https://www.kickstarter.com/projects/1563061294/impute-my-genome'>kickstart project</a></u>. The first step in using this site is to upload your genetic data, and then wait for the analysis to run a few days. After that you'll receive an email with an ID-login that you can use to explore the remaining analysis pages.<br><br>
			 This service is currently free, but donations are very welcome since the server requirements for analysis are quite high. Also the service is completely anonymous <br><br><br>"),
	
	# textOutput("text1"),
	textOutput("text2"),
	htmlOutput("text3"),
	endPanel(),
			
				
	endPage(),
	footer()
))












