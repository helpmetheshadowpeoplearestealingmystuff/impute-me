# stop(getwd())
setwd("/home/ubuntu/srv/impute-me/imputeme")
source("../uifunctions.R")
initialize('hc',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Initiate many-genome analysis"),
	beginPage(),
	beginPanel('1/3'),
	fileInput("largeFile", "Upload multi-person genome data", multiple = FALSE, accept = NULL),
	textInput(inputId="email", label = "Email", value = ""),
	checkboxInput("should_be_imputed", label ="Impute data", value = TRUE),
	HTML("<u><a href='https://www.impute.me/www/terms_of_use.html'>Terms of use.</a></u>"),
	actionButton("goButton","Start imputation"),
	endPanel(),
	beginPanel('2/3'),
	HTML("This is the module for upload of data from many people at the same time, corresponding to the <a href='https://www.impute.me/imputeme/'><u>individual-level upload module</u></a>. It is <i>very</i> important to wait until the process is finished. This may take several minutes and a receipt message will appear. If the browser window is closed or interupted before that, not all samples will process. <br><br>Note, that for cost-reasons, this module is <i>not</i> freely available. Only submissions with pre-approved recipient-email-adresses will work. The computational costs would be too large if we allowed bulk-submissions. Please contact us if you wish to set up a collaboration.<br><br><br>"),
	
	htmlOutput("text"),
	endPanel(),
			
				
	endPage(),
	footer()
))












