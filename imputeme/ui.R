source("../uifunctions.R")
initialize('hc',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Initiate genome analysis"),
	beginPage(),
	beginPanel('1/3'),
	fileInput("largeFile", "Upload genome data", multiple = FALSE, accept = NULL),
	textInput(inputId="email", label = "Email", value = ""),
	HTML("<u><a href='http://www.impute.me/www/terms_of_use.html'>Terms of use.</a></u>"),
	checkboxInput("delete2weeks", "Delete data after two weeks", value = TRUE, width = NULL),
	actionButton("goButton","Start imputation"),
	
	
	endPanel(),
	beginPanel('2/3'),
	HTML("Imputation is a cutting edge computational technology that takes genetic measurements such as those provided by 23andme or ancestry.com and proceeds to impute - or 'guess' millions of additional genetic variations that were not measured in the original data. This is done based on overall knowledge of human ethnicity and ancestry and is further explained in the video of this <u><a href='https://www.kickstarter.com/projects/1563061294/impute-my-genome'>kickstart project</a></u>. The first step in using this site is to upload your genetic data, and then wait for the analysis to run a few days. After that you'll receive an email with an ID-login that you can use to explore the remaining analysis pages.<br><br>
			 This service is currently free, but donations are very welcome since the server requirements for analysis are quite high. Also the service is completely anonymous <br><br><br>"),
	
	textOutput("text1"),
	textOutput("text2"),
	textOutput("text3"),
	endPanel(),
	
	
	endPage(),
	footer()
	))














# p("Donations to keep server running are gladly accepted"),
# img(src = "http://i.imgur.com/C823ybI.png", height = 100, width = 100),
# 			<form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top">
# 				<input type="hidden" name="cmd" value="_s-xclick">
# 				<input type="hidden" name="encrypted" value="-----BEGIN PKCS7-----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-----END PKCS7-----
# ">
# 				<input type="image" src="https://www.paypalobjects.com/da_DK/DK/i/btn/btn_donateCC_LG.gif" border="0" name="submit" alt="PayPal – den sikre og nemme måde at betale på nettet.">
# 				<img alt="" border="0" src="https://www.paypalobjects.com/da_DK/i/scr/pixel.gif" width="1" height="1">
# 				</form>