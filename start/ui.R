


shinyUI(fluidPage(
	titlePanel("Impute.me"),
	sidebarLayout(
		sidebarPanel(
			# fileInput("largeFile", "Upload genome data", multiple = FALSE, accept = NULL),
			# textInput(inputId="email", label = "Email", value = ""),
			a("ImputeMe", href="http://www.impute.me/imputeme"),br(),
			a("Terms-of-use", href='http://www.impute.me/www/terms_of_use.html'),br(),
			a("GuessMyHeight", href='http://www.impute.me/guessMyHeight'),br(),
			a("GuessMyHairColour", href="http://www.impute.me/hairColour"),br(),
			a("RareGene", href="http://www.impute.me/placeholder.html"),br(),
			a("MyOffspring", href="http://www.impute.me/placeholder.html"),br(),
			a("BRCA", href="http://www.impute.me/BRCA"),br(),
			a("Contact", href="http://www.impute.me/contact"),br(),
		
			
			# p("Then start imputation. This will take a while, but we'll mail you a download-link when ready"),
			# actionButton("goButton","Start imputation"),
			width=4
		),
		mainPanel(
			HTML("<h1>Welcome to impute.me</h1>
				<br>
			<br>


			This is the starting page for a cutting edge genetics analysis tools.<br><br>

			It can contribute better and more useful genetic analysis - if you already have genome-wide data from a direct-to-consumer service.<br><br>

		Because of expenses related to the building of the web-site it is currently only for paying backers of our <a href='http://kck.st/1VlrTlf'>kickstarter campaign</a> campaign. The project however is fundamentally non-profit and aim to driven only by donations<br><br>

		We take your anonymity much more seriously than any other online genetics-service: Your genome is only kept for two weeks for download and analysis, after which it is deleted from our servers. Also, we only want as little information about you as absolute required: an email to send the results to is the only requirement.<br><br>

	The code for the server is open-source and freely available at <a href='https://github.com/lassefolkersen/gene-surfer'>github</a>. These are the current modules:<br>
			<h2><a href='http://www.impute.me/imputeme'>ImputeMe</a> </h2>The core of the analysis engine. This advanced algorithm will take the 0.7M genotypes that customers of direct-to-consumer genetic testing companies usually receive. It will then analyze them for about 24 hours, using ethnicity information from the 1000 genomes project. Afterwards it will return an expanded version of 4M known imputed genotypes - 'guessed', but at a very high confidence level.
			<h2><a href='http://www.impute.me/guessMyHeight'>GuessMyHeight</a> </h2>A module that uses the 697 recently discovered main-drivers of variability in human height, and make a prediction of your height. In the majority of cases this should be accurate to within +/-5 cms, but do keep in mind that environment also plays a large role in most traits. For children this should be taken as expected final height.
			<h2><a href='http://www.impute.me/hairColour'>GuessMyHairColour</a> </h2>A module that attempts to guess your likely hair-colour using both black-brown-blonde scales and red-or-not scales in combination. Currently very much in beta-testing. If you have red-hair please write me and help with the tuning of the parameters.
			<h2><a href='http://www.impute.me/placeholder.html'>Rare gene discovery</a></h2>This module searches the genome of healthy adults for genes that are completely broken. The idea here is to use this as base for discovery projects on the thousands of genes in the genome that we know very little about. The existence of a broken or seriously altered version of an unknown gene in a healthy adult will be highly beneficial to the scientific community. Only use this module if you have an interest in contributing to science. Not implemented yet.
			<h2><a href='http://www.impute.me/placeholder.html'>Offspring trait prediction</a></h2>The height and hair module, just for use with children that are not even conceived yet. Not implemented yet. (Warning for larger confidence intervals).
			<h2><a href='http://www.impute.me/BRCA'>Enhanced BRCA2 investigation</a></h2>While this information is available for people who have access to the 23andme health area, the availability of imputed data improves the coverage somewhat and allows to catch some rare, maybe deleterious, mutations.")

		)
	)
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
# 				