


shinyUI(fluidPage(
	titlePanel("Statin induced cardiomyopathy"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
			actionButton("goButton","Run analysis"),
			width=4

			
		),
		mainPanel(
			HTML("Some genetic tests can be bought for a specific gene. A good example of this include the rs4363657/SLCO1B1 test sold by <a href='http://www.bostonheartdiagnostics.com/science_portfolio_statin.php'>Boston Heart Diagnostics</a> for 99$. However, when buying genome-wide kits such as those sold by ancestry.com or 23andme you can obtain the very same information from your raw data, at no extra cost. In this analysis module the analysis is provided. If you carry one or two 'C' alleles this is equal to a positive test from Boston Heart Diagnostics.<br><br>
Of note, however, it is a silly idea that you should worry about statins based on this information. The positive prediction rate is 5%, meaning that in 95% of the cases you will not have a problem even though your genotype contains a C. That hasn't stopped 250k US doctors from ordering the test. Here you can have it automatically:<br><br>"),
			dataTableOutput("table1")
			# plotOutput("plot1")
			
			
		)
	)
))






