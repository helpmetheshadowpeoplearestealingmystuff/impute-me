
source("../uifunctions.R")
initialize('sti',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Statin induced cardiomyopathy"),
	beginPage(),
	beginPanel('1/3'),
	HTML("Some drug response genetic tests can be bought, that are directed against just one specific gene. However, when buying genome-wide kits such as those sold by ancestry.com or 23andme you can obtain the very same information from your raw data, at no extra cost. In this module that analysis is provided.<br><br>To run analysis input your user-id, or use the test-value of id_57n662948:<br><br>"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	h2("HIV treatment response"),
	HTML("This is a fairly rare SNP, for which T-alleles are associated with adverse response to the <u><a href='http://www.ncbi.nlm.nih.gov/pubmed/18684101'>HIV-drug abacavir</a></u>. It is sometimes tested for before initiating abacavir treatment. Note however, that the studies supporting this are specifically only valid for caucasian ethnicities. Several other ethnicities are much more likely to have a T-allele, without problems.")
	dataTableOutput("table1"),
	
	h2("Statin response"),
	HTML("This is based on the <u><a href='http://www.bostonheartdiagnostics.com/science_portfolio_statin.php'>Boston Heart Diagnostics</a></u> test, which sells for 99$. The interpretation is that if you carry one or two 'C' alleles it is equal to a positive test.	Of note, however, it is a silly idea that you should worry about statins based on this information. The positive prediction rate is 5%, meaning that in 95% of the cases you will not have a problem even though your genotype contains a C. That hasn't stopped 250k US doctors from ordering the test. Here you can have it automatically:<br><br>"),
	dataTableOutput("table2"),
	
	endPanel(),
	
	# plotOutput("plot1")
	
	endPage(),
	footer()
))






