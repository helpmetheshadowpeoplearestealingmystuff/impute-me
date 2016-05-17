
source("../uifunctions.R")
initialize('gmh',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Autoimmune disease risk"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Most complex diseases have been investigated using the genome-wide association (GWAS) technology.<br><br>To run analysis input your user-id, or use the test-value of id_57n662948:<br><br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	h2("RA estimate"),
	htmlOutput("text_RA1"),
	plotOutput("plot_RA1"),
	htmlOutput("text_RA2"),
	
	HTML("<br><br><br>"),
	h2("IBD estimate"),
	htmlOutput("text_IBD1"),
	plotOutput("plot_IBD1"),
	htmlOutput("text_IBD2"),

	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







