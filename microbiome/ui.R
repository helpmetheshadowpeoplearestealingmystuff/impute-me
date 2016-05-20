
source("../uifunctions.R")
initialize('sti',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Microbiome"),
	beginPage(),
	beginPanel('1/3'),
	HTML("Example ID to test: id_57n662948"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	h2("Overview of bacterial abundance of various gut bacteria"),
	HTML("Throughout the last decade the human gut microbiome have been extensively explored utilising emergent high throughtput techniques. Through these studies the microbiome has been associated with various conditions ranging from metabolic diseases and irritable bowel syndrome to mental disorders like autism spectrum disorder. In general, low diversity of microbial species in the gut is related to various disease states and connections between specific species and dysbiosis are being unraveled within this relative young field of study.<br>"),
	tableOutput("table1"), #getting the table created in server.R
	htmlOutput("text_1"),
	
	endPanel(),
	
	# Optional alternative for when you want to show a plot
	# plotOutput("plot1")
	
	endPage(),
	footer()
))






