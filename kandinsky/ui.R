
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Kandinsky-fy your Genome"),
	beginPage(),
	beginPanel('1/3'),
	HTML("A function to make unique art based on your genome.<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:<br><br>"),
	textInput(inputId="uniqueID", label = "", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	
	endPanel(),
	HTML("Making unique art from your genomic data is not a new idea. However, most places I've seen that offer such service actually just use very few SNPs. In this module the all SNPs analysed in the impute.me setting is combined to create a truly unique piece of art in the style of <u><a href='https://en.wikipedia.org/wiki/Wassily_Kandinsky'>Wassily Kandinsky</a></u><br><br><br>"),
	plotOutput("plot1"),
	
	
	endPage(),
	footer()
	
)	
)







