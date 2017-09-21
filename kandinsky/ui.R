
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Kandinsky-fy your Genome"),
	beginPage(),
	beginPanel('1/3'),
	HTML("To run analysis input your user-id, or use the test-value of id_613z86871:"),
	textInput(inputId="uniqueID", label = "", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	downloadButton("downloadData", label = "Download pdf"),
	endPanel(),
	beginPanel('2/3'),
	
	
	endPanel(),
	HTML("Making unique art from your genomic data is not a new idea. However, most places I've seen that offer such service actually just use very few SNPs. In this module the all SNPs analysed in the impute.me setting is combined to create a truly unique piece of art in the style of <u><a href='https://en.wikipedia.org/wiki/Wassily_Kandinsky'>Wassily Kandinsky</a></u>, using the code available in the <i><u><a href='http://giorasimchoni.com/2017/07/30/2017-07-30-data-paintings-the-kandinsky-package/'>kandinsky R-package</a></u></i>.<br><br><br>"),
	plotOutput("plot_1"),
	
	
	endPage(),
	footer()
	
)	
)







