
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Kandinskyfy your Genome"),
	beginPage(),
	beginPanel('1/3'),
	HTML("To run analysis input your user-id, or use the test-value of id_613z86871:"),
	textInput(inputId="uniqueID", label = "", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	downloadButton("downloadData", label = "Download pdf"),
	endPanel(),
	beginPanel('2/3'),
	
	
	endPanel(),
	HTML("Making unique art from your genomic data is not a new idea. However, most places I've seen that offer such service actually just use very few SNPs. In this module all your trait-associated SNPs are combined to create a truly unique piece of art in the style of <u><a href='https://en.wikipedia.org/wiki/Wassily_Kandinsky'>Wassily Kandinsky</a></u>, using the beautiful code available in the <i><u><a href='http://giorasimchoni.com/2017/07/30/2017-07-30-data-paintings-the-kandinsky-package/'>kandinsky</a></u></i> R-package.<br><br><br>
	     You may ask if the drawing reveals deeper insight about your genome and inner self? Maybe it does, maybe it doesn't. Either way - it is guaranteed to be a unique drawing derived only from your genome. You may print it and use it as art. I do.
	     "),
	plotOutput("plot_1"),
	
	
	endPage(),
	footer()
	
)	
)







