source("../uifunctions.R")
initialize('con',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Contact"),
	beginPage(),
	HTML(	"
Genetics lead: <u><a href='http://www.dtu.dk/service/telefonbog/person?id=101696&tab=2&qt=dtupublicationquery'>Lasse Folkersen</a></u>.<br>
				
Design and interface lead: Stefan Delport.<br>
<br>
<br><br>
We welcome suggestions for other analysis types, particularly if you can program them yourself - the system is highly modular and can easily be extended even with just a limited knowledge of R-programming and genetics.<br>
				"),
# 	endPage(),
	footer()
))













