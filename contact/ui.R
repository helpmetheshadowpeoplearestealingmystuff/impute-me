source("../uifunctions.R")
initialize('con',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Contact"),
	beginPage(),
	HTML(	"
Genetics lead: <u><a href='http://orcid.org/0000-0003-0708-9530'>Lasse Folkersen</a></u>.<br>
				
Design and interface lead: Stefan Delport.<br>
HTTPS-security: <u><a href='https://xarentek.com'>Andrew Maris.</a></u><br>
<br>
<br><br>
We welcome suggestions for other analysis types, particularly if you can program them yourself - the system is highly modular and can easily be extended even with just a limited knowledge of R-programming and genetics.<br>
				"),
# 	endPage(),
	footer()
))













