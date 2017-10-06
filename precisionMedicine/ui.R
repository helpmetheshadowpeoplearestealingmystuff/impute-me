
source("../uifunctions.R")
initialize('sti',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Pharmacogenetics"),
	beginPage(),
	beginPanel('1/3'),
	HTML("To run analysis input your user-id, or use the test-value of id_613z86871:<br><br>"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	downloadButton("downloadData", label = "Download data"),
	endPanel(),
	beginPanel('2/3'),
	h2("Introduction"),
	HTML("This is currently a collection of actionable pharmacogenetic SNPs. It is by no means meant to be a complete overview of all that exists, but instead is focusing on variants that seems reasonably validated, interesting and useful. Suggestions for further loci are welcome."),
	
	h2("HIV treatment response"),
	HTML("This is a fairly rare SNP, for which T-alleles are associated with adverse response to the <u><a href='http://www.ncbi.nlm.nih.gov/pubmed/18684101'>HIV-drug abacavir</a></u>. It is sometimes tested for before initiating abacavir treatment. Note however, that the studies supporting this are specifically only valid for caucasian ethnicities. Several other ethnicities are much more likely to have a T-allele, without problems. Note, that this SNP is also available in raw 23andme data without imputation. However it is included here because it is one of the more variants more often being checked for actual clinical use"),
	tableOutput("table1"),
	# dataTableOutput("table1"),
	
	h2("Statin response"),
	HTML("This is based on a recent publication of the <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=28223407'>genetics-based statin response in three large randomized clinical trials</a></u>. The genetic risk score (GRS) is calculated as closely as possible to what is described in the paper: by weighting the total number of risk alleles by their effects (log of the odds ratios) of CHD risk from the supplementary table, and filling out with frequency-averaged values at missing data points. Further details can be gained by using the download button and investigating the per-SNP overview. If the overall percentile score is >80%, the 'Score category' will be 'High genetic risk' corresponding to a relatively higher benefit from statins, as shown in figure 1 of the <u><a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=28223407'>study</a></u>. Otherwise it'll be 'All Others'."),
	tableOutput("table2"),
	
	
	
	h2("Opiod receptor"),
	HTML("This is based <u><a href='http://www.ncbi.nlm.nih.gov/pubmed/18250251'>on a study.</a></u> which found that having this opiod receptor variant responded better to naltrexone treatment of alcoholism. It has in subsequent studies been linked to other effects in opiod-receptor related diseases.<br><br>"),
	tableOutput("table3"),
	
	
	h2("CLL treatment response"),
	HTML("This is based <u><a href='http://www.ncbi.nlm.nih.gov/pubmed/24128861'>on a study.</a></u> which found that patients having the minor allele of both rs3745274 (T) and rs2279343 (G) responded worse to fludarabine plus cyclophosphamide treatment of chronic lymphoblastic leukemia (CLL).<br><br>"),
	tableOutput("table4"),

	
	h2("Testing systematic research into precision-medicine SNPs"),
	HTML("First test at broader view of precision medicine SNPs.<br><br>"),
	tableOutput("table5"),
	
	
	endPanel(),
	
	# plotOutput("plot1")
	
	endPage(),
	footer()
))






