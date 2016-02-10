
source("../uifunctions.R")
initialize('brca',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Expanded BRCA info"),
	beginPage(),	
	beginPanel('1/3'),
	textInput(inputId="uniqueID", label = "", value = "id_XXXXXXXXX"),
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	HTML("<br><br>Thousands of mutations in the BRCA1 and BRCA2 genes have been documented. 23andMe reports data for three mutations that account much of inherited breast cancer, but other possible mutations in these two genes are not included in the 23andme report. Many can only be detected by sequencing, such as from myriad genetics. However, a few hundred extra possible mutations of interest can be reached with imputation analysis. The following lists your genotype for the directly measured three 23andme-SNPs as well as all other SNPs in the two genes that are either missense or nonsense. For interpretation we recommend reading more about <u><a href='http://genetics.bwh.harvard.edu/pph2/'>polyphen</a></u> and <u><a href='http://sift.jcvi.org/'>sift-scores</a></u>.<br><br>"),

	endPanel(),
	dataTableOutput("table1"),
	
	endPage(),
	footer()
))






