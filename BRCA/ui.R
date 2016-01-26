


shinyUI(fluidPage(
	tags$head(includeHTML('../layout/head.html')),
	includeHTML('../layout/navigation.html'),
	titlePanel("Expanded BRCA info"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
			actionButton("goButton","Run analysis"),
			width=4
			
			
			
		),
		mainPanel(
			HTML("Thousands of mutations in the BRCA1 and BRCA2 genes have been documented. 23andMe reports data for three mutations that account much of inherited breast cancer, but other possible mutations in these two genes are not included in the 23andme report. Many can only be detected by sequencing, such as from myriad genetics. However, a few hundred extra possible mutations of interest can be reached with imputation analysis. The following lists your genotype for the directly measured three 23andme-SNPs as well as all other SNPs in the two genes that are either missense or nonsense. For interpretation we recommend reading more about <a href='http://genetics.bwh.harvard.edu/pph2/'>polyphen</a> and <a href='http://sift.jcvi.org/'>sift</a>-scores.<br><br>"),
			dataTableOutput("table1")
			# plotOutput("plot1")
			
			
		),
	includeHTML('../layout/footer.html')
	)
))






