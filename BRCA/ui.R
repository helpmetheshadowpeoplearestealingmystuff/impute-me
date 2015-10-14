


shinyUI(fluidPage(
	titlePanel("Expanded BRCA info"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_1l431dI24"),
			actionButton("goButton","Run analysis"),
			width=4
			
			
			
		),
		mainPanel(
			HTML("Thousands of mutations in the BRCA1 and BRCA2 genes have been documented. 23andMe reports data for three mutations (two in BRCA1 and one in BRCA2) that account for 80-90% of inherited breast and ovarian cancer among women with Ashkenazi Jewish ancestry. Other mutations in these two genes are not included in the 23andme report. Many can only be detected by sequencing, such as from myriad genetics. A few extra of interest can be reached with imputation analysis as follows.<br><br>
					 Remember you should also look up i4000377,i4000378,i4000379"),
			dataTableOutput("table1")
			# plotOutput("plot1")
			
			
		)
	)
))






