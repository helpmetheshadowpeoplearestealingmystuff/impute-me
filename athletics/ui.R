


shinyUI(fluidPage(
	titlePanel("Athletics related SNPs"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
			actionButton("goButton","Run analysis"),
			width=4

			
		),
		mainPanel(
			HTML("Some <a href='https://www.athgene.com/'>genetic tests</a> can be bought, that claim That you can 'Learn about your pre-dispositions in fitness, nutrition and sleep'. While this claim is largely exagerated, it is true that science knows about genetic variation that have impact on human fitness. These tests are sold for approximately 200USD, giving you knowledge of handful of specific fitness related SNPs. Of course the very same information is also available for free if you already have imputed genotyping microarray information as part of this site. 

Provided in this module is therefore an overview of your fitness related SNPs. For further information on each, we refer to the <a href='https://www.athgene.com/geneInfo/litteratureList'>litterature list</a> detailing the supporting information.<br><br>"),
			dataTableOutput("table1")
			# plotOutput("plot1")
			
			
		)
	)
))






