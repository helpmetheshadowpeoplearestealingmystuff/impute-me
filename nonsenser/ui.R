


shinyUI(fluidPage(
	titlePanel("Gene mutations"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
			actionButton("goButton","Run analysis"),
			width=4

			
		),
		mainPanel(
			HTML("Most SNPs in the genome are not actually found within a gene: They are 'intergenic'. When popular media talks about a gene-mutation however, they most often mean a SNP that actually alters the sequence of a gene. Because of selection pressure throughout our evolution, these are rare. They are often the focus of scientific studies using DNA-sequencing technology to discover causes of rare diseases. However, interestingly many of us actually have these 'gene-breaking' SNPs, while being perfectly healthy nonetheless. The imputation technology used here, gives the opportunity to identify a number of these based on just on genotyping microarray results. If you give your ID-code to this module a table of all measured missense and nonsense mutations will be presented. Most likely you will "),
			dataTableOutput("table1")
			# plotOutput("plot1")
			
			
		)
	)
))






