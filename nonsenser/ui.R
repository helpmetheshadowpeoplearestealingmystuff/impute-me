


shinyUI(fluidPage(
	titlePanel("Gene mutations"),
	sidebarLayout(
		sidebarPanel(
			
			textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
			actionButton("goButton","Run analysis"),
			width=4

			
		),
		mainPanel(
			HTML("Most SNPs in the genome are not actually found within a gene: They are 'intergenic'. When popular media talks about a gene-mutation however, they most often mean a SNP that actually alters the sequence of a gene. Because of selection pressure throughout our evolution, these are rare. They are often the focus of scientific studies using DNA-sequencing technology to discover causes of rare diseases. However, interestingly many of us actually have these 'gene-breaking' SNPs, while being perfectly healthy nonetheless. The imputation technology used here, gives the opportunity to identify a number of these based on just on genotyping microarray results. If you give your ID-code to this module a table of all measured missense and nonsense mutations will be presented. <br><br>
					 
Interpretation of the table can be done in many ways. One is by searching for SNPs where you have one or two copies of the non-common allele, and then investigating the consequence using other resources such as <a href='http://www.ncbi.nlm.nih.gov/SNP/'>dbSnp</a> or <a href='http://exac.broadinstitute.org/'>ExAC</a>. Note however that the definition of 'common' is very dependent on ethnicity: in this browser common just means the allele most often found in impute.me users, but it is recommended to check the ethnical distribution in e.g. the <a href='http://www.1000genomes.org/'>1000 genomes browser</a>. Another help  provided is the <a href='http://genetics.bwh.harvard.edu/pph2/'>polyphen</a> and <a href='http://sift.jcvi.org/'>SIFT</a>-scores, which can give an indication of the consequence. Ultimately the goal of this is to see satisfy ones curiousity about the state of your own functional genes. If you happen to find out that you carry two copies of completely deleterious mutations (non-sense mutation) but otherwise feel healty, feel free to write me a <a href='http://www.google.com/recaptcha/mailhide/d?k=01rzXqaX5BCKhCKM0lTL4YjQ==&c=iq1lpHSQcZKMhH_YvNJ2Hqq1sFJpdHZqD6WgeM3nK5w='>mail</a>. By being healthy, in spite of a specific broken gene, you'd be contributing to complete our view of genes and how they work."),
			dataTableOutput("table1")
			# plotOutput("plot1")
			
			
		)
	)
))






