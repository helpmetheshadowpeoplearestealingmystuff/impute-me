source("../uifunctions.R")
initialize('ns',TRUE)

ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Gene mutations"),

	beginPage(),	
	beginPanel('1/3'),
	HTML("This module investigates rare coding SNPs, many of the only available with imputation. You can use the ID id_613z86871 to test the function"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "global", inline = FALSE,width = NULL)
	  ),
	
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
			HTML("Most SNPs in the genome are not actually found within a gene: They are 'intergenic'. When talking about a gene-mutation however, as is done in popular media, most often the meaning is a SNP that alters the sequence of a gene. Because of selection pressure throughout our evolution, these are rare. Also, they are often the focus of scientific studies using DNA-sequencing technology to discover causes of rare diseases. However, interestingly many of us actually have these 'gene-breaking' SNPs while nonetheless being perfectly healthy. The imputation technology used with this site, gives the opportunity to identify a number of these based on just on genotyping microarray results. If you give your ID-code to this module a table of all measured missense and nonsense mutations will be presented. <br><br>
					 
	Interpretation of the table can be done in many ways and unlike other modules, this does not give 'one true answer'. One method is to search for SNPs where you have one or two copies of the non-common allele and then investigate the consequence using other resources such as <u><a href='http://www.ncbi.nlm.nih.gov/SNP/'>dbSnp</a></u> or <u><a href='http://exac.broadinstitute.org/'>ExAC</a></u>. Note however that the definition of 'common' is very dependent on ethnicity: in this browser common just means the allele most often found in impute.me-users. However, it is recommended to check the ethnical distribution in e.g. the <a href='http://www.1000genomes.org/'>1000 genomes browser</a>. Another help provided is the <a href='http://genetics.bwh.harvard.edu/pph2/'>polyphen</a> and <a href='http://sift.jcvi.org/'>SIFT</a>-scores, which can give an indication of the consequence. Ultimately the goal of this is to satisfy ones curiousity about the state of your functional genes. If you happen to find out that you carry two copies of completely deleterious mutations (non-sense mutation) but otherwise feel healthy, feel free to write me a <u><a href='http://www.google.com/recaptcha/mailhide/d?k=01rzXqaX5BCKhCKM0lTL4YjQ==&c=iq1lpHSQcZKMhH_YvNJ2Hqq1sFJpdHZqD6WgeM3nK5w='>mail</a></u>. By being healthy, in spite of a specific broken gene, you'd be contributing to complete our view of genes and how they work."),
			
	endPanel(),
	dataTableOutput("table1"),
			# plotOutput("plot1")
			
	endPage(),
	footer()

))






