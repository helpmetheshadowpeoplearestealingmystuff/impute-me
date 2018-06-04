

table_file <-"/home/ubuntu/srv/impute-me/precisionMedicine/SNPs_to_analyze.txt"
# table_file <-"SNPs_to_analyze.txt"
SNPs_to_analyze<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)



diseases<-sort(unique(SNPs_to_analyze[,"disease"]))




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
	selectInput("disease", "Disease:", choices = diseases),
	uiOutput("ui"),
	
	actionButton("goButton","Run analysis"),
	
	
	
	endPanel(),
	beginPanel('2/3'),


	
	h2("Precision-medicine SNP calculations"),
	HTML("This is a test of a systematic approach to precision medicine SNPs. It shows the calculations that takes place for a number of drug response predictions, but on a per-drug level and on a per-SNP level, corresponding to the first and the second table. The first table summarizes per-drug calculation whenever possible, but since the data-curation is still ongoing this is not always possible. Ultimately the idea of these calculations is that they will be included in a more direct user-friendly output.<br><br>"),
	tableOutput("table1"),
	
	
	HTML("<br><br>"),
	tableOutput("table2"),
	
	
	endPanel(),
	
	endPage(),
	footer()
))






