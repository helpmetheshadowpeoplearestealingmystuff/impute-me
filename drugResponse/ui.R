

table_file <-paste0(get_conf("code_path"),"drugResponse/SNPs_to_analyze.txt")
# table_file <-"SNPs_to_analyze.txt"
SNPs_to_analyze<-read.table(table_file,sep="\t",header=T,stringsAsFactors=F)



diseases<-sort(unique(SNPs_to_analyze[,"disease"]))




source("../uifunctions.R")
initialize('sti',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Drug Response"),
	beginPage(),
	beginPanel('1/3'),
	HTML("To run analysis input your user-id, or use the test-value of id_613z86871:<br><br>"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	selectInput("disease", "Disease:", choices = diseases),
	uiOutput("ui"),
	
	actionButton("goButton","Run analysis"),
	
	
	
	endPanel(),
	beginPanel('2/3'),


	
	
	HTML("This is a test of a systematic approach to drug-response SNPs. Most of the known drug-response-associated genetics concern liver enzymes (e.g. CYP2C19) and their break-down of drug metabolites. These are well characterized elsewhere already. The focus of this module is to integrate systematic multi-SNP profiles beyond liver enzymes and provide estimates of drug-response.<br><br>To illustrate how this works, the module shows the calculations that takes place for a number of drug response predictions, both on a per-drug level and on a per-SNP level, corresponding to the first and the second table. The first table summarizes per-drug calculation whenever possible. If possible, a <u><a href='https://en.wikipedia.org/wiki/Standard_score'>Z-score</a></u> is calculated in the same way as also described in the <u><a href='https://www.impute.me/AllDiseases/'>complex disease</a></u> module. If not, it is indicated as 'not calculated'. In that case it is necessary to look at the second table for comments on the individual SNPs from the input studies. The Z-score approach takes information from many SNPs, and can therefore be considered as more thourough, of course depending on the underlying scientific study.<br><br>"),
	tableOutput("table1"),
	
	
	HTML("<br><br>"),
	tableOutput("table2"),
	
	
	endPanel(),
	
	endPage(),
	footer()
))






