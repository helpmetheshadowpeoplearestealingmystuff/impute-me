
source("../uifunctions.R")
initialize('sti',TRUE)


trait_choices<-list.files("/home/ubuntu/prs_dir")
trait_choices<-trait_choices[!file.info(paste0("/home/ubuntu/prs_dir/",trait_choices))[,"isdir"]]


#defining 1000 genomes populations
ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("All-SNP polygenic scores"),
	beginPage(),
	beginPanel('1/3'),
	HTML("This module analysis polygenic risk scores using the most extensive set of all SNPs possible.<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:"),
	
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	selectInput("trait_choice", "Trait:", choices = trait_choices),
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	
	conditionalPanel(
	  condition = "input.advanced",
	  # checkboxInput("only_show_newest", label ="Only show newest study", value = TRUE),
	  # radioButtons("trait_group", "Trait categories:", trait_groups, selected = "disease"),
	  radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "automatic", inline = FALSE,width = NULL)
	  # checkboxInput("real_dist", label ="Plot real distribution (experimental)", value = TRUE)
	  
	),
	
	actionButton("goButton","Run analysis"),
	
	endPanel(),
	beginPanel('2/3'),
	
	htmlOutput("text"),
	tableOutput("table1"),
	plotOutput("plot_1"),
	
	endPanel(),
	

	endPage(),
	footer()
))






