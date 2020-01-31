
source("../uifunctions.R")
initialize('gmh',TRUE)



load("/home/ubuntu/srv/impute-me/ukbiobank/2017-09-28_trait_overoverview.rdata")


#omit traits
traits<-traits[!traits[,"omit"],]
#can also omit ad-hoc if necessary
# omitThese<-c(
  #traits to omit ad-hoc (because they don't work)  
  # )
# traits<-traits[!rownames(traits)%in%omitThese,]


#defining 1000 genomes populations
ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")

#Trait groups
trait_groups<-c("all","diagnosis","treatment","self_rep","illness_of_relatives","other")
names(trait_groups)<-c("All","Diagnosis","Treatment","Self-reported illness","Illness of Relatives","Other")



selections_all<-traits[,"study_id"]
names(selections_all)<-traits[,"niceName"]

selections_diagnosis<-traits[traits[,"diagnosis"],"study_id"]
names(selections_diagnosis)<-traits[traits[,"diagnosis"],"niceName"]

selections_treatment<-traits[traits[,"treatment"],"study_id"]
names(selections_treatment)<-traits[traits[,"treatment"],"niceName"]

selections_self_rep<-traits[traits[,"self_rep"],"study_id"]
names(selections_self_rep)<-traits[traits[,"self_rep"],"niceName"]

selections_illness_of_relatives<-traits[traits[,"illness_of_relatives"],"study_id"]
names(selections_illness_of_relatives)<-traits[traits[,"illness_of_relatives"],"niceName"]

selections_other<-traits[traits[,"other"],"study_id"]
names(selections_other)<-traits[traits[,"other"],"niceName"]

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("UK-Biobank Calculator"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("A study of ~½ million UK residents, known as the UK biobank, have recently been published. This module allows the calculation of genetic risk score for any of the published traits.<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	
	conditionalPanel(
	  condition = "input.trait_group == 'all'",
	  selectInput("trait_all", "Traits:", choices = selections_all)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'diagnosis'",
	  selectInput("trait_diagnosis", "Traits:", choices = selections_diagnosis)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'treatment'",
	  selectInput("trait_treatment", "Traits:", choices = selections_treatment)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'self_rep'",
	  selectInput("trait_self_rep", "Traits:", choices = selections_self_rep)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'illness_of_relatives'",
	  selectInput("trait_illness_of_relatives", "Traits:", choices = selections_illness_of_relatives)
	),
	conditionalPanel(
	  condition = "input.trait_group == 'other'",
	  selectInput("trait_other", "Traits:", choices = selections_other)
	),
	
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  radioButtons("trait_group", "Trait categories:", trait_groups, selected = "all"),
	  radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "automatic", inline = FALSE,width = NULL),
	  sliderInput("snp_p_value", "Per-SNP -log10(P):",min = 7, max = 11,value = 7, step = 0.1)
	  # checkboxInput("real_dist", label ="Plot real distribution (experimental)", value = FALSE)
	  
	),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	# h2("Genetic risk score:"),
	htmlOutput("text_1"),
	plotOutput("plot_1"),
	htmlOutput("text_2"),
	DT::dataTableOutput("table1"),
	htmlOutput("text_3"),
	
	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







