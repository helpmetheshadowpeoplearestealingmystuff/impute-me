
source("../uifunctions.R")
initialize('gmh',TRUE)



load("/home/ubuntu/srv/impute-me/AllDiseases/2017-02-21_trait_overoverview.rdata")

#testing
# load("AllDiseases/2017-02-21_trait_overoverview.rdata")


#traits to omit ad-hoc (because they don't work)
# traits<-traits[!traits[,"omit"],]
# 
# omitThese<-c(
#   "behavioural_disinhibition_generation_interaction_23942779",
#   "body_mass_index_in_non-asthmatics_23517042"
#   
#   )
# traits<-traits[!rownames(traits)%in%omitThese,]


#defining 1000 genomes populations
ethnicities<-c("automatic","global","AFR", "AMR", "EAS", "EUR", "SAS")
names(ethnicities)<-c("Automatic guess","Global average","African","Ad Mixed American","East Asian","European","South Asian")

#Trait groups
# trait_groups<-c("all","disease","biometrics","biomarker","response","other")
# names(trait_groups)<-c("All","Disease","Biometrics","Biomarker","Response","Other")


# selections_all<-traits[,"study_id"]
# names(selections_all)<-traits[,"niceName"]
# 
# selections_disease<-traits[traits[,"disease"],"study_id"]
# names(selections_disease)<-traits[traits[,"disease"],"niceName"]
# 
# selections_biometrics<-traits[traits[,"biometrics"],"study_id"]
# names(selections_biometrics)<-traits[traits[,"biometrics"],"niceName"]
# 
# selections_biomarker<-traits[traits[,"biomarker"],"study_id"]
# names(selections_biomarker)<-traits[traits[,"biomarker"],"niceName"]
# 
# selections_response<-traits[traits[,"response"],"study_id"]
# names(selections_response)<-traits[traits[,"response"],"niceName"]
# 
# selections_other<-traits[traits[,"other"],"study_id"]
# names(selections_other)<-traits[traits[,"other"],"niceName"]


choices <- c("intelligence_29326435","emotional_Intelligence_29527006")
names(choices) <- c("IQ","EQ")


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Intelligence Calculator"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Recent studies in intelligence have found several SNPs associated with both IQ and EQ (emotional intelligence).<br><br>To run analysis input your user-id, or use the test-value of id_613z86871:<br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	

  selectInput("trait", "Traits:", choices = choices ),
	
	checkboxInput("advanced", label ="Advanced options", value = FALSE),
	conditionalPanel(
	  condition = "input.advanced",
	  # radioButtons("trait_group", "Trait categories:", trait_groups, selected = "all"),
	  radioButtons("ethnicity_group", label="Reference population:", choices=ethnicities, selected = "automatic", inline = FALSE,width = NULL)
	  # checkboxInput("real_dist", label ="Plot real distribution (experimental)", value = FALSE)
	  
	),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	# h2("Genetic risk score:"),
	htmlOutput("text_1"),
	plotOutput("plot_1"),
	htmlOutput("text_2"),
	dataTableOutput("table1"),
	htmlOutput("text_3"),
	
	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







