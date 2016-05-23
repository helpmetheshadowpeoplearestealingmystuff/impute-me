
source("../uifunctions.R")
initialize('gmh',TRUE)




diseaseNames<-rbind(
	c("CLL","Chronic Lymphoblastic Leukemia","Berndt-2015","26956414"),
	c("ALL","Acute Lymphoblastic Leukemia","Xu-2013","23512250")
)
colnames(diseaseNames)<-c("Acronym","Disease","Source","PMID")
rownames(diseaseNames)<-diseaseNames[,"Acronym"]

diseases<-diseaseNames[,"Acronym"]
names(diseases)<-diseaseNames[,"Disease"]


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Leukemia risk prediction"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("The main forms of leukemia are Acute lymphoblastic leukemia (ALL), Chronic lymphocytic leukemia (CLL), Acute myelogenous leukemia (AML), and Chronic myelogenous leukemia (CML). Like with most complex diseases have been investigated using the genome-wide association (GWAS) technology. Particularly for the CLL form, several genetiv variants have been discovered that increase or decrease the lifetime risk of the disease.  The strong point of this approach is that it has provided insight into how commonly found genetic variants affects diseases of everybody, i.e. as opposed to rare genetics variants which typically only are of relevance to the few families in which they are found. This means that most GWAS findings readily have been replicated between separate studies of different patients. <br><br>The weak point of GWAS, however, is that  findings typically only explain small fractions of the risk of each disease. This means that their actualy prognostic value still is somewhat limited. This is discussed below.<br><br>To run analysis input your user-id, or use the test-value of id_57n662948:<br><br>"),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	selectInput("disease", "Disease:", choices = diseases),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	
	h2("Genetic risk score"),
	htmlOutput("text_1"),
	plotOutput("plot_1"),
	htmlOutput("text_2"),
	
	
	
	endPanel(),
	endPage(),
	footer()
		
	)
)







