
source("../uifunctions.R")
initialize('gmh',TRUE)



diseaseNames<-rbind(
	c("RA","Rheumatoid Arthritis","Okada"),
	c("UC","Ulcerative colitis","ellinghaus"),
	c("CD","Crohn’s disease","ellinghaus"),
	c("PS","Psoriasis","ellinghaus"),
	c("PSC","Primary Sclerosing Cholangitis","ellinghaus"),
	c("AS","Ankylosing Spondylitis","ellinghaus")
)
colnames(diseaseNames)<-c("Acronym","Disease","Source")
rownames(diseaseNames)<-diseaseNames[,"Acronym"]

diseases<-diseaseNames[,"Acronym"]
names(diseases)<-diseaseNames[,"Disease"]


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Autoimmune disease risk"),
	beginPage(),	
	beginPanel('1/3'),
	HTML("Most complex diseases have been investigated using the genome-wide association (GWAS) technology. This strong point of this approach is that it has provided insight into how commonly found genetic variants affects diseases of everybody, i.e. as opposed to rare genetics variants which typically only are of relevance to the few families in which they are found. This means that most GWAS findings readily have been replicated between separate studies of different patients. <br><br>The weak point of GWAS, however, is that  findings typically only explain small fractions of the risk of each disease. This means that their actualy prognostic value still is somewhat limited. This is discussed below.<br><br>To run analysis input your user-id, or use the test-value of id_57n662948:<br><br>"),
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







