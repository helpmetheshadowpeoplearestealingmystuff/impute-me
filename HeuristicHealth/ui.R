
source("../uifunctions.R")
initialize('ath',TRUE)


shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Heurestic Health <b>(Under development)</b>"),
	beginPage(),
				beginPanel('1/3'),
	textInput(inputId="uniqueID", label = "Unique ID", value = "id_XXXXXXXXX"),
	textInput(inputId="uniqueID", label = "Disease Context", value = ""),
	checkboxInput("advanced", label ="Advanced", value = FALSE),
	conditionalPanel(
		condition = "input.advanced",
		fileInput("rules_definition", "Upload new rules-defintion", multiple = FALSE, accept = NULL)
	),
	
	actionButton("goButton","Run analysis"),
	endPanel(),
	beginPanel('2/3'),
	HTML("'Heuristic', means any approach to problem solving, learning, or discovery that employs a practical method not guaranteed to be optimal or perfect, but sufficient for the immediate goals. Health, that's the context of you. <br><br>

<b>Too much text? Watch a video instead:</b><br>
<iframe width='560' height='315' src='https://www.youtube.com/embed/ny8unxAk4JY' frameborder='0' allowfullscreen></iframe><br><br>

<b>Details</b><br>
So in other words: Except for the few strong-effect cases (the 'long tail' and the 'mendelian' genetics), much of what we can learn from our genomes suffer from being intepreted outside of the current health context: Knowledge of an increased genetic risk of a particular disease-subtype may aid you only if you anyway are being evaluated for a diffuse set of symptoms that include this disease. For example, an increased genetic risk of leukemia may mean very little in a general population, but for patients with systemic joint pain it could be the difference between a wrongful investigation for rheumatoid arthritis or correct investigation for leukemia.<br><br>

This is the purpose of the Heurestic Health module. On providing a suspected disease name, snippet of journal text, or disease ICD-10 code, the goal of the algorithm is to provide you with relevant genetic information. Nothing more, nothing less. It may provide that no relevant genetic information was found; many diseases have little genetic basis. It may provide obvious information; disease risks that were already diagnosed. But it may also with new and useful knowledge, regarding differential diagnosis or drug response prediction.<br><br>
Example <i>Disease Context</i> texts: 'supected rheumatoid arthritis, systemic joint pain', 'M06.9'<br>
Example <i>Unique ID</i>: id_613z86871<br>"),
	
	endPanel(),
	dataTableOutput("table1"),
	# plotOutput("plot1")
	
	endPage(),
	footer()

	)	
)







