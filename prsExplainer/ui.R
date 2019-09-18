
source("../uifunctions.R")
initialize('ath',TRUE)

choices <- c("basic_bell_curve","example_results","burden_jar","full_stats")
names(choices) <- c("Basic reporting","Example results","Burden Jar","Full stats illustration")

prs_available <- c("SCZ Cases\nAll-SNP\nSCZ score","SCZ Cases\nTop-SNP\nSCZ score","BP Cases\nAll-SNP\nBP score")
names(prs_available) <- c("Schizophrenia All-SNP","Schizophrenia Top-SNP","Bipolar Disorder All-SNP")




shinyUI(bootstrapPage(
  head(),
  navigation(),
  titlePanel("Explaining Polygenic Risk Score"),
  beginPage(),
  beginPanel('1/3'),
  HTML("The purpose of this module is to allow visualization and exploration of what a genetic risk score means in a sandbox setting. You therefore don't need your uniqueID to use it, just use this slider to explore different possible risk scores<br>"),
  sliderInput("genetic_z_score", "Visualize This Genetic Score",min=-2, max=2, value=0,step=0.1),
  checkboxGroupInput("explanation_choices",label="Example to show",choices=choices,selected=NULL),
  conditionalPanel(
    condition = "input.explanation_choices.includes('example_results') || input.explanation_choices.includes('full_stats')",
    selectInput("trait_label", "Example-results: Disease-score to show", choices = prs_available)
  ),
  conditionalPanel(
    condition = "input.explanation_choices.includes('burden_jar')",
    sliderInput("environment_z_score", "Burden Jar: level of environmental burden*",min=-2, max=2, value=0,step=0.1),
    HTML("<small>*(note we can't really quantify enviromental burden, this is for explanatory purposes.)</small><br>")
  ),
  
  
  
  
  endPanel(),
  beginPanel('2/3'),
  
  
  
  conditionalPanel(
    condition = "input.explanation_choices.includes('basic_bell_curve')",
    plotOutput("plot_basic_bell_curve"),
    plotOutput("plot_heritability",height = "200px"),
    htmlOutput("text_basic_bell_curve")
  ),
  
  
  
  conditionalPanel(
    condition = "input.explanation_choices.includes('example_results')",
    plotOutput("plot_example_results"),
    htmlOutput("text_example_results")
  ),
  

  conditionalPanel(
    condition = "input.explanation_choices.includes('burden_jar')",
    plotOutput("plot_burden_jar_2"),
    htmlOutput("text_burden_jar")
    
  ),
  
  
  conditionalPanel(
    condition = "input.explanation_choices.includes('full_stats')",
    plotOutput("plot_full_stats"),
    htmlOutput("text_full_stats")
    
  ),
  
  
  endPanel(),
  endPage(),
  footer()
  
)	
)







