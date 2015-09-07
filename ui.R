
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("LIEB data (DLPFC poly-A)"),

  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
    	textInput(inputId="gene1", label = "Genesymbol", value = ""),
      selectInput("label", "Group:", choices = c(
      	"All diagnosis"="Dx",
      	"SZ Medication focus"="SZ Antipsychotics",
      	"Other gene"="gene",
      	"RIN value"="RIN",
      	"Age"="Age",
      	"Gender"="Sex"
      	)),
      conditionalPanel(
      	condition = "input.label == 'gene'",
      	textInput("gene2", "Other gene",value="")
      ),
    	conditionalPanel(
    		condition = "input.label != 'gene'",
    		selectInput("colourBy", "Colour by:", choices = c(
    			"None"="None",
    			"Diagnosis"="Dx",
    			"RIN value"="RIN",
    			"Age"="Age",
    			"Gender"="Sex",
    			"SZ Medication"="SZ Medication")),
    		
    		selectInput("pchBy", "Dot-type by:", choices = c(
    			"None"="None",
    			"Diagnosis"="Dx",
    			"Gender"="Sex",
    			"SZ Medication"="SZ Medication"))
    	),
    		
      sliderInput("age","Age span (y)",min=-1,max=80,value=c(15,80)),
      sliderInput("rin_min","Minimum RIN",min=5,max=10,value=6),
    	radioButtons("data_type", "Normalization", c("RPKM"="RPKM","log2 TMM" = "log2_TMM"), selected = "log2_TMM", inline = TRUE),

      actionButton("goButton","Investigate"),
    	downloadButton("downloadData", label = "Download"),
      width=4
    ),
     
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plotGenes")
    )
  )
))

