


shinyUI(fluidPage(
	titlePanel("Analyse genome"),
	sidebarLayout(
		sidebarPanel(
			test<-fileInput("largeFile", "Upload file", multiple = FALSE, accept = NULL)
			
			),
		mainPanel(
			
			textOutput("text1"),
			p("p creates a paragraph of text."),
			p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt")
			
	)
))

