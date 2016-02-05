
isdebug<-FALSE

initialize<-function(name, isDebug = FALSE)
{
 isdebug <<-isDebug
}

head<-function()
{
	if(!isdebug)
	tags$head(includeHTML('../layout/pageparts/head.html'))
	else
	tags$head(HTML(paste('<style>',includeHTML('../layout/styling/base.css'),'</style>')))
}




navigation<-function()
{
	includeHTML('../layout/pageparts/navigation.html')
}

footer<-function()
{
	includeHTML('../layout/pageparts/footer.html')
}

beginPage<-function()
{
	includeHTML('../layout/pageparts/beginpage.html')
}

endPage<-function()
{
	HTML('</div>')
}


titlePanel<-function(title)
{
	HTML(paste0('<div class="title-block">
	<div class="container">
		<h1>' , title , '</h1>
	</div>
</div>'))
}

beginPanel<-function(size = '' ,id='')
{
	class<-'col-xs-12'

	if(size == '1/2') class<-'col-md-6 layout-panel'
	if(size == '1/3') class<-'col-md-4 layout-panel'
	if(size == '2/3') class<-'col-md-8 layout-panel'

	HTML(paste0('<div class="',class,'" id="',id,'">'))
}

endPanel<-function()
{
	HTML('</div>')
}