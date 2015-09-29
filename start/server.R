library("shiny")


shinyServer(function(input, output) {
	
	
	output$load1 <- renderPlot({ 
		par(bg = rgb(red=1, green=1, blue=1, alpha=0, maxColorValue = 1))
		
		processes<-list.files("/home/ubuntu/imputations/")
		totalProcesses <- length(processes)
		runningProcesses<-0
		for(process in processes){
			status_file<-paste("/home/ubuntu/imputations/",process,"/job_status.txt",sep="")
			if(file.exists(status_file)){
				jobStatus<-read.table(status_file,stringsAsFactors=FALSE,header=FALSE,sep="\t")[1,1]
				if(jobStatus=="Job is running"){
					runningProcesses<-runningProcesses+1
				}
			}
		}

		
		runningProcesses<-1
		totalProcesses<-1
		currentLoad<-matrix(c(runningProcesses,totalProcesses-runningProcesses),ncol=1)
		barplot(currentLoad,xlim=c(0,5),ylim=c(0,2),main="",horiz=T)
		title(xlab="imputations")
		abline(v=1,lty=2)
		abline(v=3,lty=2)
		text(3.02,1.3,adj=0,label="Max queue")
		text(1.02,1.3,adj=0,label="Max running")
		legend("topright",pch=15,col=c("grey50","grey10"),legend=c("Queued","Running"))
		
	})
	
	
	output$text1 <- renderText({ 
		
		m<-"<h1>Welcome to impute.me</h1>
				<br>
				<br>
				This is the starting page for a cutting edge genetics analysis tools. If you use this service, please consider supporting our <a href='http://kck.st/1VlrTlf'>kickstarter campaign</a>. These are the current modules:<br>
					<h2><a href='http://www.impute.me/imputeme'>ImputeMe</a> </h2>The core of the analysis engine. This advanced algorithm will take the 0.7M genotypes that customers of direct-to-consumer genetic testing companies usually receive. It will then analyze them for about 24 hours, using ethnicity information from the 1000 genomes project. Afterwards it will return an expanded version of 4M known imputed genotypes - 'guessed', but at a very high confidence level.
					<h2><a href='http://www.impute.me/guessMyHeight'>GuessMyHeight</a> </h2>A module that uses the 697 recently discovered main-drivers of variability in human height, and make a prediction of your height. In the majority of cases this should be accurate to within +/-5 cms, but do keep in mind that environment also plays a large role in most traits. For children this should be taken as expected final height.
					<h2><a href='http://www.impute.me/hairColour'>GuessMyHairColour</a> </h2>A module that attempts to guess your likely hair-colour using both black-brown-blonde scales and red-or-not scales in combination. Currently very much in beta-testing. If you have red-hair please write me and help with the tuning of the parameters.
					<h2><a href=''>Rare gene discovery</a></h2>This module searches the genome of healthy adults for genes that are completely broken. The idea here is to use this as base for discovery projects on the thousands of genes in the genome that we know very little about. The existence of a broken or seriously altered version of an unknown gene in a healthy adult will be highly beneficial to the scientific community. Only use this module if you have an interest in contributing to science. Not implemented yet.
					<h2><a href=''>Offspring trait prediction</a></h2>The height and hair module, just for use with children that are not even conceived yet. Not implemented yet. (Warning for larger confidence intervals).
					<h2><a href=''>Enhanced BRCA2 investigation</a></h2>While this information is available for people who have access to the 23andme health area, the availability of imputed data much improves the coverage and allows to catch several rare, but equally-dangerous, mutations. Not implemented yet.
					
					<br><br><br>
					<h2>Current server load:</h2>"
		return(m)
	})
	
	
	output$text2 <- renderText({ 
	m<-"<br><br><br>(Obviously the front-end parts of this page is very much under construction. If you are a web-developer, please write me an <a href='http://www.google.com/recaptcha/mailhide/d?k=01pdzWyCfeU-_1PRAPdKlJfg==&amp;c=3eyQPG-VqkHu6ECGRBSHdRraKCXOUsVtLpuyWWt-dpY=' onclick='window.open('http://www.google.com/recaptcha/mailhide/d?k\07501pdzWyCfeU-_1PRAPdKlJfg\75\75\46c\0753eyQPG-VqkHu6ECGRBSHdRraKCXOUsVtLpuyWWt-dpY\075', '', 'toolbar=0,scrollbars=0,location=0,statusbar=0,menubar=0,resizable=0,width=500,height=300'); return false;' title='Reveal this e-mail address'>email</a>)."
	return(m)
	})
	
	
})


