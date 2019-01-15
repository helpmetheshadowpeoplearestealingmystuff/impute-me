# stop(getwd())
setwd("/home/ubuntu/srv/impute-me/imputeme")
source("../uifunctions.R")
initialize('hc',TRUE)

shinyUI(bootstrapPage(
	head(),
	navigation(),
	titlePanel("Initiate many-genome analysis"),
	beginPage(),
	beginPanel('1/3'),
	fileInput("largeFile", "Upload multi-person genome data", multiple = FALSE, accept = NULL),
	textInput(inputId="email", label = "Email", value = ""),
	checkboxInput("should_be_imputed", label ="Impute data", value = TRUE),
	HTML("<u><a href='https://www.impute.me/www/terms_of_use.html'>Terms of use.</a></u>"),
	actionButton("goButton","Start imputation"),
	endPanel(),
	beginPanel('2/3'),
	HTML("This is the module for upload of data from many people at the same time, corresponding to the <a href='https://www.impute.me/imputeme/'><u>individual-level upload module</u></a>. Note, that when uploading it is <i>very</i> important to wait until the process is finished. This may take several minutes and a receipt message will appear. If the browser window is closed or interupted before that, not all samples will process. <br><br>For cost-reasons, this module is <i>not</i> freely available. Only submissions with pre-approved recipient-email-adresses will work. The computational costs would be too large if we allowed bulk-submissions. Please contact us if you wish to set up a collaboration."),
	HTML("<br><br>The upload formats currently accepted and auto-detected are these. Other formats can be implemented as needed.<br><br>
	     <b>Format-1</b><br>
       <i>Upload as zipped tab-separated txt file. Must have the three left-most columns indicated, with headers exactly as written. Additional columns are taken as samples. All positions must be given in hg19/GRCh37 built notation. </i><br><br>
	     <table style='width:100%'>
  <tr>
	     <th>RsID</th>
	     <th>Chr</th> 
	     <th>Position</th>
       <th>SAMPLENAME1</th>
       <th>SAMPLENAME2</th>
	     </tr>
	     <tr>
	     <td>rs116587930</td>
	     <td>1</td> 
	     <td>752721</td>
       <td>AG</td>
       <td>AA</td>
	     </tr>
	     <tr>
	     <td>rs114525117</td>
	     <td>1</td> 
	     <td>759036</td>
       <td>AG</td>
       <td>GG</td>
	     </tr>
	     <tr>
	     <td>rs79373928</td>
	     <td>1</td> 
	     <td>801536</td>
	     <td>TT</td>
	     <td>TT</td>
	     </tr>
	     
	     </table>
	     <br>
	     <br>
	     "),
	htmlOutput("text"),
	endPanel(),
			
				
	endPage(),
	footer()
))












