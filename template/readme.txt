In a module there's three mandatory files:
server.R
SNPs_to_analyze.txt
ui.R


To create a module, it is necessary to do these four steps:
1) Fill in the rsids and chromosomes of the SNPs you want to analyze in SNPs_to_analyze.txt
2) Go to server.R at the indicated section and do any calculations necessary (or skip and just get a table of genotypes)
3) Go to ui.R and write what the users should see on the web-page
4) throughout the three files search-replace 'template' to whatever your modules name is.



Details about files:
server.R - the boilerplate code imports the SNPs indicated in SNPs_to_analyze. It returns a table, but can also be modified to return an R-image. Then you need to use change the 'renderTable' part to renderPlot.

SNPs_to_analyze.txt - a tab-sep file. The two first columns SNP and chr_name are mandatory. The remaining are optional and just for the purpose of e.g. calculation in server.R

ui.R - code that defines what is shown on web. The only things that needs to be filled are, h2("Some title") and HTML("Some longer text describing what you do").