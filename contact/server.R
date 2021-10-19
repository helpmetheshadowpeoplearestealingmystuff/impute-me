library("shiny")


shinyServer(function(input, output) {
	
  
  
  output$text_1 <- renderText({ 
    external_material_path <- paste0(get_conf("misc_files_path"),"external_material.txt")
    if(file.exists(external_material_path)){
        m <- "<br><br><br><b>Related media and web-resources</b><br>"
        return(m)
      }else{
        m <- ""
      }
    return(m)  
      
    
  })
  
  
  
	
  output$table_1 <- DT::renderDataTable({ 
	  external_material_path <- paste0(get_conf("misc_files_path"),"external_material.txt")
	  
	  required_headers <- c("short_title",	"long_title",	"year",	"author",	"language",	"url",	"url2","display")
	  used_headers <- c("short_title",	"long_title",	"year",	"language",	"url")
	  nice_header_names <- c("Entry",	"Description",	"Year",	"Language",	"Link")
	  d <- NULL
	  
	  #if there exists a relevant file, we make that into a table in the contacts section
	  if(file.exists(external_material_path)){
      d<-read.table(external_material_path,sep="\t",header=T,stringsAsFactors = F,comment.char = "")
      if(all(required_headers%in% colnames(d))){
        d <- d[d[,"display"],]
        
        single_urls<-which(d[,"url2"]=="")
        d[single_urls,"url"]<-paste0("<a href='",d[single_urls,"url"],"'>link</a>")

        double_urls<-which(d[,"url2"]!="")        
        d[double_urls,"url"]<-paste0("<a href='",d[double_urls,"url"],"'>link1</a>, <a href='",d[double_urls,"url2"],"'>link2</a>")
        
        
        d <- d[,used_headers]
        colnames(d) <- nice_header_names
        
        
      }
    }
	  
	  
		
		return(d)
	},escape=F,options = list(searching = FALSE, paging = FALSE),rownames= FALSE)

	
	
})


