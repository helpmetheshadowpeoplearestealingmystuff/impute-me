source("/home/ubuntu/srv/impute-me/functions.R")


export_function<-function(uniqueID){
  
  output <- list(
    power = list(
      "1"=list(
        marker="rs1815739",
        gene_name="ACTN3",
        genotype="unknown",
        positive_effect_allele="C"
      ),
      "2"=list(
        marker="rs4646994",
        gene_name="something",
        genotype="unknown",
        positive_effect_allele="C"
      )
    ),
    endurance=list(
      "1"=list(
        marker="rs699",
        gene_name="GENE1",
        genotype="unknown",
        positive_effect_allele="C"
      )
    )
  )
    
  
  for(topic in names(output)){
    for(snp in names(output[[topic]])){
      snp_id <- output[[topic]][[snp]][["marker"]]
      genotypes<-get_genotypes(uniqueID=uniqueID,request=snp_id)
      output[[topic]][[snp]][["unknown"]] <- genotypes[1,1]
    }
  }
  
  

  return(output)
  
}




