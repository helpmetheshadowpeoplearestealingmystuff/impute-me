library("jsonlite")
uniqueIDs <- list.files("~/data")

o <- data.frame(row.names=uniqueIDs)

for(uniqueID in uniqueIDs){
  o[uniqueID, "found"] <- F
  path <- paste0("~/data/",uniqueID,"/",uniqueID,"_data.json")
  d <- fromJSON(path)
  if(!"rareDiseases"%in%names(d))next
  
  d1 <- d[["rareDiseases"]]
  
  if(!"advice"%in%names(d1))next
  
  o[uniqueID, "found"] <- T
  
  d2 <- d1[["advice"]]
  
  
  
  d3 <- sub("^.+: ","",d2)
  
  d4 <- gsub(" ","",strsplit(d3, ",")[[1]])
  
  for(d5 in d4){
    o[uniqueID, d5] <- TRUE
  }
  
}




for(col in colnames(o)){
  o[is.na(o[,col]),col] <- FALSE
  
  # print(paste(sum(o[,col]),"of", nrow(o), "for", col))
  
  
  
  print(paste(signif(100*sum(o[,col])/nrow(o),2), "% for", col))
  
}



[1] "32 % for Hemochromatosis(HFErelated)"
[1] "8.3 % for FamilialMediterraneanFever"
[1] "0.21 % for MediumChainAcylCoADehydrogenase(MCAD)Deficiency"
[1] "10 % for Alpha1AntitrypsinDeficiency"
[1] "1.9 % for G6PDDeficiency"
[1] "1 % for DPDDeficiency"
[1] "0.16 % for LimbgirdleMuscularDystrophy"
[1] "0.12 % for TTRRelatedCardiacAmyloidosis"
[1] "0.35 % for CongenitalDisorderofGlycosylationType1a(PMM2CDG)"
[1] "0.51 % for Phenylketonuria"
[1] "0.12 % for AutosomalRecessivePolycysticKidneyDisease"
[1] "0.93 % for HereditaryFructoseIntolerance"
[1] "0.047 % for GaucherDisease"
[1] "0.17 % for TaySachsDisease"
[1] "0.05 % for CysticFibrosis"
[1] "0.047 % for FamilialHypercholesterolemiaTypeB"
[1] "0.03 % for NeuronalCeroidLipofuscinosis(PPT1related)"
[1] "0.005 % for SallaDisease"


