
setUdunits <- function(){
  units <- units::valid_udunits()
  .geometa$units$lapply(1:nrow(units), function(i){
    unit <- units[i,]
    unit_gml <- switch(unit$source_xml,
      "base" = GMLBaseUnit$new(),
      "derived" = GMLDerivedUnit$new(),
      "accepted" = GMLConventionalUnit$new(),
      "common" = GMLConventionalUnit$new()
    )
    
    unit_gml$setIdentifier(unit$name_singular, unit$name_singular)
    unit_gml$addName(unit$name_singular)
    if(unit$symbol!="") unit_gml$setCatalogSymbol(unit$symbol)
    #if(unit$definition!="") unit_gml$addRemark(unit$definition)
    #if(unit$comment!="") unit_gml$addRemark(unit$comment)
    if(unit$source_xml=="base") unit_gml$setUnitsSystem("http://www.bipm.fr/en/si/")
    
    #if derived
    if(unit$source_xml=="derived"){
      unit_obj <- as_units(unit$def)
      unit_obj_units <- attributes(unit_obj)$units
      numerator <- unlist(strsplit(unit_obj_units$numerator,"\\."))
      for(num in numerator) unit_gml$addDerivationUnitTerm(num, 1)
      if(length(unit_obj_units$denominator)>0){
        denominators <- as.data.frame(table(unit_obj_units$denominator), stringsAsFactors = FALSE)
        for(j in 1:nrow(denominators)){
          unit_gml$addDerivationUnitTerm(denominators[j,1L], -denominators[j,2L])
        }
      }
    }
    
    #if accepted/common
    if(unit$source_xml %in% c("accepted","common")){
      
    }
    
  })
}