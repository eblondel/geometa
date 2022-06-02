#' GMLUnitDefinition
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO GML unit definition
#' @return Object of \code{\link{R6Class}} for modelling an GML unit definition
#' @format \code{\link{R6Class}} object.
#'
#' @field quantityTypeReference [\code{\link{character}}]
#' @field catalogSymbol [\code{\link{character}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults, id)}}{
#'    This method is used to instantiate a GML Definition
#'  }
#'  \item{\code{setQuantityTypeReference(ref)}}{
#'    Set the quantity type reference. The content is a reference to a remote value
#'  }
#'  \item{\code{setCatalogSymbol(symbol)}}{
#'    Sets the preferred lexical symbol used for this unit of measure
#'  
#'  }
#' }
#' 
#' @examples 
#'   gml <- GMLUnitDefinition$new()
#'   gml$setDescriptionReference("someref")
#'   gml$setIdentifier("identifier", "codespace")
#'   gml$addName("name1", "codespace")
#'   gml$addName("name2", "codespace")
#'   gml$setQuantityTypeReference("someref")
#'   gml$setCatalogSymbol("symbol")
#' 
#' @references 
#'   ISO 19136:2007 Geographic Information -- Geographic Markup Language.
#'   http://www.iso.org/iso/iso_catalogue/catalogue_tc/catalogue_detail.htm?csnumber=32554 
#'   
#'   OGC Geography Markup Language. http://www.opengeospatial.org/standards/gml
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
GMLUnitDefinition <- R6Class("GMLUnitDefinition",
   inherit = GMLDefinition,
   private = list(
     xmlElement = "UnitDefinition",
     xmlNamespacePrefix = "GML"
   ),
   public = list(
     #+ quantityTypeReference [0..1]: character
     quantityTypeReference = NULL,
     #+ catalogSymbol [0..1]: character
     catalogSymbol = NULL,
     initialize = function(xml = NULL, defaults = list(), id = NULL){
       super$initialize(xml, defaults)
       if(is.null(xml)){
         self$setId(id, addNS = TRUE)
       }
     },
     
     #setQuantityTypeReference
     setQuantityTypeReference = function(quantityTypeReference){
       self$quantityTypeReference <- GMLElement$create("quantityTypeReference", href = quantityTypeReference)
     },
     
     #setCatalogSymbol
     setCatalogSymbol = function(catalogSymbol){
       self$catalogSymbol <- GMLElement$create("catalogSymbol", value = catalogSymbol)
     }
   )
)

GMLUnitDefinition$buildFrom = function(x, by = "symbol", unitsystem = "udunits2"){
   out <- NULL
   
   if(!requireNamespace("units", quietly = TRUE)) 
      stop("Package 'units' is required.")
   
   if(unitsystem == "udunits2"){
      units_df <- units::valid_udunits()
      unit <- units_df[units_df[,by] == x,]
      if(nrow(unit)==0) return(NULL)
      unitType <- unit$source_xml
      unitClass <- switch(unitType,
         "base" = GMLBaseUnit,
         "derived" = GMLDerivedUnit,
         "accepted" = GMLConventionalUnit,
         "common" = GMLConventionalUnit
      )
      unitBaseUri <- paste0("https://mmisw.org/ont/mmi/udunits2-", unitType)
      unitUri <- paste(unitBaseUri, unit$name_singular, sep="/")
      
      gmlunit <- unitClass$new()
      gmlunit$setIdentifier(unit$symbol, unitUri)
      gmlunit$setCatalogSymbol(unit$symbol)
      gmlunit$addName(unit$name_singular)
      if(unit$definition != "") gmlunit$setDescription(unit$definition)
      if(unitType == "base") gmlunit$setUnitsSystem(unitsystem)
      if(unitType != "base"){
         def <- unit$def
         def_unit <-  gsub("\\.", " ", gsub("/", " ",gsub("\\^", "-", def)))
         def_unit_obj <- units::as_units(def_unit, implicit_exponents = TRUE)
         def_unit_attrs <- attributes(def_unit_obj)$units
         
         if(unitType == "derived"){
            if(length(def_unit_attrs$denominator)>0){
               denominators <- as.list(table(def_unit_attrs$denominator))
               for(uom in names(denominators)){
                  gmlunit$addDerivationUnitTerm(uom, exponent = denominators[[uom]])
               }
            }
         }else{
            base_units_df <- units_df[units_df$source_xml == "base",]
            def <- unit$def
            def_unit <-  gsub("\\.", " ", gsub("/", " ",gsub("\\^", "-", def)))
            def_unit_comps <- unlist(strsplit(def_unit, " "))
            base_unit <- base_units_df[base_units_df$symbol %in% def_unit_comps,]
            if(nrow(base_unit)>0){
               uom = as.character(base_unit[1L,]$symbol)
               gmlunit$setConversionToPreferredUnit(
                  uom = uom,
                  factor = def_unit_comps[def_unit_comps!= uom][1]
               )
            }
         }
      }
      
      out <- gmlunit
      
   }
   return(out)
}