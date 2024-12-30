#' ISOFeatureAttribute
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature operation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOFeatureAttribute
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOFeatureAttribute$new()
#'   md$setMemberName("name")
#'   md$setDefinition("definition")
#'   md$setCardinality(lower=1,upper=1)
#'   md$setCode("code")
#'   
#'   gml <- GMLBaseUnit$new(id = "ID")
#'   gml$setDescriptionReference("someref")
#'   gml$setIdentifier("identifier", "codespace")
#'   gml$addName("name1", "codespace")
#'   gml$addName("name2", "codespace")
#'   gml$setQuantityTypeReference("someref")
#'   gml$setCatalogSymbol("symbol")
#'   gml$setUnitsSystem("somelink")
#'   md$setValueMeasurementUnit(gml)
#'   
#'   val1 <- ISOListedValue$new()
#'   val1$setCode("code1")
#'   val1$setLabel("label1")
#'   val1$setDefinition("definition1")
#'   md$addListedValue(val1)
#'   val2 <- ISOListedValue$new()
#'   val2$setCode("code2")
#'   val2$setLabel("label2")
#'   val2$setDefinition("definition2")
#'   md$addListedValue(val2)
#'   md$setValueType("typeName")
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureAttribute <- R6Class("ISOFeatureAttribute",
   inherit = ISOPropertyType,
   private = list(
     xmlElement = "FC_FeatureAttribute",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #'@field code code [0..1]: character
     code = NULL,
     #'@field valueMeasurementUnit valueMeasurementUnit [0..1]: GMLUnitDefinition
     valueMeasurementUnit = NA,
     #'@field valueType valueType [0..1]: ISOTypeName
     valueType = NULL,
     #'@field listedValue listedValue [0..*]: ISOListedValue
     listedValue = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set code
     #'@param code code
     #'@param locales list of localized codes. Default is \code{NULL}
     setCode = function(code, locales = NULL){
       self$code <- code
       if(!is.null(locales)){
         self$code <- self$createLocalisedProperty(code, locales)
       }
     },
     
     #'@description Set value measurement unit
     #'@param uom uom, object of class \link{GMLUnitDefinition} (in ISO 19139) 
     #'or \link{ISOUomIdentifier} / \link{character} (in ISO 19115-3)
     setValueMeasurementUnit = function(uom){
       switch(getMetadataStandard(),
        "19139" = {
          if(!is(uom, "GMLUnitDefinition")){
            stop("The argument should be an object of class 'GMLUnitDefinition")
          } 
        },
        "19115-3" = {
          if(!is(uom, "ISOUomIdentifier")) uom = ISOUomIdentifier$new(value = uom)
        }
       )
       self$valueMeasurementUnit <- uom
     },
     
     #'@description Set type name
     #'@param typeName typeName
     #'@param locales list of localized typeNames. Default is \code{NULL}
     setValueType = function(typeName, locales = NULL){
       if(!is(typeName, "ISOTypeName")){
         tn <- ISOTypeName$new()
         tn$setName(typeName)
         if(!is.null(locales)){
           tn$setName(typeName, locales)
         }
         typeName <- tn
       }
       self$valueType <- typeName
     },
     
     #'@description Adds listed value
     #'@param value value, object of class \link{ISOListedValue}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addListedValue = function(value){
       if(!is(value, "ISOListedValue")){
         stop("The parameter should be an object of class 'ISOListedValue'")
       }
       return(self$addListElement("listedValue", value))
     },
     
     #'@description Deletes listed value
     #'@param value value, object of class \link{ISOListedValue}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delListedValue = function(value){
       if(!is(value, "ISOListedValue")){
         stop("The parameter should be an object of class 'ISOListedValue")
       }
       return(self$delListElement("listedValue", value))
     }
   )         
)
