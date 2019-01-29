#' ISOFeatureAttribute
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature operation
#' @return Object of \code{\link{R6Class}} for modelling an ISOFeatureAttribute
#' @format \code{\link{R6Class}} object.
#'
#' @field code
#' @field valueMeasurementUnit
#' @field valueType
#' @field listedValue
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureAttribute
#'  }
#'  \item{\code{setCode(code, locales)}}{
#'    Sets the code. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setValueMeasurementUnit(uom)}}{
#'    Sets the value measurement unit, an object of class \code{GMLUnitDefinition}
#'  }
#'  \item{\code{setValueType(typeName, locales)}}{
#'    Sets the value type. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{addListedValue(value)}}{
#'    Adds a listed value (object of class \code{ISOListedValue})
#'  }
#'  \item{\code{delListedValue(value)}}{
#'   Deletes a listed value (object of class \code{ISOListedValue})
#'  }
#' }
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
     
     #+ code [0..1]: character
     code = NULL,
     #+ valueMeasurementUnit [0..1]: GMLUnitDefinition
     valueMeasurementUnit = NA,
     #+ valueType [0..1]: ISOTypeName
     valueType = NULL,
     #+ listedValue [0..*]: ISOListedValue
     listedValue = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setCode
     setCode = function(code, locales = NULL){
       self$code <- code
       if(!is.null(locales)){
         self$code <- self$createLocalisedProperty(code, locales)
       }
     },
     
     #setValueMeasurementUnit
     setValueMeasurementUnit = function(uom){
       if(!is(uom, "GMLUnitDefinition")){
         stop("The argument should be an object of class 'GMLUnitDefinition")
       } 
       self$valueMeasurementUnit <- uom
     },
     
     #setValueType
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
     
     #addListedValue
     addListedValue = function(value){
       if(!is(value, "ISOListedValue")){
         stop("The parameter should be an object of class 'ISOListedValue'")
       }
       return(self$addListElement("listedValue", value))
     },
     
     #delListedValue
     delListedValue = function(value){
       if(!is(value, "ISOListedValue")){
         stop("The parameter should be an object of class 'ISOListedValue")
       }
       return(self$delListElement("listedValue", value))
     }
   )         
)
