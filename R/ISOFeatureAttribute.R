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
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureAttribute
#'  }
#'  \item{\code{setCode(code)}}{
#'    Sets the code
#'  }
#'  \item{\code{setValueMeasurementUnit(uom)}}{
#'    Sets the value measurement unit
#'  }
#'  \item{\code{setValueType(typeName)}}{
#'    Sets the value type
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
#'   uom <- ISOUomLength$new()
#'   uom$setUomName("Meter")
#'   uom$setUomSymbol("m")
#'   md$setValueMeasurementUnit(uom)
#'   
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
     #+ valueMeasurementUnit [0..1]: ISOUnitOfMeasure
     valueMeasurementUnit = NULL,
     #+ valueType [0..1]: ISOTypeName
     valueType = NULL,
     
     initialize = function(xml = NULL){
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
       )
     },
     
     #setCode
     setCode = function(code){
       self$code <- code
     },
     
     #setValueMeasurementUnit
     setValueMeasurementUnit = function(uom){
       if(!is(uom, "ISOUnitOfMeasure")){
         stop("The argument should be an object of class 'ISOUnitOfMeasure")
       } 
       self$valueMeasurementUnit <- uom
     },
     
     #setValueType
     setValueType = function(typeName){
       if(!is(typeName, "ISOTypeName")){
         tn <- ISOTypeName$new()
         tn$setName(typeName)
         typeName <- tn
       }
       self$valueType <- typeName
     }
   )         
)