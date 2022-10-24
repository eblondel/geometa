#' ISOAbstractPropertyType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract property type
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractPropertyType
#' @format \code{\link{R6Class}} object.
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractPropertyType <- R6Class("ISOAbstractPropertyType",
   inherit = ISOAbstractCarrierOfCharacteristics,
   private = list(
     xmlElement = "AbstractFC_PropertyType",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #'@field memberName typeName [1..1]: ISOLocalName
     memberName = NULL,
     #'@field definition definition [0..1]: character
     definition = NULL,
     #'@field cardinality cardinality [1..1]: ISOMultiplicity
     cardinality = NULL,
     #'@field definitionReference definitionReference [0..1]
     definitionReference = NULL,
     #'@field featureCatalogue featureCatalogue [0..1]
     featureCatalogue = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param defaults default values
     initialize = function(xml = NULL, defaults = NULL){
       if(is.null(defaults)) defaults <- list(cardinality = ISOMultiplicity$new(lower=1L,upper=1L))
       super$initialize(xml = xml, defaults = defaults)
     },
     
     #'@description Set member name
     #'@param memberName member name object of class \link{character} or \link{ISOLocalName}
     setMemberName = function(memberName){
       if(!is(memberName,"ISOLocalName")) memberName <- ISOLocalName$new(value = memberName)
       self$memberName <- memberName
     },
     
     #'@description Set definition
     #'@param definition definition
     #'@param locales locale definitions, as \link{list}
     setDefinition = function(definition, locales = NULL){
       self$definition <- definition
       if(!is.null(locales)){
         self$definition <- self$createLocalisedProperty(definition, locales)
       }
     },
     
     #'@description Set cardinality
     #'@param lower lower
     #'@param upper upper
     setCardinality = function(lower, upper){
       self$cardinality = ISOMultiplicity$new(lower = lower, upper = upper)
     },
     
     #'@description Set definition reference
     #'@param definitionReference object of class \link{ISODefinitionReference}
     setDefinitionReference = function(definitionReference){
       if(!is(definitionReference, "ISODefinitionReference")){
         stop("The argument should be an object of class 'ISODefinitionReference'")
       }
       self$definitionReference = definitionReference
     },
     
     #'@description Set feature catalogue
     #'@param featureCatalogue object of class \link{ISOFeatureCatalogue}
     setFeatureCatalogue = function(featureCatalogue){
       if(!is(featureCatalogue, "ISOFeatureCatalogue")){
         stop("The argument should be an object of class 'ISOFeatureCatalogue'")
       }
       self$featureCatalogue <- featureCatalogue
     }
   )         
)
