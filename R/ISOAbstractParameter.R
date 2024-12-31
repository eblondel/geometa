#' ISOAbstractParameter
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract parameter
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract parameter
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mcc/1.0/mcc/#element_Abstract_Parameter}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractParameter <- R6Class("ISOAbstractParameter",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "Abstract_Parameter",
     xmlNamespacePrefix = list(
       "19115-3" = "MCC"
     )
   ),
   public = list(
     
     #'@field name name [1..1]: character|ISOMemberName
     name = NULL,
     #'@field direction direction [0..1]: ISOParameterDirection
     direction = NULL,
     #'@field description description [0..1]: character
     description = NULL,
     #'@field optionality optionality [1..1]: logical
     optionality = FALSE,
     #'@field repeatability repeatability [1..1]: logical
     repeatability = FALSE,
     #'@field valueType valueType [1..1]: ISORecordType
     valueType = NULL,
     #'@field value value [0..*] : ISORecord
     value = list(),
     #'@field resource resource [0..*] : ISOSource
     resource = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set name
     #'@param name name
     #'@param attributeType attribute type
     #'@param locales list of localized texts. Default is \code{NULL}
     setName = function(name, attributeType, locales = NULL){
       if(!is(attributeType, "ISOMemberName")){
         attrType <- ISOMemberName$new()
         attrType$setName(attributeType)
         if(!is.null(locales)){
           attrType$setName(attributeType, locales = locales)
         }
         attributeType <- attrType
       }
       self$name <- ISOElementSequence$new(aName = name, attributeType = attributeType)
     },
     
     #'@description Set direction
     #'@param direction object of class \link{ISOParameterDirection} or \link{character}
     #'  among values returned by \code{ISOParameterDirection$values()}
     setDirection = function(direction){
       if(!is(direction, "ISOParameterDirection")){
         direction <- ISOParameterDirection$new(value = direction)
       }
       self$direction <- direction
     },
     
     #'@description Set description
     #'@param description description
     #'@param locales list of localized texts. Default is \code{NULL}
     setDescription = function(description, locales = NULL){
       self$description <- as.character(description)
       if(!is.null(locales)){
         self$description <- self$createLocalisedProperty(description, locales)
       }
     },
     
     #'@description Set optionality
     #'@param optional object of class \link{logical}
     setOptionality = function(optional){
       if(!is(optional, "logical")){
         optional <- as.logical(optional)
         if(is.na(optional)){
           stop("The argument value should be an object of class 'logical' or coercable to 'logical'")
         }
       }
       self$optionality <- optional
     },
     
     #'@description Set repeatability
     #'@param repeatable object of class \link{logical}
     setRepeatability = function(repeatable){
       if(!is(repeatable, "logical")){
         repeatable <- as.logical(repeatable)
         if(is.na(repeatable)){
           stop("The argument value should be an object of class 'logical' or coercable to 'logical'")
         }
       }
       self$repeatability <- repeatable
     },
     
     #'@description Set value type
     #'@param valueType object of class \link{ISORecordType}
     setValueType = function(valueType){
       if(!is(valueType, "ISORecordType")){
         valueType = ISORecordType$new(value = valueType)
       }
       self$valueType <- valueType
     },
     
     #'@description Adds value
     #'@param value object of class \link{ISORecord}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addValue = function(value){
       if(!is(value, "ISORecord")){
         value = ISORecord$new(value = value)
       }
       return(self$addListElement("value", value))
     },
     
     #'@description Deletes value
     #'@param value object of class \link{ISORecord}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delValue = function(value){
       if(!is(value, "ISORecord")){
         value = ISORecord$new(value = value)
       }
       return(self$delListElement("value", value))
     },
     
     #'@description Adds resource
     #'@param resource object of class \link{ISOSource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addResource = function(resource){
       if(!is(resource, "ISOSource")){
         stop("The argument should be an object of class 'ISOSource'")
       }
       return(self$addListElement("resource", resource))
     },
     
     
     #'@description Deletes resource
     #'@param resource object of class \link{ISOSource}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     delResource = function(resource){
       if(!is(resource, "ISOSource")){
         stop("The argument should be an object of class 'ISOSource'")
       }
       return(self$delListElement("resource", resource))
     }
   )                        
)
