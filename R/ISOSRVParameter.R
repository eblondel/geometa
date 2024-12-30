#' ISOSRVParameter
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO parameter
#' @return Object of \code{\link{R6Class}} for modelling an ISOSRVParameter
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   md <- ISOSRVParameter$new()
#'   md$setName("name", "attType")
#'   md$setDirection("in")
#'   md$setDescription("description")
#'   md$setOptionality(FALSE)
#'   md$setRepeatability(FALSE)
#'   md$setValueType("CharacterString")  
#'   xml <- md$encode()
#' 
#' @references 
#'   - ISO 19119 \url{https://schemas.isotc211.org/19119/srv/srv/#element_SV_Parameter}

#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/srv/2.0/srv/#element_SV_Parameter}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSRVParameter <- R6Class("ISOSRVParameter",
     inherit = ISOAbstractObject,
     private = list(
       xmlElement = "SV_Parameter",
       xmlNamespacePrefix = list(
         "19139" = "SRV",
         "19115-3" = "SRV"
       )
     ),
     public = list(
       
       #'@field name name [1..1]: character
       name = NULL,
       #'@field direction direction [0..1]: ISOParameterDirection or character
       direction = NULL,
       #'@field description description [0..1]: character
       description = NULL,
       #'@field optionality optionality [1..1]: character
       optionality = "Mandatory",
       #'@field repeatability repeatability [1..1]: logical
       repeatability = FALSE,
       #'@field valueType valueType [1..1]: ISOTypeName (=> ISO 19139)
       valueType = NULL,
       
       #'@description Initializes object
       #'@param xml object of class \link{XMLInternalNode-class}
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
       #'@param direction object of class \link{ISOSRVParameterDirection} or \link{character}
       #'  among values returned by \code{ISOSRVParameterDirection$values()}
       setDirection = function(direction){
         if(!is(direction, "ISOSRVParameterDirection")){
           direction <- ISOSRVParameterDirection$new(value = direction)
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
         self$optionality <- ifelse(optional, "Optional", "Mandatory")
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
       #'@param valueType object of class \link{ISOTypeName} or \link{character}
       #'@param locales list of localized texts. Default is \code{NULL}
       setValueType = function(valueType, locales = NULL){
         if(!is(valueType, "ISOTypeName")){
           typeName <- ISOTypeName$new()
           typeName$setName(valueType)
           if(!is.null(locales)){
             typeName$setName(valueType, locales = locales)
           }
           valueType <- typeName
         }
         self$valueType <- valueType
       }
     )                        
)
