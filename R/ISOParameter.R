#' ISOParameter
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO parameter
#' @return Object of \code{\link{R6Class}} for modelling an ISOParameter
#' @format \code{\link{R6Class}} object.
#'
#' @field name
#' @field direction
#' @field description
#' @field optionality
#' @field repeatability
#' @field valueType
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOParameter
#'  }
#'  \item{\code{setName(name, attributeType)}}{
#'    Sets the parameter name (\code{character}) and attributeType (\code{ISOTypeName} 
#'    or \code{character})
#'  }
#'  \item{\code{setDirection(direction)}}{
#'    Sets the direction, an object of class \code{ISOParameterDirection} or any
#'    \code{character} value among \code{ISOParameterDirection$values()}
#'  }
#'  \item{\code{setDescription(description)}}{
#'    Sets the parameter description
#'  }
#'  \item{\code{setOptionality(optional)}}{
#'    Set whether the parameter is optional (\code{TRUE}), \code{FALSE} otherwise
#'  }
#'  \item{\code{setRepeatability(repeatable)}}{
#'    Set whether the parameter is repeatable (\code{TRUE}), \code{FALSE} otherwise
#'  }
#'  \item{\code{setValueType(valueType)}}{
#'    Sets the type of parameter value, object of class \code{ISOTypeName} or \code{character}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOParameter$new()
#'   md$setName("name", "attType")
#'   md$setDirection("in")
#'   md$setDescription("description")
#'   md$setOptionality(FALSE)
#'   md$setRepeatability(FALSE)
#'   md$setValueType("CharacterString")  
#'   xml <- md$encode()
#' 
#' @references 
#'   ISO 19119:2005 - Geographic information -- Services
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOParameter <- R6Class("ISOParameter",
     inherit = ISOAbstractObject,
     private = list(
       xmlElement = "SV_Parameter",
       xmlNamespacePrefix = "SRV"
     ),
     public = list(
       
       #+ name [1..1]: character
       name = NULL,
       #+ direction [0..1]: ISOParameterDirection
       direction = NULL,
       #+ description [0..1]: character
       description = NULL,
       #+ optionality [1..1]: character
       optionality = "Mandatory",
       #+ repeatability [1..1]: logical
       repeatability = FALSE,
       #+ valueType [1..1]: ISOTypeName
       valueType = NULL,
       
       initialize = function(xml = NULL){
         super$initialize(xml = xml)
       },
       
       #setName
       setName = function(name, attributeType){
         if(!is(attributeType, "ISOTypeName")){
           attrType <- ISOTypeName$new()
           attrType$setName(attributeType)
           attributeType <- attrType
         }
         self$name <- ISOElementSequence$new(aName = name, attributeType = attributeType)
       },
       
       #setDirection
       setDirection = function(direction){
         if(!is(direction, "ISOParameterDirection")){
           direction <- ISOParameterDirection$new(value = direction)
         }
         self$direction <- direction
       },
       
       #setDescription
       setDescription = function(description){
         self$description <- as.character(description)
       },
       
       #setOptionality
       setOptionality = function(optional){
         if(!is(optional, "logical")){
           optional <- as.logical(optional)
           if(is.na(optional)){
             stop("The argument value should be an object of class 'logical' or coercable to 'logical'")
           }
         }
         self$optionality <- ifelse(optional, "Optional", "Mandatory")
       },
       
       #setRepeatability
       setRepeatability = function(repeatable){
         if(!is(repeatable, "logical")){
           repeatable <- as.logical(repeatable)
           if(is.na(repeatable)){
             stop("The argument value should be an object of class 'logical' or coercable to 'logical'")
           }
         }
         self$repeatability <- repeatable
       },
       
       #setValueType
       setValueType = function(valueType){
         if(!is(valueType, "ISOTypeName")){
           typeName <- ISOTypeName$new()
           typeName$setName(valueType)
           valueType <- typeName
         }
         self$valueType <- valueType
       }
     )                        
)
