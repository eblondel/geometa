#' ISOMemberName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO record
#' @return Object of \code{\link{R6Class}} for modelling an ISOMemberName
#' @format \code{\link{R6Class}} object.
#'
#' @field aName
#' @field attributeType
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, aName, attributeType)}}{
#'    This method is used to instantiate an ISOMemberName
#'  }
#'  \item{\code{setName(aName)}}{
#'    Set the aName, object of class \code{character}. Locale names can be specified 
#'    as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setAttributeType(attributeType, locales)}}{
#'    Set the attribute type, object of class \code{ISOTypeName} or \code{character}
#'    Locale names can be specified as \code{list} with the \code{locales} argument.
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMemberName <- R6Class("ISOMemberName",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "MemberName",
     xmlNamespacePrefix = "GCO"
   ),
   public = list(
     aName = NULL,
     attributeType = NULL,
     initialize = function(xml = NULL, aName = NULL, attributeType = NULL){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$setName(aName)
         self$setAttributeType(attributeType)
       }
     },
     
     #setName
     setName = function(aName, locales = NULL){
       self$aName <- aName
       if(!is.null(aName)){
         self$aName <- self$createLocalisedProperty(aName, locales)
       }
     },
     
     #setAttributeType
     setAttributeType = function(attributeType, locales = NULL){
       if(!is(attributeType, "ISOTypeName")){
         attrType <- ISOTypeName$new(aName = attributeType)
         if(!is.null(locales)){
           attrType <- ISOTypeName$new()
           attrType$setName(attributeType, locales)
         }
       }
       self$attributeType <- attrType
     }
   )                        
)