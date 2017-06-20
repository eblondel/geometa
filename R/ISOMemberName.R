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
#'    Set the aName, object of class \code{character}
#'  }
#'  \item{\code{setAttributeType(attributeType)}}{
#'    Set the attribute type, object of class \code{ISOTypeName} or \code{character}
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
     initialize = function(xml = NULL, aName, attributeType){
       super$initialize(xml = xml)
       if(is.null(xml)){
         self$setName(aName)
         self$setAttributeType(attributeType)
       }
     },
     
     #setName
     setName = function(aName){
       self$aName <- aName
     },
     
     #setAttributeType
     setAttributeType = function(attributeType){
       if(!is(attributeType, "ISOTypeName")){
         attributeType <- ISOTypeName$new(aName = attributeType)
       }
       self$attributeType <- attributeType
     }
   )                        
)