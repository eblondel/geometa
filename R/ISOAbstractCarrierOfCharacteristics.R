#' ISOAbstractCarrierOfCharacteristics
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract carrierOfCharacteristics
#' @return Object of \code{\link[R6]{R6Class}} for modelling an abstract ISOCarrierOfCharacteristics
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note abstract class
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractCarrierOfCharacteristics <- R6Class("ISOAbstractCarrierOfCharacteristics",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractFC_CarrierOfCharacteristics",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #'@field featureType featureType [0..1]: ISOFeatureType
     featureType = NULL,
     #'@field constrainedBy constrainedBy [0..*]: ISOConstraint
     constrainedBy = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     #'@param defaults default values
     initialize = function(xml = NULL, defaults = NULL){
       super$initialize(xml = xml, defaults = defaults)
     },
     
     #'@description Set feature type
     #'@param featureType feature type, object of class \link{ISOFeatureType}
     setFeatureType = function(featureType){
       if(!is(featureType, "ISOFeatureType")){
         stop("The argument should be an object of class 'ISOFeatureType'")
       }
       self$featureType <- featureType
     },
     
     #'@description Adds constraint
     #'@param constraint, object of class \link{ISOConstraint}
     #'@return \code{TRUE} if added, \link{FALSE} otherwise
     addConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$addListElement("constrainedBy", constraint))
     },
     
     #'@description Deletes constraint
     #'@param constraint, object of class \link{ISOConstraint}
     #'@return \code{TRUE} if deleted, \link{FALSE} otherwise
     delConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$delListElement("constrainedBy", constraint))
     }
   )         
)
