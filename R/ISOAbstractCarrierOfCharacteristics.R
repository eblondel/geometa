#' ISOAbstractCarrierOfCharacteristics
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract carrierOfCharacteristics
#' @return Object of \code{\link{R6Class}} for modelling an abstract ISOCarrierOfCharacteristics
#' @format \code{\link{R6Class}} object.
#'
#' @field featureType [\code{\link{ISOFeatureType}}]
#' @field constrainedBy [\code{\link{ISOConstraint}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an \code{\link{ISOAbstractCarrierOfCharacteristics}}
#'  }
#'  \item{\code{setFeatureType(featureType)}}{
#'    Set a feature type, object of class \code{\link{ISOFeatureType}}
#'  }
#'  \item{\code{addConstraint(constraint)}}{
#'    Add constraint, object of class \code{\link{ISOConstraint}}
#'  }
#'  \item{\code{delConstraint(constraint)}}{
#'    Deletes constraint, object of class \code{\link{ISOConstraint}}
#'  }
#' }
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
     
     #+ featureType [0..1]: ISOFeatureType
     featureType = NULL,
     #+ constrainedBy [0..*]: ISOConstraint
     constrainedBy = list(),
     
     initialize = function(xml = NULL, defaults = NULL){
       super$initialize(xml = xml, defaults = defaults)
     },
     
     #setFeatureType
     setFeatureType = function(featureType){
       if(!is(featureType, "ISOFeatureType")){
         stop("The argument should be an object of class 'ISOFeatureType'")
       }
       self$featureType <- featureType
     },
     
     #addConstraint
     addConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$addListElement("constrainedBy", constraint))
     },
     
     #delConstraint
     delConstraint = function(constraint){
       if(!is(constraint, "ISOConstraint")){
         constraint <- ISOConstraint$new(description = constraint)
       }
       return(self$delListElement("constrainedBy", constraint))
     }
   )         
)