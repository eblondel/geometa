#' ISOCarrierOfCharacteristics
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO carrierOfCharacteristics
#' @return Object of \code{\link{R6Class}} for modelling an ISOCarrierOfCharacteristics
#' @format \code{\link{R6Class}} object.
#'
#' @section Methods inherited from \code{\link{ISOAbstractCarrierOfCharacteristics}}:
#' \describe{
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
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an \code{\link{ISOCarrierOfCharacteristics}}
#'  }
#' }
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCarrierOfCharacteristics <- R6Class("ISOCarrierOfCharacteristics",
   inherit = ISOAbstractCarrierOfCharacteristics,
   private = list(
     xmlElement = "FC_CarrierOfCharacteristics",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     initialize = function(xml = NULL, defaults = NULL){
       super$initialize(xml = xml, defaults = defaults)
     }
   )         
)