#' ISOBoundAssociationRole
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO BoundAssociationRole
#' @return Object of \code{\link{R6Class}} for modelling an ISOBoundAssociationRole
#' @format \code{\link{R6Class}} object.
#'
#' @field rolePlayer
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, defaults)}}{
#'    This method is used to instantiate an ISOBoundAssociationRole
#'  }
#'  \item{\code{setFeatureType(featureType)}}{
#'    Set feature type, object of class \code{ISOFeatureType}
#'  }
#' }
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBoundAssociationRole <- R6Class("ISOBoundAssociationRole",
    inherit = ISOBinding,
    private = list(
      xmlElement = "FC_BoundAssociationRole",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      
      #+ rolePlayer [0..1]: ISOFeatureType
      rolePlayer = NULL,
      
      #setRolePlayer
      setRolePlayer = function(rolePlayer){
        if(!is(rolePlayer, "ISOFeatureType")){
          stop("The argument value should be an object of class 'ISOFeatureType'")
        }
        self$rolePlayer <- rolePlayer
      }
      
    )         
)