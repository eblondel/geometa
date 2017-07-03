#' ISOFeatureAssociation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature association
#' @return Object of \code{\link{R6Class}} for modelling an ISOFeatureAssociation
#' @format \code{\link{R6Class}} object.
#' 
#' @field roleName
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFeatureAssociation
#'  }
#'  \item{\code{addRoleName(associationRole)}}{
#'    Adds an association role, object of class \code{ISOAssociationRole}
#'  }
#'  \item{\code{delRoleName(associationRole)}}{
#'    Deletes an association role, object of class \code{ISOAssociationRole}
#'  }
#' }
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFeatureAssociation <- R6Class("ISOFeatureAssociation",
   inherit = ISOFeatureType,
   private = list(
     xmlElement = "FC_FeatureAssociation",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #+ roleName [2..*]: ISOAssociationRole
     roleName = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #addRoleName
     addRoleName = function(associationRole){
       if(!is(associationRole, "ISOAssociationRole")){
         stop("The argument value should be an object of class 'ISOAssocationRole'")
       }
       return(self$addListElement("roleName", associationRole))
     },
     
     #delRoleName
     delRoleName = function(associationRole){
       if(!is(associationRole, "ISOAssociationRole")){
         stop("The argument value should be an object of class 'ISOAssocationRole'")
       }
       return(self$delListElement("roleName", associationRole))
     }

   )         
)