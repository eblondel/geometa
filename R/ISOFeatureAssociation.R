#' ISOFeatureAssociation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO feature association
#' @return Object of \code{\link{R6Class}} for modelling an ISOFeatureAssociation
#' @format \code{\link{R6Class}} object.
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
     
     #'@field roleName roleName [2..*]: ISOAssociationRole
     roleName = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds role name
     #'@param associationRole object of class \link{ISOAssociationRole}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addRoleName = function(associationRole){
       if(!is(associationRole, "ISOAssociationRole")){
         stop("The argument value should be an object of class 'ISOAssocationRole'")
       }
       return(self$addListElement("roleName", associationRole))
     },
     
     #'@description Deletes role name
     #'@param associationRole object of class \link{ISOAssociationRole}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delRoleName = function(associationRole){
       if(!is(associationRole, "ISOAssociationRole")){
         stop("The argument value should be an object of class 'ISOAssocationRole'")
       }
       return(self$delListElement("roleName", associationRole))
     }

   )         
)