#' ISOAssociationRole
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO association role
#' @return Object of \code{\link{R6Class}} for modelling an ISOAssociationRole
#' @format \code{\link{R6Class}} object.
#'
#' @field type
#' @field isOrdered
#' @field isNavigable
#' @field relation
#' @field valueType
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAssociationRole
#'  }
#'  \item{\code{setType(type)}}{
#'   Set the type of association
#'  }
#'  \item{\code{setIsOrdered(isOrdered)}}{
#'   Sets TRUE/FALSE if ordered
#'  }
#'  \item{\code{setIsNavigable(isNavigable)}}{
#'   Sets TRUE/FALSE if navigable
#'  }
#'  \item{\code{setRelation(relation)}}{
#'   Sets the relation, objec of class \code{ISOFeatureAssociation}
#'  }
#'  \item{\code{setValueType(valueType)}}{
#'   Sets a featureType, object of class \code{ISOFeatureType}
#'  }
#' }
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAssociationRole <- R6Class("ISOAssociationRole",
   inherit = ISOPropertyType,
   private = list(
     xmlElement = "FC_AssociationRole",
     xmlNamespacePrefix = "GFC"
   ),
   public = list(
     
     #+ type: ?
     type = NULL,
     #+ isOrdered: logical
     isOrdered = NULL,
     #+ isNavigable: logical
     isNavigable = NULL,
     #+ relation: ISOFeatureAssociation
     relation = NULL,
     #+ valueType: ISOFeatureType
     valueType = NULL,
     
     initialize = function(xml = NULL){
       defaults = list(cardinality = ISOMultiplicityRange(lower = 0, upper = Inf))
       super$initialize(
         xml = xml,
         element = private$xmlElement,
         namespace = getISOMetadataNamespace(private$xmlNamespacePrefix),
         defaults = defaults
       )
     },
     
     #setType
     setType = function(type){
       
     },
     
     #setIsOrdered
     setIsOrdered = function(isOrdered){
       if(!is(isOrdered, "logical")){
         isOrdered <- as.logical(isOrdered)
         if(is.na(isOrdered)){
           stop("Value cannot be coerced to 'logical'")
         }
       }
       self$isOrdered <- isOrdered
     },
     
     #setIsNavigable
     setIsNavigable = function(isNavigable){
       if(!is(isNavigable, "logical")){
         isNavigable <- as.logical(isNavigable)
         if(is.na(isNavigable)){
           stop("Value cannot be coerced to 'logical'")
         }
       }
       self$isNavigable <- isNavigable
     },
     
     #setRelation
     setRelation = function(featureAssociation){
       if(!is(featureAssociation, "ISOFeatureAssociation")){
         stop("The argument should be an object of class 'ISOFeatureAssociation")
       }
       self$relation <- featureAssociation
     },
     
     #setValueType
     setValueType = function(featureType){
       if(!is(featureType, "ISOFeatureType")){
         stop("The argument should be an object of class 'ISOFeatureType")
       }
       self$valueType <- featureType
     }
   )         
)