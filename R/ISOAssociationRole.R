#' ISOAssociationRole
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO association role
#' @return Object of \code{\link{R6Class}} for modelling an ISOAssociationRole
#' @format \code{\link{R6Class}} object.
#'
#' @field type [\code{\link{ISORoleType}}]
#' @field isOrdered [\code{\link{logical}}]
#' @field isNavigable [\code{\link{logical}}]
#' @field relation [\code{\link{ISOFeatureAssociation}}]
#' @field rolePlayer [\code{\link{ISOFeatureType}}]
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOAssociationRole}}
#'  }
#'  \item{\code{setRoleType(roleType)}}{
#'    Sets the role type, object of class \code{\link{ISORoleType}} or any \code{character}
#'    value among \code{ISORoleType$values()}.
#'  }
#'  \item{\code{setIsOrdered(isOrdered)}}{
#'    Sets \code{TRUE} if ordered, \code{FALSE} otherwise
#'  }
#'  \item{\code{setIsNavigable(isNavigable)}}{
#'    Sets \code{TRUE} if navigable, \code{FALSE} otherwise
#'  }
#'  \item{code{setRelation(relation)}}{
#'    Sets an object of class \code{\link{ISOFeatureAssocation}} as relation
#'  }
#'  \item{\code{addRolePlayer(rolePlayer)}}{
#'    Adds a role player, object of class \code{\link{ISOFeatureType}}
#'  }
#'  \item{\code{delRolePlayer(rolePlayer)}}{
#'    Deletes a role player, object of class \code{\link{ISOFeatureType}}
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
      
      #+ type: ISORoleType
      type = NULL,
      #+ isOrdered: logical
      isOrdered = NULL,
      #+ isNavigable: logical
      isNavigable = NULL,
      #+ relation: ISOAssociationRole
      relation = NA,
      #+ rolePlayer: ISOFeatureType
      rolePlayer = list(),
      
      initialize = function(xml = NULL){
        defaults = list(type = ISORoleType$new(value = "ordinary"))
        super$initialize(xml = xml, defaults = defaults)
      },
      
      #setRoleType
      setRoleType = function(roleType){
        if(!is(roleType, "ISORoleType")){
          if(is(roleType, "character")){
            roleType <- ISORoleType$new(value = roleType)
          }else{
            stop("The argument value should be an object of class 'ISORoleType' or 'character'")
          }
        }
        self$type <- roleType
      },
      
      #setIsOrdered
      setIsOrdered = function(isOrdered){
        if(!is(isOrdered, "logical")){
          isOrdered <- as.logical(isOrdered)
          if(is.na(isOrdered)){
            stop("The argument should be 'logical' or coercable as 'logical'")
          }
        }
        self$isOrdered <- isOrdered
      },
      
      #setIsNavigable
      setIsNavigable = function(isNavigable){
        if(!is(isNavigable, "logical")){
          isNavigable <- as.logical(isNavigable)
          if(is.na(isNavigable)){
            stop("The argument should be 'logical' or coercable as 'logical'")
          }
        }
        self$isNavigable <- isNavigable
      },
      
      #setRelation
      setRelation = function(relation){
        if(!is(relation, "ISOAssociationRole")){
          stop("The argument value should be an object of class 'ISOAssociationRole")
        }
        self$relation <- relation
      },
      
      #addRolePlayer
      addRolePlayer = function(rolePlayer){
        if(!is(rolePlayer, "ISOFeatureType")){
          stop("The argument value should be an object of class (ISOFeatureType")
        }
        return(self$addListElement("rolePlayer", rolePlayer))
      },
      
      #delRolePlayer
      delRolePlayer = function(rolePlayer){
        if(!is(rolePlayer, "ISOFeatureType")){
          stop("The argument value should be an object of class (ISOFeatureType")
        }
        return(self$delListElement("rolePlayer", rolePlayer))
      }
    )         
)