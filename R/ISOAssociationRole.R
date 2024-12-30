#' ISOAssociationRole
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO association role
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAssociationRole
#' @format \code{\link[R6]{R6Class}} object.
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
      
      #'@field type type: ISORoleType
      type = NULL,
      #'@field isOrdered isOrdered: logical
      isOrdered = NULL,
      #'@field isNavigable isNavigable: logical
      isNavigable = NULL,
      #'@field relation relation: ISOAssociationRole
      relation = NA,
      #'@field rolePlayer rolePlayer: ISOFeatureType
      rolePlayer = list(),
      
      #'@description Initializes object
      #'@param xml object of class \link{XMLInternalNode-class}
      initialize = function(xml = NULL){
        defaults = list(type = ISORoleType$new(value = "ordinary"))
        super$initialize(xml = xml, defaults = defaults)
      },
      
      #'@description Set role type
      #'@param roleType role type, object of class \link{ISORoleType} or any \link{character} among
      #' values returned by \code{ISORoleType$values()}
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
      
      #'@description Set is ordered
      #'@param isOrdered object of class \link{logical}
      setIsOrdered = function(isOrdered){
        if(!is(isOrdered, "logical")){
          isOrdered <- as.logical(isOrdered)
          if(is.na(isOrdered)){
            stop("The argument should be 'logical' or coercable as 'logical'")
          }
        }
        self$isOrdered <- isOrdered
      },
      
      #'@description Set is navigable
      #'@param isNavigable object of class \link{logical}
      setIsNavigable = function(isNavigable){
        if(!is(isNavigable, "logical")){
          isNavigable <- as.logical(isNavigable)
          if(is.na(isNavigable)){
            stop("The argument should be 'logical' or coercable as 'logical'")
          }
        }
        self$isNavigable <- isNavigable
      },
      
      #'@description Set relation
      #'@param relation relation
      setRelation = function(relation){
        if(!is(relation, "ISOAssociationRole")){
          stop("The argument value should be an object of class 'ISOAssociationRole")
        }
        self$relation <- relation
      },
      
      #'@description Adds role player
      #'@param rolePlayer object of class \link{ISOFeatureType}
      #'@return \code{TRUE} if added, \code{FALSE} otherwise
      addRolePlayer = function(rolePlayer){
        if(!is(rolePlayer, "ISOFeatureType")){
          stop("The argument value should be an object of class (ISOFeatureType")
        }
        return(self$addListElement("rolePlayer", rolePlayer))
      },
      
      #'@description Deletes role player
      #'@param rolePlayer object of class \link{ISOFeatureType}
      #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
      delRolePlayer = function(rolePlayer){
        if(!is(rolePlayer, "ISOFeatureType")){
          stop("The argument value should be an object of class (ISOFeatureType")
        }
        return(self$delListElement("rolePlayer", rolePlayer))
      }
    )         
)
