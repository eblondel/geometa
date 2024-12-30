#' ISOScope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO scope
#' @return Object of \code{\link[R6]{R6Class}} for modelling a scope
#' @format \code{\link[R6]{R6Class}} object.
#'    
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mco/1.0/mco/#element_MD_Scope}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScope <- R6Class("ISOScope",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_Scope",
    xmlNamespacePrefix = list(
      "19115-3" = "MCC"
    )
  ),
  public = list(
    
    #'@field level level [0..1]: ISOScope
    level = list(),
    #'@field extent extent [0..*]: ISOAbstractExtent
    extent = NULL,
    #'@field levelDescription levelDescription [0..*]: ISOScopeDescription
    levelDescription = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set level
    #'@param level scope code, object of class \link{ISOScopeCode} or \link{character} among values
    #' listed by \code{ISOScopeCode$values()}
    setLevel = function(level){
      if(!is(level, "ISOScopeCode")){
        if(is(level, "character")){
          level <- ISOScopeCode$new(value = level)
        }else{
          stop("The 'level' argument should be an object of class 'character' or 'ISOScopeCode'")
        }
      }
      self$level = level
    },
    
    #'@description Adds extent
    #'@param extent extent of class \link{ISOAbstractExtent}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addExtent = function(extent){
      if(!is(extent, "ISOAbstractExtent")){
        stop("The argument should be an object inheriting class 'ISOAbstractExtent'")
      }
      return(self$addListElement("extent", extent))
    },
    
    #'@description Deletes extent
    #'@param extent extent of class \link{ISOAbstractExtent}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delExtent = function(extent){
      if(!is(extent, "ISOAbstractExtent")){
        stop("The argument should be an object inheriting class 'ISOAbstractExtent'")
      }
      return(self$delListElement("extent", extent))
    },
    
    #'@description Adds level description
    #'@param levelDescription levelDescription of class \link{ISOScopeDescription}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addLevelDescription = function(levelDescription){
      if(!is(levelDescription, "ISOScopeDescription")){
        stop("The argument should be an object of class 'ISOScopeDescription'")
      }
      return(self$addListElement("levelDescription", levelDescription))
    },
    
    #'@description Deletes level description
    #'@param levelDescription levelDescription of class \link{ISOScopeDescription}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delLevelDescription = function(levelDescription){
      if(!is(levelDescription, "ISOScopeDescription")){
        stop("The argument should be an object of class 'ISOScopeDescription'")
      }
      return(self$delListElement("levelDescription", levelDescription))
    }
    
  )                        
)
