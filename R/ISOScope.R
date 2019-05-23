#' ISOScope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO Scope
#' @format \code{\link{R6Class}} object.
#'
#' @field level [\code{\link{ISOHierarchyLevel}}] the scope/hierarchy level
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOScope}}
#'  }
#'  \item{\code{setLevel(level)}}{
#'    Sets the scope level, object of class 'character' or \code{\link{ISOHierarchyLevel}}
#'  }
#' }
#' 
#' @examples 
#'   md <- ISOScope$new()
#'   md$setLevel("dataset")
#'   xml <- md$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScope <- R6Class("ISOScope",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "DQ_Scope",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     level = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setLevel
     setLevel = function(level){
       if(is(level, "character")){
         level <- ISOHierarchyLevel$new(value = level)
       }
       self$level <- level
     }
   )                        
)