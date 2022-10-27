#' ISOScope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO Scope
#' @format \code{\link{R6Class}} object.
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
     #'@field level level
     level = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}  
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set level
     #'@param level object of class \link{ISOHierarchyLevel} or any \link{character}
     #'  among values returned by \link{ISOHierarchyLevel}
     setLevel = function(level){
       if(is(level, "character")){
         level <- ISOHierarchyLevel$new(value = level)
       }
       self$level <- level
     }
   )                        
)