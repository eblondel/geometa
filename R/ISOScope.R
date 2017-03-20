#' ISOScope
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO extent
#' @return Object of \code{\link{R6Class}} for modelling an ISO Scope
#' @format \code{\link{R6Class}} object.
#'
#' @field level
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOScope
#'  }
#'  \item{\code{setLevel(level)}}{
#'    Sets the scope level
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScope <- R6Class("ISOScope",
   inherit = ISOMetadataElement,
   public = list(
     level = NULL,
     initialize = function(xml = NULL){
       super$initialize(
         element = "DQ_Scope",
         namespace = ISOMetadataNamespace$GMD
       )
       if(!is.null(xml)){
         self$decode(xml)
       }
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