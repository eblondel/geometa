#' SWECategory
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE Category
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWECategory <- R6Class("SWECategory",
   inherit = SWEAbstractSimpleComponent,
   private = list(
     xmlElement = "Category",
     xmlNamespacePrefix = "SWE"
   ),
   public = list(
     
     #'@field codeSpace codeSpace
     codeSpace = NULL,
     
     #'@field constraint constraint
     constraint = NULL,
     
     #'@field value value
     value = NA_character_,
     
     #'@description Initializes an object of class \link{SWECategory}
     #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
     #'@param codeSpace codeSpace
     #'@param constraint constraint
     #'@param value value
     #'@param updatable updatable
     #'@param optional optional
     #'@param definition definition
     initialize = function(xml = NULL, 
                           codeSpace = NULL, constraint = NULL, value = NULL,
                           updatable = NULL, optional = FALSE, definition = NULL){
       super$initialize(xml, element = private$xmlElement,
                        updatable = updatable, optional = optional, definition = definition)
       if(is.null(xml)){
         self$setCodeSpace(codeSpace)
         self$setConstraint(constraint)
         self$setValue(value)
       }
     },
     
     
     #'@description setCodeSpace
     #'@param codeSpace codeSpace
     setCodeSpace = function(codeSpace){
       self$codeSpace <- codeSpace
     },
     
     #'@description setConstraint
     #'@param constraint constraint
     setConstraint = function(constraint){
       self$constraint <- constraint
     },
     
     #'@description setValue
     #'@param value value
     setValue = function(value){
       if(!is.character(value)){
         stop("Value should be character")
       }
       self$value <- value
     }
   )                        
)