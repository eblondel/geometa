#' SWECount
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE Count
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   OGC Geography Markup Language. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWECount <- R6Class("SWECount",
  inherit = SWEAbstractSimpleComponent,
  private = list(
    xmlElement = "Count",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@field constraint constraint
    constraint = NULL,
    
    #'@field value  value
    value = NULL,
    
    #'@description Initializes an object of class \link{SWECount}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param element element
    #'@param attrs attrs
    #'@param defaults defaults
    #'@param wrap wrap
    initialize = function(xml = NULL, constraint = NULL, value = NULL){
      super$initialize(xml, element = private$xmlElement)
      if(is.null(xml)){
        self$constraint <- constraint
        self$value <- value
      }
    },
    
    #'@description setConstraint
    #'@param constraint
    setConstraint = function(constraint){
      self$constraint <- constraint
    },
    
    #'@description setValue
    #'@param value
    setValue = function(value){
      self$value <- value
    }
  )                        
)