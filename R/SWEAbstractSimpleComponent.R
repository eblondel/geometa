#' SWEAbstractSimpleComponent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE Abstract simple component
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   OGC Geography Markup Language. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEAbstractSimpleComponent <- R6Class("SWEAbstractSimpleComponent",
  inherit = SWEAbstractDataComponent,
  private = list(
    xmlElement = "AbstractSimpleComponent",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@description Initializes an object of class \link{SWEAbstractSimpleComponent}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param element element
    #'@param updatable updatable
    #'@param optional optional
    #'@param definition definition
    initialize = function(xml = NULL, element = NULL, updatable = NULL, optional = FALSE, definition = NULL){
      if(is.null(element)) element <- private$xmlElement
      super$initialize(xml, element = element, updatable = updatable, optional = optional, definition = definition)
    }
  )                        
)