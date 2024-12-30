#' SWEAbstractDataComponent
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link[R6]{R6Class}} for modelling an SWE Abstract data component
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEAbstractDataComponent <- R6Class("SWEAbstractDataComponent",
  inherit = SWEAbstractSWEIdentifiable,
  private = list(
    xmlElement = "AbstractDataComponent",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(

    #'@field name name
    name = list(),
    
    #'@description Initializes an object of class \link{SWEAbstractDataComponent}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param element element
    #'@param updatable updatable
    #'@param optional optional
    #'@param definition definition
    initialize = function(xml = NULL, element = NULL, updatable = NULL, optional = FALSE, definition = NULL){
      if(is.null(element)) element <- private$xmlElement
      super$initialize(xml, element = element, attrs = list(), defaults = list(), wrap = TRUE, value_as_field = TRUE)
      if(!is.null(updatable)) if(is.logical(updatable)) self$setAttr("updatable", tolower(updatable))
      self$setAttr("optional", tolower(optional))
      if(!is.null(definition)) self$setAttr("definition", definition)
    },
    
    #'@description Adds name
    #'@param name name
    #'@param codeSpace codespace
    addName = function(name, codeSpace = NULL){
      name <- GMLCodeType$new(value = name, codeSpace = codeSpace)
      return(self$addListElement("name", name))
    },
    
    #'@description Deletes name
    #'@param name name
    #'@param codeSpace codespace
    delName = function(name, codeSpace = NULL){
      name <- GMLCodeType$new(value = name, codeSpace = codeSpace)
      return(self$delListElement("name", name))
    }
  )                        
)
