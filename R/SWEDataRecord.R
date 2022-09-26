#' SWEDataRecord
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE data record
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used internally by geometa
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEDataRecord <- R6Class("SWEDataRecord",
  inherit = SWEAbstractDataComponent,
  private = list(
    xmlElement = "DataRecord",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    #'@field field field
    field = list(),
    
    #'@description Initializes an object of class \link{SWEDataRecord}
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    #'@param element element
    #'@param updatable updatable
    #'@param optional optional
    #'@param definition definition
    initialize = function(xml = NULL, element = NULL, updatable = NULL, optional = FALSE, definition = NULL){
      if(is.null(element)) element <- private$xmlElement
      super$initialize(xml, element = element, updatable = updatable, optional = optional, definition = definition)
    },
    
    #'@description Adds field
    #'@param field field
    addField = function(field){
      return(self$addListElement("field", field))
    },
    
    #'@description Deletes field
    #'@param field field
    delField = function(field){
      return(self$delListElement("field", field))
    }
  )                        
)