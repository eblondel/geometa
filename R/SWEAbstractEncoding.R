#' SWEAbstractEncoding
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO SWE
#' @return Object of \code{\link{R6Class}} for modelling an SWE abstract encoding object
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   SWE Common Data Model Encoding Standard. https://www.ogc.org/standards/swecommon
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
SWEAbstractEncoding <- R6Class("SWEAbstractEncoding",
  inherit = SWEAbstractSWE,
  private = list(
    xmlElement = "AbstractEncoding",
    xmlNamespacePrefix = "SWE"
  ),
  public = list(
    
    #'@description Initializes a SWE Nil Values object
    #'@param xml object of class \link{XMLInternalNode-class} from \pkg{XML}
    initialize = function(xml = NULL){
      super$initialize(xml, element = private$xmlElement, 
                       attrs = list(), defaults = list(),
                       wrap = TRUE)
    }
  )                        
)