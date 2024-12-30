#' ISOURL
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO URL
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOURL
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOURL <- R6Class("ISOURL",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "URL",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$value = value
      }
    },
    
    #'@description Set URL
    #'@param url url
    setUrl = function(url){
      self$value <- url
    }
  )                        
)
