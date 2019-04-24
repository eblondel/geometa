#' ISOURL
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO URL
#' @return Object of \code{\link{R6Class}} for modelling an ISOURL
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOURL
#'  }
#'  \item{\code{setUrl(url)}}{
#'    Set the url
#'  }
#' }
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
    value = NA,
    initialize = function(xml = NULL, value = NULL){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$value = value
      }
    },
    
    setUrl = function(url){
      self$value <- url
    }
  )                        
)
