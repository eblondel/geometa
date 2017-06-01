#' ISOBaseInteger
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO integer
#' @return Object of \code{\link{R6Class}} for modelling an ISO Integer
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseInteger
#'  }
#' }
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseInteger <- R6Class("ISOBaseInteger",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "Integer",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
      if(is.null(xml)){
        if(!is(value, "integer")){
          value <- as.integer(value)
        }
        self$value = value
      }
    }
  )                        
)