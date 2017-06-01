#' ISOLength
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO measure length
#' @return Object of \code{\link{R6Class}} for modelling an ISO Length measure
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOLength measure
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLength <- R6Class("ISOLength",
    inherit = ISOMeasure,
    private = list(
      xmlElement = "Length",
      xmlNamespacePrefix = "GCO"
    ),
    public = list(
      initialize = function(xml = NULL, value, uom){
        super$initialize(
          xml = xml,
          element = private$xmlElement,
          namespace = getISOMetadataNamespace(private$xmlNamespacePrefix),
          value = value,
          uom = uom
        )
      }
    )                        
)