#' ISORecord
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO record
#' @return Object of \code{\link{R6Class}} for modelling an ISORecord
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISORecord
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORecord <- R6Class("ISORecord",
    inherit = ISOMetadataElement,
    private = list(
      xmlElement = "Record",
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
          self$value = value
        }
      }
    )                        
)