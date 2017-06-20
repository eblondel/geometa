#' ISOLocalName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO local name
#' @return Object of \code{\link{R6Class}} for modelling an ISO LocalName
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOLocalName
#'  }
#' }
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLocalName <- R6Class("ISOLocalName",
    inherit = ISOAbstractObject,
    private = list(
      xmlElement = "LocalName",
      xmlNamespacePrefix = "GCO"
    ),
    public = list(
      value = NA,
      initialize = function(xml = NULL, value){
        super$initialize(xml = xml)
        if(is.null(xml)){
          self$value = value
        }
      }
    )                        
)