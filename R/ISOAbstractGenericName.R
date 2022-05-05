#' ISOAbstractGenericName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract generic name
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract GenericName
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
ISOAbstractGenericName <- R6Class("ISOAbstractGenericName",
  inherit = GMLCodeType,
  private = list(
    xmlElement = "AbstractGenericName",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    value = NA,
    initialize = function(xml = NULL, value, codeSpace = NULL){
      super$initialize(xml = NULL, value, codeSpace = NULL)
    }
  )                        
)