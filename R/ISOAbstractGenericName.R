#' ISOAbstractGenericName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract generic name
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract GenericName
#' @format \code{\link[R6]{R6Class}} object.
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
    #'@field value value
    value = NA,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    #'@param codeSpace code space
    initialize = function(xml = NULL, value = NULL, codeSpace = NULL){
      super$initialize(xml = xml, value = value, codeSpace = codeSpace)
    }
  )                        
)
