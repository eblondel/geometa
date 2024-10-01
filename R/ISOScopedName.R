#' ISOScopedName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO scoped name
#' @return Object of \code{\link{R6Class}} for modelling an ISO ScopedName
#' @format \code{\link{R6Class}} object.
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOScopedName <- R6Class("ISOScopedName",
  inherit = ISOAbstractGenericName,
  private = list(
    xmlElement = "ScopedName",
    xmlNamespacePrefix = list(
      "19139" = "GCO",
      "19115-3" = "GCO"
    )
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value){
      super$initialize(xml = xml, value = value)
    }
  )                        
)