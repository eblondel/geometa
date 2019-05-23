#' ISOScopedName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO scoped name
#' @return Object of \code{\link{R6Class}} for modelling an ISO ScopedName
#' @format \code{\link{R6Class}} object.
#'
#' @field value [\code{\link{character}}] scope name
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an \code{\link{ISOScopedName}}
#'  }
#' }
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
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(xml = xml, value = value)
    }
  )                        
)