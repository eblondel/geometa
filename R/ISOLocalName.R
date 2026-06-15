#' ISOLocalName
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO local name
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO LocalName
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLocalName <- R6Class("ISOLocalName",
    inherit = ISOAbstractGenericName,
    private = list(
      xmlElement = "LocalName",
      xmlNamespacePrefix = list(
        "19139" = "GCO"
      )
    ),
    public = list(
      #'@field value value
      value = NA,
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      #'@param value value
      initialize = function(xml = NULL, value = NULL){
        super$initialize(xml = xml, value = value)
      }
    )                        
)
