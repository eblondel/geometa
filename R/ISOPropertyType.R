#' ISOPropertyType
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO property type
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOPropertyType
#' @format \code{\link[R6]{R6Class}} object.
#'  
#' @references 
#'   ISO 19110:2005 Methodology for Feature cataloguing
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOPropertyType <- R6Class("ISOPropertyType",
    inherit = ISOAbstractPropertyType,
    private = list(
      xmlElement = "FC_PropertyType",
      xmlNamespacePrefix = "GFC"
    ),
    public = list(
      
      #'@description Initializes object
      #'@param xml object of class \link[XML]{XMLInternalNode-class}
      #'@param defaults default values
      initialize = function(xml = NULL, defaults = NULL){
        if(is.null(defaults)) defaults <- list(cardinality = ISOMultiplicity$new(lower=1L,upper=1L))
        super$initialize(xml = xml, defaults = defaults)
      }
    )         
)
