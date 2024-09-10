#' ISOMultiplicity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO multiplicity
#' @return Object of \code{\link{R6Class}} for modelling an ISOMultiplicity
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   md <- ISOMultiplicity$new(lower = 1, upper = Inf)
#'   xml <- md$encode()
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMultiplicity <- R6Class("ISOMultiplicity",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Multiplicity",
    xmlNamespacePrefix = list(
      "19115-1/2" = "GCO",
      "19115-3" = "GCO"
    )
  ),
  public = list(
    #'@field range range
    range = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param lower lower
    #'@param upper upper
    initialize = function(xml = NULL, lower, upper){
      super$initialize(xml = xml)
      if(is.null(xml)){
        self$range <- ISOMultiplicityRange$new(lower = lower, upper = upper)
      }
    }
  )                        
)