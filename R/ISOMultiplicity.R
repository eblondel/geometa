#' ISOMultiplicity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO multiplicity
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOMultiplicity
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   md <- ISOMultiplicity$new(lower = 1, upper = Inf)
#'   xml <- md$encode()
#' 
#' @references
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gco/1.0/gco/#element_Multiplicity}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/gco/1.0/gco/#element_Multiplicity}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMultiplicity <- R6Class("ISOMultiplicity",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Multiplicity",
    xmlNamespacePrefix = list(
      "19139" = "GCO",
      "19115-3" = "GCO"
    )
  ),
  public = list(
    #'@field range range
    range = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
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
