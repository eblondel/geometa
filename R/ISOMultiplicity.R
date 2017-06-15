#' ISOMultiplicity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO multiplicity
#' @return Object of \code{\link{R6Class}} for modelling an ISOMultiplicity
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, lower, upper)}}{
#'    This method is used to instantiate an ISOMultiplicity. The range is specified
#'    by two arguments \code{lower} and \code{upper}.
#'  }
#' }
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
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "Multiplicity",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    range = NULL,
    initialize = function(xml = NULL, lower, upper){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
      if(is.null(xml)){
        self$range <- ISOMultiplicityRange$new(lower = lower, upper = upper)
      }
    }
  )                        
)