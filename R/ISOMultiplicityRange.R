#' ISOMultiplicityRange
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO multiplicity range
#' @return Object of \code{\link{R6Class}} for modelling an ISO MultiplicityRange
#' @format \code{\link{R6Class}} object.
#'
#' @field lower [\code{\link{integer}}] lower value
#' @field upper [\code{\link{integer}}] upper value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, lower, upper)}}{
#'    This method is used to instantiate an \code{\link{ISOMultiplicityRange}}
#'  }
#' }
#' 
#' @examples
#'   md <- ISOMultiplicityRange$new(lower = 1, upper = Inf)
#'   xml <- md$encode()
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMultiplicityRange <- R6Class("ISOMultiplicityRange",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MultiplicityRange",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    lower = NULL,
    upper = NULL,
    initialize = function(xml = NULL, lower, upper){
      super$initialize(xml = xml)
      if(is.null(xml)){
        if(!is(lower, "integer")){
          lower <- as.integer(lower)
          if(is.na(lower)){
            stop("The argument 'lower' should an integer")
          }
        }
        self$lower = lower
        self$upper = upper
        class(self$upper) <- "unlimitedinteger"
      }
    }
  )                        
)