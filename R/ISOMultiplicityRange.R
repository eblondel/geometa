#' ISOMultiplicityRange
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO multiplicity range
#' @return Object of \code{\link{R6Class}} for modelling an ISO MultiplicityRange
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, lower, upper)}}{
#'    This method is used to instantiate an ISOMultiplicityRange
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOMultiplicityRange <- R6Class("ISOMultiplicityRange",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "MultiplicityRange",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    lower = NULL,
    upper = NULL,
    initialize = function(xml = NULL, lower, upper){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
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