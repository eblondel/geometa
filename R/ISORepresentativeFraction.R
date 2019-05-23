#' ISORepresentativeFraction
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO representative fraction
#' @return Object of \code{\link{R6Class}} for modelling an ISO RepresentativeFraction
#' @format \code{\link{R6Class}} object.
#'
#' @field denominator [\code{\link{integer}}] denominator
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, denominator)}}{
#'    This method is used to instantiate an \code{\link{ISORepresentativeFraction}}
#'  }
#'  \item{\code{setDenominator(denominator)}}{
#'    Sets the denominator, object of class \code{integer}
#'  }
#' }
#' 
#' @examples 
#'   fr <- ISORepresentativeFraction$new(denominator = 1L)
#'   xml1 <- fr$encode()
#'   fr$setDenominator(2L)
#'   xml2 <- fr$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#'  
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORepresentativeFraction <- R6Class("ISORepresentativeFraction",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_RepresentativeFraction",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    denominator = NULL,
    initialize = function(xml = NULL, denominator){
      super$initialize(xml = xml)
      if(is.null(xml) & !missing(denominator)){
        self$setDenominator(denominator)
      }
    },
    
    #setDenominator
    setDenominator = function(denominator){
      newValue <- denominator
      if(!is(denominator, "integer")){
        newValue <- as.integer(denominator)
        if(is.na(newValue)){
          stop(sprintf("Value '%s' cannot be coerced to 'integer'", denominator))
        }
      }
      self$denominator = newValue
    }
  )                        
)