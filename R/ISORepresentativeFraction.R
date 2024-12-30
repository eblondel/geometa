#' ISORepresentativeFraction
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO representative fraction
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO RepresentativeFraction
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   fr <- ISORepresentativeFraction$new(denominator = 1L)
#'   xml1 <- fr$encode()
#'   fr$setDenominator(2L)
#'   xml2 <- fr$encode()
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_MD_RepresentativeFraction}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/mri/1.0/mri/#element_MD_RepresentativeFraction}
#'  
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORepresentativeFraction <- R6Class("ISORepresentativeFraction",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "MD_RepresentativeFraction",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MRI"
    )
  ),
  public = list(
    #'@field denominator denominator
    denominator = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param denominator denominator
    initialize = function(xml = NULL, denominator){
      super$initialize(xml = xml)
      if(is.null(xml) & !missing(denominator)){
        self$setDenominator(denominator)
      }
    },
    
    #'@description Set denominator
    #'@param denominator object of class \link{integer}
    setDenominator = function(denominator){
      newValue <- denominator
      if(!is(denominator, "numeric")){
        newValue <- as.numeric(denominator)
        if(is.na(newValue)){
          stop(sprintf("Value '%s' cannot be coerced to 'numeric'", denominator))
        }
      }
      self$denominator = newValue
    }
  )                        
)
