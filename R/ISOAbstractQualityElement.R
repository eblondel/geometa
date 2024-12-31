#' ISOAbstractQualityElement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract quality element
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract quality element
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/dqc/1.2/dqc/#element_Abstract_QualityElement}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractQualityElement <- R6Class("ISOAbstractQualityElement",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Abstract_QualityElement",
    xmlNamespacePrefix = list(
      "19115-3" = "DQC"
    )
  ),
  public = list(
    
    #'@field dateTime dateTime
    dateTime = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set date time
    #'@param dateTime dateTime object of class \link{ISOBaseDateTime}
    setDateTime = function(dateTime){
      if(!is(dateTime, "ISOBaseDateTime")){
        stop("The argument 'dateTime' should be an object of class 'ISOBaseDateTime'")
      }
      self$dateTime = dateTime
    }
  )                        
)
