#' ISOConformanceResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO conformance result
#' @return Object of \code{\link{R6Class}} for modelling an ISO ConformanceResult
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'  md <- ISOConformanceResult$new()
#'  spec <- ISOCitation$new()
#'  spec$setTitle("specification title")
#'  spec$addAlternateTitle("specification alternate title")
#'  d <- ISODate$new()
#'  d$setDate(ISOdate(2015, 1, 1, 1))
#'  d$setDateType("publication")
#'  spec$addDate(d)
#'  md$setSpecification(spec)
#'  md$setExplanation("some explanation about the conformance")
#'  md$setPass(TRUE)
#'  xml <- md$encode()
#'  
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConformanceResult <- R6Class("ISOConformanceResult",
  inherit = ISOAbstractResult,
  private = list(
      xmlElement = "DQ_ConformanceResult",
      xmlNamespacePrefix = "GMD"
  ),
  public = list(
    #'@field specification specification
    specification = NULL,
    #'@field explanation explanation
    explanation = NULL,
    #'@field pass pass
    pass = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set specification
    #'@param specification specification
    setSpecification = function(specification){
      if(!is(specification, "ISOCitation")){
        stop("The argument should be a 'ISOCitation' object")
      }
      self$specification <- specification
    },
    
    #'@description Set explanation about the conformance result
    #'@param explanation explanation
    #'@param locales list of localized explanations. Default is \code{NULL}
    setExplanation = function(explanation, locales = NULL){
      self$explanation <- as.character(explanation)
      if(!is.null(locales)){
        self$explanation <- self$createLocalisedProperty(explanation, locales)
      }
    },
    
    #'@description Set wether the conformance passed or not
    #'@param pass object of class \link{logical}
    setPass = function(pass){
      self$pass <- as.logical(pass)
    }
    
  )                        
)