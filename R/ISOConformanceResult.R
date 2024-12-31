#' ISOConformanceResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO conformance result
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO ConformanceResult
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_ConformanceResult}
#' 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_ConformanceResult}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConformanceResult <- R6Class("ISOConformanceResult",
  inherit = ISOAbstractResult,
  private = list(
      xmlElement = "DQ_ConformanceResult",
      xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "MDQ"
      )
  ),
  public = list(
    
    #'@field resultScope resultScope [0..1]: ISOScope (=> 19115-3)
    resultScope = NULL,
    #'@field dateTime dateTime [0..1] (=> 19115-3)
    dateTime = NULL,
    #'@field specification specification
    specification = NULL,
    #'@field explanation explanation
    explanation = NULL,
    #'@field pass pass
    pass = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set result scope
    #'@param scope object of class \link{ISOScope}
    setResultScope = function(scope){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!is(scope, "ISOScope")){
        stop("The argument should be a 'ISOScope' object")
      }
      self$resultScope = scope
    },
    
    #'@description Set date time
    #'@param dateTime date time, object of class \link{POSIXct}
    setDateTime = function(dateTime){
      self$stopIfMetadataStandardIsNot("19115-3")
      if(!all(class(dateTime) == c("POSIXct","POSIXt"))){ 
        stop("The argument should be an 'POSIXct'/'POSIXt' object")
      }
      self$dateTime <- dateTime
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
