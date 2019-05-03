#' ISOConformanceResult
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO conformance result
#' @return Object of \code{\link{R6Class}} for modelling an ISO ConformanceResult
#' @format \code{\link{R6Class}} object.
#'
#' @field result
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOConformanceResult
#'  }
#'  \item{\code{setSpecification(specification)}}{
#'    Sets the specification (an ISOCitation object)
#'  }
#'  \item{\code{setExplanation(explanation, locales)}}{
#'    Sets the explanation. Locale names can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
##'  \item{\code{setPass(pass)}}{
#'    Sets if passing the conformance or not (logical value)
#'  }
#' }
#' 
#' @examples
#'  md <- ISOConformanceResult$new()
#'  spec <- ISOCitation$new()
#'  spec$setTitle("specification title")
#'  spec$setAlternateTitle("specification alternate title")
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
    specification = NULL,
    explanation = NULL,
    pass = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #setSpecification
    setSpecification = function(specification){
      if(!is(specification, "ISOCitation")){
        stop("The argument should be a 'ISOCitation' object")
      }
      self$specification <- specification
    },
    
    #setExplanation
    setExplanation = function(explanation, locales = NULL){
      self$explanation <- as.character(explanation)
      if(!is.null(locales)){
        self$explanation <- self$createLocalisedProperty(explanation, locales)
      }
    },
    
    #setPass
    setPass = function(pass){
      self$pass <- as.logical(pass)
    }
    
  )                        
)