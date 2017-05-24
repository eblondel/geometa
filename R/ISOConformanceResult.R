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
#'  \item{\code{setExplanation(explanation)}}{
#'    Sets the explanation
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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConformanceResult <- R6Class("ISOConformanceResult",
  inherit = ISOMetadataElement,
  private = list(
      xmlElement = "DQ_ConformanceResult",
      xmlNamespacePrefix = "GMD"
  ),
  public = list(
    specification = NULL,
    explanation = NULL,
    pass = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
    },
    
    #setSpecification
    setSpecification = function(specification){
      if(!is(specification, "ISOCitation")){
        stop("The argument should be a 'ISOCitation' object")
      }
      self$specification <- specification
    },
    
    #setExplanation
    setExplanation = function(explanation){
      self$explanation <- as.character(explanation)
    },
    
    #setPass
    setPass = function(pass){
      self$pass <- as.logical(pass)
    }
    
  )                        
)