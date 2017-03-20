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
#'  \item{\code{new(value)}}{
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
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConformanceResult <- R6Class("ISOConformanceResult",
  inherit = ISOMetadataElement,
  public = list(
    specification = NULL,
    explanation = NULL,
    pass = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        element = "DQ_ConformanceResult",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
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