#' ISODomainConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO domain consistency
#' @return Object of \code{\link{R6Class}} for modelling an ISO DomainConsistency
#' @format \code{\link{R6Class}} object.
#'
#' @field result
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISODomainConsistency
#'  }
#'  \item{\code{addResult(result)}}{
#'    Adds a result
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODomainConsistency <- R6Class("ISODomainConsistency",
  inherit = ISOMetadataElement,
  public = list(
    result = list(),
    initialize = function(xml = NULL){
      super$initialize(
        element = "DQ_DomainConsistency",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #addResult
    addResult = function(result){
      if(!is(result, "ISOConformanceResult")){
        stop("The argument should be a 'ISOConformanceResult' object")
      }
      self$result <- c(self$result, result)
    }
  )                        
)