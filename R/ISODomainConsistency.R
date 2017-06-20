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
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISODomainConsistency
#'  }
#'  \item{\code{addResult(result)}}{
#'    Adds a result
#'  }
#' }
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODomainConsistency <- R6Class("ISODomainConsistency",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "DQ_DomainConsistency",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    result = list(),
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
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