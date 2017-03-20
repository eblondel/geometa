#' ISODataQuality
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataQuality
#' @format \code{\link{R6Class}} object.
#'
#' @field scope
#' @field report
#' @field lineage
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISODataQuality
#'  }
#'  \item{\code{setScope(scope)}}{
#'    Sets the scope
#'  }
#'  \item{\code{addReport(report)}}{
#'    Adds a report
#'  }
#'  \item{\code{setLineage(lineage)}}{
#'    Sets the lineage
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataQuality <- R6Class("ISODataQuality",
  inherit = ISOMetadataElement,
  public = list(
    scope = NULL,
    report = list(),
    lineage = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        element = "DQ_DataQuality",
        namespace = ISOMetadataNamespace$GMD
      )
      if(!is.null(xml)){
        self$decode(xml)
      }
    },
    
    #setScope
    setScope = function(scope){
      if(!is(scope, "ISOScope")){
        stop("The argument should be a 'ISOScope' object")
      }
      self$scope <- scope
    },
    
    #addReport
    addReport = function(report){
      if(!is(report, "ISODomainConsistency")){
        stop("The argument should be a 'ISODomainConsistency' object")
      }
      self$report <- c(self$report, report)
    },
    
    #setLineage
    setLineage = function(lineage){
      if(!is(lineage, "ISOLineage")){
        stop("The argument should be a 'ISOLineage' object")
      }
      self$lineage <- lineage
    }
  )                        
)