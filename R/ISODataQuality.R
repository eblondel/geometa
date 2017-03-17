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
#'  \item{\code{setReport(report)}}{
#'    Sets the report
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
    report = NULL,
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
      stop("Not yet implemented")
    },
    
    #setReport
    setReport = function(report){
      stop("Not yet implemented")
    },
    
    #setLineage
    setLineage = function(lineage){
      stop("Not yet implemented")
    }
  )                        
)