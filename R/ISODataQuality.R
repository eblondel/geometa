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
#'  \item{\code{new(xml,value)}}{
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
#' @examples
#'   #create dataQuality object
#'   dq <- ISODataQuality$new()
#'   
#'   #add scope
#'   scope <- ISOScope$new()
#'   scope$setLevel("dataset")
#'   dq$setScope(scope)
#'   
#'   #add report
#'   dc <- ISODomainConsistency$new()
#'   result <- ISOConformanceResult$new()
#'   spec <- ISOCitation$new()
#'   spec$setTitle("specification title")
#'   spec$setAlternateTitle("specification alternate title")
#'   d <- ISODate$new()
#'   d$setDate(ISOdate(2015, 1, 1, 1))
#'   d$setDateType("publication")
#'   spec$addDate(d)
#'   result$setSpecification(spec)
#'   result$setExplanation("some explanation about the conformance")
#'   result$setPass(TRUE)
#'   dc$addResult(result)
#'   dq$addReport(dc)
#'   
#'   #add lineage
#'   lineage <- ISOLineage$new()
#'   lineage$setStatement("statement")
#'   dq$setLineage(lineage)
#'   
#'   #xml
#'   xml <- dq$encode()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODataQuality <- R6Class("ISODataQuality",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "DQ_DataQuality",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    scope = NULL,
    report = list(),
    lineage = NULL,
    initialize = function(xml = NULL){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
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