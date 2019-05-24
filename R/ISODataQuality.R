#' ISODataQuality
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality
#' @return Object of \code{\link{R6Class}} for modelling an ISO DataQuality
#' @format \code{\link{R6Class}} object.
#'
#' @field scope [\code{\link{ISOScope}}] scope
#' @field report [\code{\link{ISODomainConsistency}}] report
#' @field lineage [\code{\link{ISOLineage}}] lineage information
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISODataQuality}}
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
#'   #create dataQuality object with a 'dataset' scope
#'   dq <- ISODataQuality$new()
#'   scope <- ISOScope$new()
#'   scope$setLevel("dataset")
#'   dq$setScope(scope)
#'   
#'   #add data quality reports...
#'   
#'   #add a report the data quality
#'   dc <- ISODomainConsistency$new()
#'   result <- ISOConformanceResult$new()
#'   spec <- ISOCitation$new()
#'   spec$setTitle("Data Quality check")
#'   spec$setAlternateTitle("This is is some data quality check report")
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
#'   #add INSPIRE reports?
#'   #INSPIRE - interoperability of spatial data sets and services
#'   dc_inspire1 <- ISODomainConsistency$new()
#'   cr_inspire1 <- ISOConformanceResult$new()
#'   cr_inspire_spec1 <- ISOCitation$new()
#'   cr_title <- paste(
#'     "Commission Regulation (EU) No 1089/2010 of 23 November 2010 implementing Directive 2007/2/EC",
#'     "of the European Parliament and of the Council as regards interoperability of spatial data",
#'     "sets and services"
#'   )
#'   cr_inspire_spec1$setTitle(cr_title)
#'   cr_inspire1$setExplanation("See the referenced specification")
#'   cr_inspire_date1 <- ISODate$new()
#'   cr_inspire_date1$setDate(ISOdate(2010,12,8))
#'   cr_inspire_date1$setDateType("publication")
#'   cr_inspire_spec1$addDate(cr_inspire_date1)
#'   cr_inspire1$setSpecification(cr_inspire_spec1)
#'   cr_inspire1$setPass(TRUE)
#'   dc_inspire1$addResult(cr_inspire1)
#'   dq$addReport(dc_inspire1)
#'   #INSPIRE - metadata
#'   dc_inspire2 <- ISODomainConsistency$new()
#'   cr_inspire2 <- ISOConformanceResult$new()
#'   cr_inspire_spec2 <- ISOCitation$new()
#'   cr_title2 <- paste(
#'     "COMMISSION REGULATION (EC) No 1205/2008 of 3 December 2008 implementing Directive 2007/2/EC",
#'     "of the European Parliament and of the Council as regards metadata"
#'   )
#'   cr_inspire_spec2$setTitle(cr_title2)
#'   cr_inspire2$setExplanation("See the referenced specification")
#'   cr_inspire_date2 <- ISODate$new()
#'   cr_inspire_date2$setDate(ISOdate(2008,12,4))
#'   cr_inspire_date2$setDateType("publication")
#'   cr_inspire_spec2$addDate(cr_inspire_date2)
#'   cr_inspire2$setSpecification(cr_inspire_spec2)
#'   cr_inspire2$setPass(TRUE)
#'   dc_inspire2$addResult(cr_inspire2)
#'   dq$addReport(dc_inspire2)
#'   
#'   #add lineage (more example of lineages in ISOLineage documentation)
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
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "DQ_DataQuality",
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    scope = NULL,
    report = list(),
    lineage = NULL,
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
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