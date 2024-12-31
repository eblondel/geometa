#' ISODataQuality
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO DataQuality
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #create dataQuality object with a 'dataset' scope
#'   dq <- ISODataQuality$new()
#'   scope <- ISODataQualityScope$new()
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
#'   spec$addAlternateTitle("This is is some data quality check report")
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
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ" 
    )
  ),
  public = list(
    #'@field scope scope
    scope = NULL,
    #'@field standaloneQualityReport standalone quality report (=> 19115-3)
    standaloneQualityReport = NULL,
    #'@field report list of reports (=> 19139)
    report = list(),
    #'@field lineage lineage
    lineage = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set scope
    #'@param scope scope
    setScope = function(scope){
      switch(getMetadataStandard(),
        "19139" = {
          if(!is(scope, "ISODataQualityScope")){
            stop("The argument should be a 'ISODataQualityScope' object")
          }
        },
        "19115-3" = {
          if(!is(scope, "ISOScope")){
            stop("The argument should be a 'ISOScope' object")
          }
        }
      )
      self$scope <- scope
    },
    
    #'@description Set standalone quality report
    #'@param report object of class \link{ISOStandaloneQualityReportInformation}
    setStandaloneQualityReport = function(report){
      if(!is(report, "ISOStandaloneQualityReportInformation")){
        stop("The argument should inherit class 'ISOStandaloneQualityReportInformation'")
      }
      self$standaloneQualityReport = report
    },
    
    #'@description Adds report
    #'@param report report, object of class \link{ISODataQualityAbstractElement}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addReport = function(report){
      if(!is(report, "ISODataQualityAbstractElement")){
        stop("The argument should inherit class 'ISODataQualityAbstractElement'")
      }
      self$report <- c(self$report, report)
    },
    
    #'@description Set lineage
    #'@param lineage lineage, object of class \link{ISOLineage}
    setLineage = function(lineage){
      self$stopIfMetadataStandardIsNot("19139")
      if(!is(lineage, "ISOLineage")){
        stop("The argument should be a 'ISOLineage' object")
      }
      self$lineage <- lineage
    }
  )                        
)
