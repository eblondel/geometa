#' ISOAbstractTemporalAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract temporal accuracy
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAbstractTemporalAccuracy
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractTemporalAccuracy <- R6Class("ISOAbstractTemporalAccuracy",
  inherit = ISODataQualityAbstractElement,
  private = list(
   xmlElement = "AbstractDQ_TemporalAccuracy",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
  ),
  public = list()
)

#' ISOTemporalValidity
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality temporal validity
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOTemporalValidity
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOTemporalValidity$new()
#'   dq$addNameOfMeasure("measure")
#'   metaId <- ISOMetaIdentifier$new(code = "measure-id")
#'   dq$setMeasureIdentification(metaId)
#'   dq$setMeasureDescription("description")
#'   dq$setEvaluationMethodDescription("method description")
#'   dq$setEvaluationMethodType("indirect")
#'   dq$setDateTime(ISOdate(2015,1,1,12,10,49))
#'   spec <- ISOCitation$new()
#'   spec$setTitle("specification title")
#'   spec$addAlternateTitle("specification alternate title")
#'   d <- ISODate$new()
#'   d$setDate(ISOdate(2015, 1, 1, 1))
#'   d$setDateType("publication")
#'   spec$addDate(d)
#'   dq$setEvaluationProcedure(spec)
#'   result <- ISOConformanceResult$new()
#'   result$setSpecification(spec)
#'   result$setExplanation("some explanation about the conformance")
#'   result$setPass(TRUE)
#'   dq$addResult(result)
#'   xml <- dq$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTemporalValidity <- R6Class("ISOTemporalValidity",
  inherit = ISOAbstractTemporalAccuracy,
  private = list(
   xmlElement = "DQ_TemporalValidity",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
  ),
  public = list()
)

#' ISOTemporalConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality temporal consistency
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOTemporalConsistency
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOTemporalConsistency$new()
#'   dq$addNameOfMeasure("measure")
#'   metaId <- ISOMetaIdentifier$new(code = "measure-id")
#'   dq$setMeasureIdentification(metaId)
#'   dq$setMeasureDescription("description")
#'   dq$setEvaluationMethodDescription("method description")
#'   dq$setEvaluationMethodType("indirect")
#'   dq$setDateTime(ISOdate(2015,1,1,12,10,49))
#'   spec <- ISOCitation$new()
#'   spec$setTitle("specification title")
#'   spec$addAlternateTitle("specification alternate title")
#'   d <- ISODate$new()
#'   d$setDate(ISOdate(2015, 1, 1, 1))
#'   d$setDateType("publication")
#'   spec$addDate(d)
#'   dq$setEvaluationProcedure(spec)
#'   result <- ISOConformanceResult$new()
#'   result$setSpecification(spec)
#'   result$setExplanation("some explanation about the conformance")
#'   result$setPass(TRUE)
#'   dq$addResult(result)
#'   xml <- dq$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTemporalConsistency <- R6Class("ISOTemporalConsistency",
  inherit = ISOAbstractTemporalAccuracy,
  private = list(
   xmlElement = "DQ_TemporalConsistency",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
  ),
  public = list()
)

#' ISOAccuracyOfATimeMeasurement
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality temporal accuracy time measurement
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAccuracyOfATimeMeasurement
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOAccuracyOfATimeMeasurement$new()
#'   dq$addNameOfMeasure("measure")
#'   metaId <- ISOMetaIdentifier$new(code = "measure-id")
#'   dq$setMeasureIdentification(metaId)
#'   dq$setMeasureDescription("description")
#'   dq$setEvaluationMethodDescription("method description")
#'   dq$setEvaluationMethodType("indirect")
#'   dq$setDateTime(ISOdate(2015,1,1,12,10,49))
#'   spec <- ISOCitation$new()
#'   spec$setTitle("specification title")
#'   spec$addAlternateTitle("specification alternate title")
#'   d <- ISODate$new()
#'   d$setDate(ISOdate(2015, 1, 1, 1))
#'   d$setDateType("publication")
#'   spec$addDate(d)
#'   dq$setEvaluationProcedure(spec)
#'   result <- ISOConformanceResult$new()
#'   result$setSpecification(spec)
#'   result$setExplanation("some explanation about the conformance")
#'   result$setPass(TRUE)
#'   dq$addResult(result)
#'   xml <- dq$encode()
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAccuracyOfATimeMeasurement <- R6Class("ISOAccuracyOfATimeMeasurement",
  inherit = ISOAbstractTemporalAccuracy,
  private = list(
    xmlElement = "DQ_AccuracyOfATimeMeasurement",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list()
)
