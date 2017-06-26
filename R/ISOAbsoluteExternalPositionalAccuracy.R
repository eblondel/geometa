#' ISOAbsoluteExternalPositionalAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality absolute external positional accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbsoluteExternalPositionalAccuracy
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited methods from \code{ISODataQualityAbstractElement}
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOAbsoluteExternalPositionalAccuracy
#'  }
#' }
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOAbsoluteExternalPositionalAccuracy$new()
#'   dq$addNameOfMeasure("measure")
#'   metaId <- ISOMetaIdentifier$new(code = "measure-id")
#'   dq$setMeasureIdentification(metaId)
#'   dq$setMeasureDescription("description")
#'   dq$setEvaluationMethodDescription("method description")
#'   dq$setEvaluationMethodType("indirect")
#'   dq$setDateTime(ISOdate(2015,1,1,12,10,49))
#'   spec <- ISOCitation$new()
#'   spec$setTitle("specification title")
#'   spec$setAlternateTitle("specification alternate title")
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
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbsoluteExternalPositionalAccuracy <- R6Class("ISOAbsoluteExternalPositionalAccuracy",
  inherit = ISOAbstractPositionalAccuracy,
  private = list(
    xmlElement = "DQ_AbsoluteExternalPositionalAccuracy",
    xmlNamespacePrefix = "GMD"
  ),
  public = list()
)