#' ISOAbstractLogicalConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract logical consistency
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractLogicalConsistency
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractLogicalConsistency <- R6Class("ISOAbstractLogicalConsistency",
  inherit = ISODataQualityAbstractElement,
  private = list(
   xmlElement = "AbstractDQ_LogicalConsistency",
   xmlNamespacePrefix = "GMD"
  ),
  public = list()
)

#' ISOTopologicalConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality topological consistency
#' @return Object of \code{\link{R6Class}} for modelling an ISOTopologicalConsistency
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOTopologicalConsistency$new()
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
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopologicalConsistency <- R6Class("ISOTopologicalConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
    xmlElement = "DQ_TopologicalConsistency",
    xmlNamespacePrefix = "GMD"
  ),
  public = list()
)

#' ISOFormatConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality format consistency
#' @return Object of \code{\link{R6Class}} for modelling an ISOFormatConsistency
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOFormatConsistency$new()
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
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFormatConsistency <- R6Class("ISOFormatConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
   xmlElement = "DQ_FormatConsistency",
   xmlNamespacePrefix = "GMD"
  ),
  public = list()
)

#' ISODomainConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality domain consistency
#' @return Object of \code{\link{R6Class}} for modelling an ISODomainConsistency
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISODomainConsistency$new()
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
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODomainConsistency <- R6Class("ISODomainConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
    xmlElement = "DQ_DomainConsistency",
    xmlNamespacePrefix = "GMD"
  ),
  public = list()
)

#' ISOConceptualConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality conceptual consistency
#' @return Object of \code{\link{R6Class}} for modelling an ISOConceptualConsistency
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOConceptualConsistency$new()
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
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConceptualConsistency <- R6Class("ISOConceptualConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
    xmlElement = "DQ_ConceptualConsistency",
    xmlNamespacePrefix = "GMD"
  ),
  public = list()
)