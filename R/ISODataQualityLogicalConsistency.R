#' ISOAbstractLogicalConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract logical consistency
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAbstractLogicalConsistency
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractDQ_LogicalConsistency}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_LogicalConsistency}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractLogicalConsistency <- R6Class("ISOAbstractLogicalConsistency",
  inherit = ISODataQualityAbstractElement,
  private = list(
   xmlElement = "AbstractDQ_LogicalConsistency",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
  ),
  public = list()
)

#' ISOTopologicalConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality topological consistency
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOTopologicalConsistency
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_TopologicalConsistency}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_TopologicalConsistency} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTopologicalConsistency <- R6Class("ISOTopologicalConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
    xmlElement = "DQ_TopologicalConsistency",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list()
)

#' ISOFormatConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality format consistency
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOFormatConsistency
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_FormatConsistency}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_FormatConsistency} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFormatConsistency <- R6Class("ISOFormatConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
   xmlElement = "DQ_FormatConsistency",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
  ),
  public = list()
)

#' ISODomainConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality domain consistency
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISODomainConsistency
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_DomainConsistency}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_DomainConsistency} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISODomainConsistency <- R6Class("ISODomainConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
    xmlElement = "DQ_DomainConsistency",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list()
)

#' ISOConceptualConsistency
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality conceptual consistency
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOConceptualConsistency
#' @format \code{\link[R6]{R6Class}} object.
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_ConceptualConsistency}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_ConceptualConsistency} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOConceptualConsistency <- R6Class("ISOConceptualConsistency",
  inherit = ISOAbstractLogicalConsistency,
  private = list(
    xmlElement = "DQ_ConceptualConsistency",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list()
)
