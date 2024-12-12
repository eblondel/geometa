#' ISOAbstractThematicAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract thematic accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractThematicAccuracy
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractDQ_ThematicAccuracy}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_ThematicAccuracy}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractThematicAccuracy <- R6Class("ISOAbstractThematicAccuracy",
 inherit = ISODataQualityAbstractElement,
 private = list(
   xmlElement = "AbstractDQ_ThematicAccuracy",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
 ),
 public = list()
)

#' ISOQuantitativeAttributeAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality quantitative attribute accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISOQuantitativeAttributeAccuracy
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOQuantitativeAttributeAccuracy$new()
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
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_QuantitativeAttributeAccuracy}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_QuantitativeAttributeAccuracy}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOQuantitativeAttributeAccuracy <- R6Class("ISOQuantitativeAttributeAccuracy",
 inherit = ISOAbstractThematicAccuracy,
 private = list(
   xmlElement = "DQ_QuantitativeAttributeAccuracy",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
 ),
 public = list()
)

#' ISONonQuantitativeAttributeAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality nonquantitative attribute accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISONonQuantitativeAttributeAccuracy
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISONonQuantitativeAttributeAccuracy$new()
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
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_NonQuantitativeAttributeAccuracy}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_NonQuantitativeAttributeAccuracy}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISONonQuantitativeAttributeAccuracy <- R6Class("ISONonQuantitativeAttributeAccuracy",
  inherit = ISOAbstractThematicAccuracy,
  private = list(
    xmlElement = "DQ_NonQuantitativeAttributeAccuracy",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list()
)

#' ISOThematicClassificationCorrectness
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality thematic accuracy classification correctness
#' @return Object of \code{\link{R6Class}} for modelling an ISOThematicClassificationCorrectness
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOThematicClassificationCorrectness$new()
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
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_ThematicClassificationCorrectness}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_ThematicClassificationCorrectness}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOThematicClassificationCorrectness <- R6Class("ISOThematicClassificationCorrectness",
  inherit = ISOAbstractTemporalAccuracy,
  private = list(
   xmlElement = "DQ_ThematicClassificationCorrectness",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
  ),
  public = list()
)