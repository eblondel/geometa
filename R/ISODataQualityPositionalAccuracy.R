#' ISOAbstractPositionalAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract positional accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractPositionalAccuracy
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractDQ_PositionalAccuracy}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_PositionalAccuracy}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractPositionalAccuracy <- R6Class("ISOAbstractPositionalAccuracy",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "AbstractDQ_PositionalAccuracy",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MDQ"
     )
   ),
   public = list()
)

#' ISOAbsoluteExternalPositionalAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality absolute external positional accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbsoluteExternalPositionalAccuracy
#' @format \code{\link{R6Class}} object.
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
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_AbsoluteExternalPositionalAccuracy}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_AbsoluteExternalPositionalAccuracy}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbsoluteExternalPositionalAccuracy <- R6Class("ISOAbsoluteExternalPositionalAccuracy",
 inherit = ISOAbstractPositionalAccuracy,
 private = list(
   xmlElement = "DQ_AbsoluteExternalPositionalAccuracy",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
 ),
 public = list()
)

#' ISORelativeInternalPositionalAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality relative internal positional accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISORelativeInternalPositionalAccuracy
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISORelativeInternalPositionalAccuracy$new()
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
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_RelativeInternalPositionalAccuracy}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_RelativeInternalPositionalAccuracy}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISORelativeInternalPositionalAccuracy <- R6Class("ISORelativeInternalPositionalAccuracy",
 inherit = ISOAbstractPositionalAccuracy,
 private = list(
   xmlElement = "DQ_RelativeInternalPositionalAccuracy",
   xmlNamespacePrefix = list(
     "19139" = "GMD",
     "19115-3" = "MDQ"
   )
 ),
 public = list()
)

#' ISOGriddedDataPositionalAccuracy
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality gridded data positional accuracy
#' @return Object of \code{\link{R6Class}} for modelling an ISOGriddedDataPositionalAccuracy
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOGriddedDataPositionalAccuracy$new()
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
#'   - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_GriddedDataPositionalAccuracy}
#'   
#'   - ISO 19115-3 \link{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_GriddedDataPositionalAccuracy}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOGriddedDataPositionalAccuracy <- R6Class("ISOGriddedDataPositionalAccuracy",
  inherit = ISOAbstractPositionalAccuracy,
  private = list(
    xmlElement = "DQ_GriddedDataPositionalAccuracy",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list()
)