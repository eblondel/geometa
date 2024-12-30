#' ISOAbstractCompleteness
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract completeness
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAbstractCompleteness
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_AbstractDQ_Completeness}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_Completeness}  
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractCompleteness <- R6Class("ISOAbstractCompleteness",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "AbstractDQ_Completeness",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MDQ"
     )
   ),
   public = list()
)

#' ISOCompletenessOmission
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality completeness omission
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOCompletenessOmission
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOCompletenessOmission$new()
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_CompletenessOmission}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_CompletenessOmission} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCompletenessOmission <- R6Class("ISOCompletenessOmission",
  inherit = ISOAbstractThematicAccuracy,
  private = list(
    xmlElement = "DQ_CompletenessOmission",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "MDQ"
    )
  ),
  public = list()
)

#' ISOCompletenessCommission
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality completeness commission
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOCompletenessCommission
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples 
#'   #encoding
#'   dq <- ISOCompletenessCommission$new()
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
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_DQ_CompletenessCommission}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_CompletenessCommission} 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCompletenessCommission <- R6Class("ISOCompletenessCommission",
   inherit = ISOAbstractThematicAccuracy,
   private = list(
     xmlElement = "DQ_CompletenessCommission",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "MDQ"
     )
   ),
   public = list()
)
