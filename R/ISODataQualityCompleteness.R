#' ISOAbstractCompleteness
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality abstract completeness
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstractCompleteness
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited methods:
#' \describe{
#'   from \code{\link{ISODataQualityAbstractElement}}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOAbstractCompleteness}}
#'  }
#' }
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractCompleteness <- R6Class("ISOAbstractCompleteness",
   inherit = ISODataQualityAbstractElement,
   private = list(
     xmlElement = "AbstractDQ_Completeness",
     xmlNamespacePrefix = "GMD"
   ),
   public = list()
)

#' ISOCompletenessOmission
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality completeness omission
#' @return Object of \code{\link{R6Class}} for modelling an ISOCompletenessOmission
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited methods:
#' \describe{
#'   from \code{\link{ISODataQualityAbstractElement}}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOCompletenessOmission}}
#'  }
#' }
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
ISOCompletenessOmission <- R6Class("ISOCompletenessOmission",
  inherit = ISOAbstractThematicAccuracy,
  private = list(
    xmlElement = "DQ_CompletenessOmission",
    xmlNamespacePrefix = "GMD"
  ),
  public = list()
)

#' ISOCompletenessCommission
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO data quality completeness commission
#' @return Object of \code{\link{R6Class}} for modelling an ISOCompletenessCommission
#' @format \code{\link{R6Class}} object.
#'
#' @section Inherited methods:
#' \describe{
#'   from \code{\link{ISODataQualityAbstractElement}}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an \code{\link{ISOCompletenessCommission}}
#'  }
#' }
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
ISOCompletenessCommission <- R6Class("ISOCompletenessCommission",
   inherit = ISOAbstractThematicAccuracy,
   private = list(
     xmlElement = "DQ_CompletenessCommission",
     xmlNamespacePrefix = "GMD"
   ),
   public = list()
)
