#' ISOEvaluationMethod
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO evaluation method
#' @return Object of \code{\link{R6Class}} for modelling an ISO abstract evaluation method
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_EvaluationMethod}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOEvaluationMethod <- R6Class("ISOEvaluationMethod",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "DQ_EvaluationMethod",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@field dateTime dateTime
     dateTime = NULL,
     #'@field evaluationMethodDescription evaluationMethodDescription
     evaluationMethodDescription = NULL,
     #'@field evaluationProcedure evaluationProcedure
     evaluationProcedure = NULL,
     #'@field referenceDoc referenceDoc
     referenceDoc = list(),
     #'@field evaluationMethodType evaluationMethodType
     evaluationMethodType = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set date time
     #'@param dateTime dateTime object of class \link{ISOBaseDateTime}
     setDateTime = function(dateTime){
       if(!is(dateTime, "ISOBaseDateTime")){
         stop("The argument 'dateTime' should be an object of class 'ISOBaseDateTime'")
       }
       self$dateTime = dateTime
     },
     
     #'@description Set evaluation method description
     #'@param description description
     #'@param locales list of localized descriptions. Default is \code{NULL}
     setEvaluationMethodDescription = function(description, locales = NULL){
       if(!is.null(locales)){
         description <- self$createLocalisedProperty(description, locales)
       }
       self$evaluationMethodDescription <- description
     },
     
     #'@description Set evaluation procedure
     #'@param procedure procedure, object of class \link{ISOCitation}
     setEvaluationProcedure = function(procedure){
       if(!is(procedure, "ISOCitation")){
         stop("The argument value should be an object of class 'ISOCitation'")
       }
       self$evaluationProcedure <- procedure
     },
     
     #'@description Adds reference doc
     #'@param referenceDoc object of class \link{ISOCitation}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addReferenceDoc = function(referenceDoc){
       if(!is(referenceDoc, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation'")
       }
       return(self$addListElement("referenceDoc",referenceDoc))
     },
     
     #'@description Deletes reference doc
     #'@param referenceDoc object of class \link{ISOCitation}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delReferenceDoc = function(referenceDoc){
       if(!is(referenceDoc, "ISOCitation")){
         stop("The argument should be an object of class 'ISOCitation'")
       }
       return(self$delListElement("referenceDoc",referenceDoc))
     },
     
     #'@description Set evaluation method type
     #'@param type object of class \link{ISOEvaluationMethodType} or any \link{character} value
     #'  from those returned by \code{ISOEvaluationMethodType$values()}
     setEvaluationMethodType = function(type){
       if(!is(type, "ISOEvaluationMethodType")){
         type <- ISOEvaluationMethodType$new(value = type)
       }
       self$evaluationMethodType <- type
     }
   )                        
)

#' ISOAggregationDerivation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO aggregation derivation
#' @return Object of \code{\link{R6Class}} for modelling an ISO aggregation derivation
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_AggregationDerivation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAggregationDerivation <- R6Class("ISOAggregationDerivation",
   inherit = ISOEvaluationMethod,
   private = list(
     xmlElement = "DQ_AggregationDerivation",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )
)

#' ISOFullInspection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO full inspection
#' @return Object of \code{\link{R6Class}} for modelling an ISO full inspection
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_FullInspection}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFullInspection <- R6Class("ISOFullInspection",
  inherit = ISOEvaluationMethod,
  private = list(
    xmlElement = "DQ_FullInspection",
    xmlNamespacePrefix = list(
      "19115-3" = "MDQ"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    }
  )
)

#' ISOIndirectEvaluation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO indirect evaluation
#' @return Object of \code{\link{R6Class}} for modelling an ISO indirect evaluation
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_IndirectEvaluation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOIndirectEvaluation <- R6Class("ISOIndirectEvaluation",
   inherit = ISOEvaluationMethod,
   private = list(
     xmlElement = "DQ_IndirectEvaluation",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )
)

#' ISOSampleBasedInspection
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO sample based inspection
#' @return Object of \code{\link{R6Class}} for modelling an ISO sample based inspection
#' @format \code{\link{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_DQ_SampleBasedInspection}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSampleBasedInspection <- R6Class("ISOSampleBasedInspection",
   inherit = ISOEvaluationMethod,
   private = list(
     xmlElement = "DQ_SampleBasedInspection",
     xmlNamespacePrefix = list(
       "19115-3" = "MDQ"
     )
   ),
   public = list(
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     }
   )
)
