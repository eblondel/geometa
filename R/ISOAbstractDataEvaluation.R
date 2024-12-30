#' ISOAbstractDataEvaluation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract data evaluation
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO abstract data evaluation
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19157/-/mdq/1.2/mdq/#element_AbstractDQ_DataEvaluation}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractDataEvaluation <- R6Class("ISOAbstractDataEvaluation",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "AbstractDQ_DataEvaluation",
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
