#' ISOCitationSeries
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO citation series
#' @return Object of \code{\link{R6Class}} for modelling an ISOCitationSeries
#' @format \code{\link{R6Class}} object.
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOCitationSeries <- R6Class("ISOCitationSeries",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CI_Series",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "CIT"
     )
   ),
   public = list(
     #'@field name name [0..1]
     name = NULL,
     #'@field issueIdentification issueIdentification [0..1] 
     issueIdentification = NULL,
     #'@field page page [0..1]
     page = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },

     #'@description Set name
     #'@param name name
     #'@param locales list of localized names. Default is \code{NULL}
     setName = function(name, locales = NULL){
       classPass <- TRUE
       if(is.null(name)){
         classPass <- FALSE
       }else{
         if(!inherits(name,"ISOAbstractObject")){
           if(!(is.na(name) || is(name, "character"))) classPass <- FALSE
         }else{
           if(is(name,"ISOAnchor")){ classPass <- TRUE }else{ classPass <- FALSE }
         }
       }
       if(!classPass){
         stop("Name should be an object of class 'character' or 'ISOAnchor'")
       }
       self$name <- name
       if(!is.null(locales)){
         self$name <- self$createLocalisedProperty(name, locales)
       }
     },
     
     #'@description Set issue ID
     #'@param issueId issueId
     #'@param locales list of localized ids Default is \code{NULL}
     setIssueIdentification = function(issueId, locales = NULL){
       classPass <- TRUE
       if(is.null(issueId)){
         classPass <- FALSE
       }else{
         if(!inherits(issueId,"ISOAbstractObject")){
           if(!(is.na(issueId) || is(issueId, "character"))) classPass <- FALSE
         }else{
           if(is(issueId,"ISOAnchor")){ classPass <- TRUE }else{ classPass <- FALSE }
         }
       }
       if(!classPass){
         stop("Issue identification should be an object of class 'character' or 'ISOAnchor'")
       }
       self$issueIdentification <- issueId
       if(!is.null(locales)){
         self$issueIdentification <- self$createLocalisedProperty(issueId, locales)
       }
     },
     
     #'@description Set page
     #'@param page page
     #'@param locales list of localized pages. Default is \code{NULL}
     setPage = function(page, locales = NULL){
       classPass <- TRUE
       if(is.null(page)){
         classPass <- FALSE
       }else{
         if(!inherits(page,"ISOAbstractObject")){
           if(!(is.na(page) || is(page, "character"))) classPass <- FALSE
         }else{
           if(is(page,"ISOAnchor")){ classPass <- TRUE }else{ classPass <- FALSE }
         }
       }
       if(!classPass){
         stop("Issue identification should be an object of class 'character' or 'ISOAnchor'")
       }
       self$page <- page
       if(!is.null(locales)){
         self$page <- self$createLocalisedProperty(page, locales)
       }
     }
     
   )                        
)