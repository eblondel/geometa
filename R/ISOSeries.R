#' ISOSeries
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO series
#' @return Object of \code{\link{R6Class}} for modelling an ISO Extent
#' @format \code{\link{R6Class}} object.
#'
#' @field name
#' @field issueIdentification
#' @field page
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOSeries
#'  }
#'  \item{\code{setName(name, locales)}}{
#'    Sets a name (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setIssueIdentification(issueId, locales)}}{
#'    Sets an issue identification (object of class "character"). Locale 
#'    names can be specified as \code{list} with the \code{locales} argument.
#'  }
#'  \item{\code{setPage(page, locales)}}{
#'    Sets a page (object of class "character"). Locale names can be 
#'    specified as \code{list} with the \code{locales} argument.
#'  }
#' }
#' 
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata 
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSeries <- R6Class("ISOSeries",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CI_Series",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+name [0..1]
     name = NULL,
     #+issueIdentification [0..1] 
     issueIdentification = NULL,
     #+page [0..1]
     page = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },

     #setName
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
     
     #setIssueIdentification
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
     
     #setPage
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