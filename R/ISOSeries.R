#' ISOSeries
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO Series
#' @return Object of \code{\link{R6Class}} for modelling an ISOSeries
#' @format \code{\link{R6Class}} object.
#'    
#' @references 
#'  - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_CI_Series}
#'  
#'  - ISO 19115-3 \link{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_Series}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOSeries <- R6Class("ISOSeries",
 inherit = ISOAbstractAggregate,
 private = list(
   xmlElement = "DS_Series",
   xmlNamespacePrefix = "GMD"
 ),
 public = list(
   
    #'@field name name [0..1] : character
    name = NULL,
    #'@field issueIdentification issueIdentification [0..1]: character
    issueIdentification = NULL,
    #'@field page page [0..1]: character
    page = NULL,
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set name
    #'@param name name
    #'@param locales list of localized titles. Default is \code{NULL}
    setName = function(name, locales = NULL){
      self$name <- name
      if(!is.null(locales)){
        self$name <- self$createLocalisedProperty(name, locales)
      }
    },
    
    #'@description Set issue identification
    #'@param issueIdentification issue identification
    #'@param locales list of localized titles. Default is \code{NULL}
    setIssueIdentification = function(issueIdentification, locales = NULL){
      self$issueIdentification <- issueIdentification
      if(!is.null(locales)){
        self$issueIdentification <- self$createLocalisedProperty(issueIdentification, locales)
      }
    },
    
    #'@description Set page
    #'@param page
    #'@param locales list of localized titles. Default is \code{NULL}
    setPage = function(page, locales = NULL){
      self$page <- page
      if(!is.null(locales)){
        self$page <- self$createLocalisedProperty(page, locales)
      }
    }
 )                        
)