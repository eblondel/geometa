#' ISOAbstractCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract catalogue
#' @return Object of \code{\link{R6Class}} for modelling an ISOAbstracCatalogue
#' @format \code{\link{R6Class}} object.
#'  
#' @references 
#'   ISO 19139:2007 Metadata - XML schema implementation
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractCatalogue <- R6Class("ISOAbstractCatalogue",
   inherit = ISOAbstractObject,
   private = list(
     document = TRUE,
     xmlElement = "AbstractCT_Catalogue",
     xmlNamespacePrefix = "GMX"
   ),
   public = list(
     
     #'@field name name [1..1]: character
     name = NULL,
     #'@field scope scope [1..*]: character
     scope = list(),
     #'@field fieldOfApplication fieldOfApplication [0.*]: character
     fieldOfApplication = list(),
     #'@field versionNumber versionNumber [1..1]: character
     versionNumber = NULL,
     #'@field versionDate versionDate [1..1]: Date/Posix
     versionDate = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Sets the name. Locale names can be specified as
     #'@param name name
     #'@param locales locales, object of class \link{list}
     setName = function(name, locales = NULL){
       if(!is(name,"character")) name <- as(name, "character")
       if(!is.null(locales)){
         name <- self$createLocalisedProperty(name, locales)
       }
       self$name <- name
     },
     
     #'@description Adds scope
     #'@param scope scope
     #'@param locales locales, object of class \link{list}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addScope = function(scope, locales = NULL){
       if(!is.null(locales)){
         scope <- self$createLocalisedProperty(scope, locales)
       }
       return(self$addListElement("scope", scope))
     },
     
     #'@description Deletes scope
     #'@param scope scope
     #'@param locales locales, object of class \link{list}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delScope = function(scope, locales = NULL){
       if(!is.null(locales)){
         scope <- self$createLocalisedProperty(scope, locales)
       }
       return(self$delListElement("scope", scope))
     },
     
     #'@description Adds field of application
     #'@param fieldOfApplication field of application
     #'@param locales locales, object of class \link{list}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addFieldOfApplication = function(fieldOfApplication, locales = NULL){
       if(!is.null(locales)){
         fieldOfApplication <- self$createLocalisedProperty(fieldOfApplication, locales)
       }
       return(self$addListElement("fieldOfApplication", fieldOfApplication))
     },
     
     #'@description Deletes field of application
     #'@param fieldOfApplication field of application
     #'@param locales locales, object of class \link{list}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delFieldOfApplication = function(fieldOfApplication){
       if(!is.null(locales)){
         fieldOfApplication <- self$createLocalisedProperty(fieldOfApplication, locales)
       }
       return(self$delListElement("fieldOfApplication", fieldOfApplication))
     },
     
     #'@description Set version number
     #'@param versionNumber version number
     setVersionNumber = function(versionNumber){
       if(!is(versionNumber,"character")) versionNumber <- as(versionNumber, "character")
       self$versionNumber <- versionNumber
     },
     
     #'@description Set version date
     #'@param versionDate version date
     setVersionDate = function(versionDate){
       self$versionDate <- versionDate
     }
   )                        
)