#' ISOAbstractCatalogue
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO abstract catalogue
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISOAbstracCatalogue
#' @format \code{\link[R6]{R6Class}} object.
#'  
#' @references 
#'   - ISO 19139 \url{https://schemas.isotc211.org/19139/-/cat/1.2.0/cat/#element_AbstractCT_Catalogue}
#'   
#'   - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/cat/1.0/cat/#element_AbstractCT_Catalogue}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOAbstractCatalogue <- R6Class("ISOAbstractCatalogue",
   inherit = ISOAbstractObject,
   private = list(
     document = TRUE,
     xmlElement = "AbstractCT_Catalogue",
     xmlNamespacePrefix = list(
       "19139" = "GMX",
       "19115-3" = "CAT"
     )
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
     #'@field language language [0..1]: character
     language = NULL,
     #'@field characterSet character set [0..1]: character
     characterSet = NULL,
     #'@field locale locale [0..*]: ISOLocale
     locale = list(),
     #'@field subCatalogue subCatalogue [0..*]: ISOAbstractCatalogue (=> 19139)
     subCatalogue = list(),
     
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
     },
     
     #'@description Set language
     #'@param locale object of class \link{ISOLanguage} or any \link{character}
     #' from values returned by \code{ISOLanguages$values()}
     setLanguage = function(locale){
        if(is(locale, "character")){
           locale <- ISOLanguage$new(value = locale)
        }
        self$language <- locale
     },
     
     #'@description Set charset
     #'@param charset object of class \link{ISOCharacterSet} or any \link{character}
     #' from values returned by \code{ISOCharacterSet$values()}
     setCharacterSet = function(charset){
        if(is(charset, "character")){
           charset <- ISOCharacterSet$new(value = charset)
        }
        self$characterSet <- charset
     },
     
     #'@description Adds locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addLocale = function(locale){
        if(!is(locale,"ISOLocale")){
           stop("The argument should be a 'ISOLocale' object")  
        }
        return(self$addListElement("locale", locale))
     },
     
     #'@description Deletes locale
     #'@param locale object of class \link{ISOLocale}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delLocale = function(locale){
        if(!is(locale,"ISOLocale")){
           stop("The argument should be a 'ISOLocale' object")  
        }
        return(self$delListElement("locale", locale))
     },
     
     #'@description Add sub catalogue
     #'@param subCatalogue object of class \link{ISOAbstractCatalogue}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addSubCatalogue = function(subCatalogue){
        self$stopIfMetadataStandardIsNot("19139")
        if(!is(subCatalogue, "ISOAbstractCatalogue")){
           stop("The argument should be an object inheriting 'ISOAbstractCatalogue")
        }
        return(self$addListElement("subCatalogue", subCatalogue))
     },
     
     #'@description Deletes sub catalogue
     #'@param subCatalogue object of class \link{ISOAbstractCatalogue}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delSubCatalogue = function(subCatalogue){
        self$stopIfMetadataStandardIsNot("19139")
        if(!is(subCatalogue, "ISOAbstractCatalogue")){
           stop("The argument should be an object inheriting 'ISOAbstractCatalogue")
        }
        return(self$delListElement("subCatalogue", subCatalogue))
     }
     
   )                        
)
