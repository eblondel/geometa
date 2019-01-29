#' ISOLocale
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO locale
#' @return Object of \code{\link{R6Class}} for modelling an ISO Locale
#' @format \code{\link{R6Class}} object.
#'
#' @field languageCode
#' @field country
#' @field characterEncoding
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, code, country, encoding)}}{
#'    This method is used to instantiate an ISOLocale
#'  }
#'  \item{\code{setId(id)}}{
#'    Set the locale Id, to bind to \code{ISOFreeText} elements to identify a locale text.
#'  }
#'  \item{\code{setLanguage(language)}}{
#'    Set the language, object of class "character" (language code), or object of
#'    class \code{ISOLanguage}.
#'  }
#'  \item{\code{setCountry(country)}}{
#'    Set the country, object of class "character" (country code), or object of class
#'    \code{ISOCountry}.
#'  }
#'  \item{\code{setCharacterSet(charset)}}{
#'    Set the character encoding, object of class "character" (encoding code), or object
#'    of class \code{ISOCharacterSet}.
#'  }
#' }
#' 
#' @examples
#'   loc <- ISOLocale$new()
#'   loc$setId("eng")
#'   loc$setLanguage("eng")
#'   loc$setCountry("UK")
#'   loc$setCharacterSet("utf8")
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLocale <- R6Class("ISOLocale",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "PT_Locale",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #+ description [1..1]: ISOLanguage
     languageCode = NULL,
     #+ country [0..1]: ISOCountry
     country = NULL,
     #+ characterEncoding [1..1]: ISOCharacterSet
     characterEncoding = NULL,

     initialize = function(xml = NULL, id = NULL, language = NULL,
                           country = NULL, encoding = NULL){
       super$initialize(xml = xml)
       if(!is.null(id)) self$setId(id)
       if(!is.null(language)) self$setLanguage(language)
       if(!is.null(country)) self$setCountry(country)
       if(!is.null(encoding)) self$setCharacterSet(encoding)
     },
     
     #setId
     setId = function(id){
       self$attrs$id = id
     },
     
     #setLanguage
     setLanguage = function(language){
       if(is(language, "character")){
         language <- ISOLanguage$new(value = language)
       }
       self$languageCode <- language
     },
     
     #setCountry
     setCountry = function(country){
       if(is(country, "character")){
         country <- ISOCountry$new(value = country)
       }
       self$country <- country
     },   
     
     
     #setCharacterSet
     setCharacterSet = function(charset){
       if(is(charset, "character")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       self$characterEncoding <- charset
     }
     
   )                        
)