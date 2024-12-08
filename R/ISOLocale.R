#' ISOLocale
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO locale
#' @return Object of \code{\link{R6Class}} for modelling an ISO Locale
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   loc <- ISOLocale$new()
#'   loc$setId("eng")
#'   loc$setLanguage("eng")
#'   loc$setCountry("UK")
#'   loc$setCharacterSet("utf8")
#'   
#' @references 
#'  - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_PT_Locale}
#'  
#'  - ISO 19115-3 \link{https://schemas.isotc211.org/19115/-3/lan/1.0/lan/#element_PT_Locale}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLocale <- R6Class("ISOLocale",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "PT_Locale",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "LAN"
     )
   ),
   public = list(
     #'@field languageCode languageCode [1..1]: ISOLanguage (ISO 19139)
     languageCode = NULL,
     #'@field language language [1..1]: ISOLanguage (ISO 19115-3)
     language = NULL,
     #'@field country country [0..1]: ISOCountry
     country = NULL,
     #'@field characterEncoding characterEncoding [1..1]: ISOCharacterSet
     characterEncoding = NULL,

     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
     #'@param id id
     #'@param language language
     #'@param country country
     #'@param characterEncoding characterEncoding
     initialize = function(xml = NULL, id = NULL, language = NULL,
                           country = NULL, characterEncoding = NULL){
       super$initialize(xml = xml)
       if(!is.null(id)) self$setId(id)
       if(!is.null(language)) self$setLanguage(language)
       if(!is.null(country)) self$setCountry(country)
       if(!is.null(characterEncoding)) self$setCharacterSet(characterEncoding)
     },
     
     #'@description Set ID
     #'@param id id
     setId = function(id){
       self$attrs$id = id
     },
     
     #'@description Set language
     #'@param language object of class \link{ISOLanguage} or any \link{character} among
     #' values returned by \code{ISOLanguage$values()}
     setLanguage = function(language){
       if(is(language, "character")){
         language <- ISOLanguage$new(value = language)
       }
       switch(getMetadataStandard(),
        "19139" = {
          self$languageCode <- language
        },
        "19115-3" = {
          self$language = language
        }
       )
     },
     
     #'@description Set country
     #'@param country object of class \link{ISOCountry} or any \link{character} among
     #' values returned by \code{ISOCountry$values()} or any other ISO-2 country code
     setCountry = function(country){
       if(is(country, "character")){
         country <- ISOCountry$new(value = country)
       }
       self$country <- country
     },   
     
     
     #'@description Set character set
     #'@param charset object of class \link{ISOCharacterSet} or any \link{character} among
     #' values returned by \code{ISOCharacterSet$values()}
     setCharacterSet = function(charset){
       if(is(charset, "character")){
         charset <- ISOCharacterSet$new(value = charset)
       }
       self$characterEncoding <- charset
     }
     
   )                        
)