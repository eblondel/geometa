#' ISOLocaleContainer
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO locale container
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO LocaleContainer
#' @format \code{\link[R6]{R6Class}} object.
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_PT_LocaleContainer}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/lan/1.0/lan/#element_PT_LocaleContainer}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLocaleContainer <- R6Class("ISOLocaleContainer",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "PT_LocaleContainer",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "LAN"
    )
  ),
  public = list(
    
    #'@field language language [1..1]: ISOLanguage
    language = NULL,
    #'@field country country [0..1]: ISOCountry
    country = NULL,
    #'@field characterEncoding character encoding [0..1]: ISOCharacterSet
    characterEncoding = NULL,
    #'@field description description [1..1]
    description = NULL,
    #'@field locale locale [1..1]
    locale = NULL,
    #'@field date date [1..*]
    date = list(),
    #'@field responsibleParty responsibleParty [1..*]
    responsibleParty = list(),
    #'@field localisedString localisedString [1..*]
    localisedString = list(),
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    initialize = function(xml = NULL){
      super$initialize(xml = xml)
    },
    
    #'@description Set language
    #'@param language object of class \link{ISOLanguage} or \link{character}
    setLanguage = function(language){
      if(!is(language, "ISOLanguage")){
        if(is(language,"character")){
          language = ISOLanguage$new(value = language)
        }else{
          stop("The argument 'language' should be an object of class 'ISOLanguage' or 'character'")
        }
      }
      self$language = language
    },
    
    #'@description Set country
    #'@param country object of class \link{ISOCountry} or \link{character}
    setCountry = function(country){
      if(!is(country, "ISOCountry")){
        if(is(country,"character")){
          country = ISOCountry$new(value = country)
        }else{
          stop("The argument 'country' should be an object of class 'ISOCountry' or 'character'")
        }
      }
      self$country = country
    },
    
    #'@description Set character encoding
    #'@param characterEncoding object of class \link{ISOCharacterSet} or \link{character}
    setCharacterEncoding = function(characterEncoding){
      if(!is(characterEncoding, "ISOCharacterSet")){
        if(is(characterEncoding,"character")){
          characterEncoding = ISOCharacterSet$new(value = characterEncoding)
        }else{
          stop("The argument 'characterEncoding' should be an object of class 'ISOCharacterSet' or 'character'")
        }
      }
      self$characterEncoding = characterEncoding
    },
    
    #'@description Set description
    #'@param description description
    #'@param locales list of localized texts. Default is \code{NULL}
    setDescription = function(description, locales = NULL){
      self$description <- as.character(description)
      if(!is.null(locales)){
        self$description <- self$createLocalisedProperty(description, locales)
      }
    },
    
    #'@description Set locale
    #'@param locale object of class \link{ISOLocale}
    setLocale = function(locale){
      if(!is(locale, "ISOLocale")){
        stop("The argument 'locale' should be an object of class 'ISOLocale'")
      }
      self$locale = locale
    },
    
    #'@description Adds date
    #'@param date object of class \link{ISODate}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument 'date' should be an object of class 'ISODate'")
      }
      return(self$addListElement("date", date))
    },
    
    #'@description Deletes date
    #'@param date object of class \link{ISODate}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delDate = function(date){
      if(!is(date, "ISODate")){
        stop("The argument 'date' should be an object of class 'ISODate'")
      }
      return(self$delListElement("date", date))
    },
    
    #'@description Adds responsible party
    #'@param responsibleParty object of class \link{ISOResponsibleParty} (in ISO 19139) or 
    #'\link{ISOResponsibility} (in ISO 19115-3)
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addResponsibleParty = function(responsibleParty){
      switch(getMetadataStandard(),
       "19139" = {
         if(!is(responsibleParty, "ISOResponsibleParty")){
           stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibleParty'")
         }
       },
       "19115-3" = {
         if(!is(responsibleParty, "ISOResponsibility")){
           stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibility'")
         }
       }
      )
      return(self$addListElement("responsibleParty", responsibleParty))
    },
    
    #'@description Deletes responsible party
    #'@param responsibleParty object of class \link{ISOResponsibleParty} (in ISO 19139) or 
    #'\link{ISOResponsibility} (in ISO 19115-3)
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delResponsibleParty = function(responsibleParty){
      switch(getMetadataStandard(),
       "19139" = {
         if(!is(responsibleParty, "ISOResponsibleParty")){
           stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibleParty'")
         }
       },
       "19115-3" = {
         if(!is(responsibleParty, "ISOResponsibility")){
           stop("The argument 'responsibleParty' should be an object of class 'ISOResponsibility'")
         }
       }
      )
      return(self$delListElement("responsibleParty", responsibleParty))
    },
    
    #'@description Adds localised string
    #'@param string object of class \link{character}
    #'@return \code{TRUE} if added, \code{FALSE} otherwise
    addLocalisedString = function(string){
      str <- ISOLocalisedCharacterString$new(value = string)
      return(self$addListElement("localisedString", str))
    },
    
    #'@description Deletes localised string
    #'@param string object of class \link{character}
    #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
    delLocalisedString = function(string){
      str <- ISOLocalisedCharacterString$new(value = string)
      return(self$delListElement("localisedString", str))
    }
  )                        
)
