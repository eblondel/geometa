#' ISOTelephone
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO file identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO Telephone
#' @format \code{\link{R6Class}} object.
#' 
#' @examples 
#'   md <- ISOTelephone$new()
#'   md$setVoice("myphonenumber")
#'   md$setFacsimile("myfacsimile")
#'   xml <- md$encode()
#'   
#' @references 
#'  - ISO 19139 \link{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_CI_Telephone}
#'  
#'  - ISO 19115-3 \link{https://schemas.isotc211.org/19115/-3/cit/2.0/cit/#element_CI_Telephone}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTelephone <- R6Class("ISOTelephone",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CI_Telephone",
     xmlNamespacePrefix = list(
       "19139" = "GMD",
       "19115-3" = "CIT"
     )
   ),
   public = list(
     #ISO 19115-1:2003 / 19139
     #'@field voice voice
     voice = NULL,
     #'@field facsimile facsimile
     facsimile = NULL,
     
     #ISO 19115-1:2014 / 19115-3:2016
     #'@field number number
     number = NULL,
     #'@field numberType  numberType
     numberType = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}  
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set voice
     #'@param voice voice
     #'@param locales list of localized voices. Default is \code{NULL}
     setVoice = function(voice, locales = NULL){
       self$stopIfMetadataStandardIsNot("19139")
       if(!is(voice,"character")) voice <- as.character(voice)
       self$voice = voice
       if(!is.null(locales)){
         self$voice <- self$createLocalisedProperty(voice, locales)
       }
     },
     
     #'@description Set facsimile
     #'@param facsimile facsimile
     #'@param locales list of localized facsimiles. Default is \code{NULL}
     setFacsimile = function(facsimile, locales = NULL){
       self$stopIfMetadataStandardIsNot("19139")
       if(!is(facsimile,"character")) facsimile <- as.character(facsimile)
       self$facsimile = facsimile
       if(!is.null(locales)){
         self$facsimile <- self$createLocalisedProperty(facsimile, locales)
       }
     },
     
     #'@description Set number
     #'@param number number
     #'@param locales list of localized numbers Default is \code{NULL}
     setNumber = function(number, locales = NULL){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(!is(number,"character")) number <- as.character(number)
       self$number = number
       if(!is.null(locales)){
         self$number <- self$createLocalisedProperty(number, locales)
       }
     },
     
     #'@description Set numberType
     #'@param numberType numberType object of class \link{ISOTelephoneType} or any \link{character}
     #' among values returned by \code{ISOTelephoneType$values()}
     setNumberType = function(numberType){
       self$stopIfMetadataStandardIsNot("19115-3")
       if(is(numberType, "character")){
         numberType <- ISOTelephoneType$new(value = numberType)
       }
       self$numberType <- numberType
     }
   )                        
)