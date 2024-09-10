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
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOTelephone <- R6Class("ISOTelephone",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "CI_Telephone",
     xmlNamespacePrefix = list(
       "19115-1/2" = "GMD",
       "19115-3" = "CIT"
     )
   ),
   public = list(
     #'@field voice voice
     voice = NULL,
     #'@field facsimile facsimile
     facsimile = NULL,
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}  
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Set voice
     #'@param voice voice
     #'@param locales list of localized voices. Default is \code{NULL}
     setVoice = function(voice, locales = NULL){
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
       if(!is(facsimile,"character")) facsimile <- as.character(facsimile)
       self$facsimile = facsimile
       if(!is.null(locales)){
         self$facsimile <- self$createLocalisedProperty(facsimile, locales)
       }
     }
   )                        
)