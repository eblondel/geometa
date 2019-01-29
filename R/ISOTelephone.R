#' ISOTelephone
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO file identifier
#' @return Object of \code{\link{R6Class}} for modelling an ISO Telephone
#' @format \code{\link{R6Class}} object.
#'
#' @field voice
#' @field facsimile
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOTelephone
#'  }
#'  \item{\code{setVoice(voice, locales)}}{
#'    Set voice phone number. Locale numbers can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#'  \item{\code{setFacsimile(voice, locales)}}{
#'    Set facsimile phone number. Locale numbers can be specified as \code{list}
#'    with the \code{locales} argument.
#'  }
#' }
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
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     voice = NULL,
     facsimile = NULL,
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #setVoice
     setVoice = function(voice, locales = NULL){
       if(!is(voice,"character")) voice <- as.character(voice)
       self$voice = voice
       if(!is.null(locales)){
         self$voice <- self$createLocalisedProperty(voice, locales)
       }
     },
     
     #setFacsimile
     setFacsimile = function(facsimile, locales = NULL){
       if(!is(facsimile,"character")) facsimile <- as.character(facsimile)
       self$facsimile = facsimile
       if(!is.null(locales)){
         self$facsimile <- self$createLocalisedProperty(facsimile, locales)
       }
     }
   )                        
)