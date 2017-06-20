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
     setVoice = function(voice){
       if(!is(voice,"character")) voice <- as.character(voice)
       self$voice = voice
     },
     
     #setFacsimile
     setFacsimile = function(facsimile){
       if(!is(facsimile,"character")) facsimile <- as.character(facsimile)
       self$facsimile = facsimile
     }
   )                        
)