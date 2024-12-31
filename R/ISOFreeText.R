#' ISOFreeText
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO freeText
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO FreeText
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   ft <- ISOFreeText$new()
#'   
#' @references 
#'  - ISO 19139 \url{https://schemas.isotc211.org/19139/-/gmd/1.0/gmd/#element_PT_FreeText}
#'  
#'  - ISO 19115-3 \url{https://schemas.isotc211.org/19115/-3/lan/1.0/lan/#element_PT_FreeText}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFreeText <- R6Class("ISOFreeText",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "PT_FreeText",
     xmlNamespacePrefix = list(
        "19139" = "GMD",
        "19115-3" = "LAN"
     )
   ),
   public = list(
     #'@field textGroup textGroup [1..*]: ISOLocalisedCharacterString
     textGroup = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link[XML]{XMLInternalNode-class}
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #'@description Adds text group
     #'@param textGroup text group, object of class \link{ISOLocalisedCharacterString}
     #'@return \code{TRUE} if added, \code{FALSE} otherwise
     addTextGroup = function(textGroup){
       if(!is(textGroup, "ISOLocalisedCharacterString")){
         stop("The argument should be an object of class 'ISOLocalisedCharacterString")
       }
       return(self$addListElement("textGroup", textGroup))
     },
     
     #'@description Deletes text group
     #'@param textGroup text group, object of class \link{ISOLocalisedCharacterString}
     #'@return \code{TRUE} if deleted, \code{FALSE} otherwise
     delTextGroup = function(textGroup){
       if(!is(textGroup, "ISOLocalisedCharacterString")){
         stop("The argument should be an object of class 'ISOLocalisedCharacterString")
       }
       return(self$delListElement("textGroup", textGroup))
     }
     
   )                        
)
