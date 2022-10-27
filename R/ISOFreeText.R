#' ISOFreeText
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO freeText
#' @return Object of \code{\link{R6Class}} for modelling an ISO FreeText
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   ft <- ISOFreeText$new()
#'   
#' @references 
#'   ISO 19115:2003 - Geographic information -- Metadata
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOFreeText <- R6Class("ISOFreeText",
   inherit = ISOAbstractObject,
   private = list(
     xmlElement = "PT_FreeText",
     xmlNamespacePrefix = "GMD"
   ),
   public = list(
     #'@field textGroup textGroup [1..*]: ISOLocalisedCharacterString
     textGroup = list(),
     
     #'@description Initializes object
     #'@param xml object of class \link{XMLInternalNode-class}
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