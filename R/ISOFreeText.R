#' ISOFreeText
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO freeText
#' @return Object of \code{\link{R6Class}} for modelling an ISO FreeText
#' @format \code{\link{R6Class}} object.
#'
#' @field textGroup
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml)}}{
#'    This method is used to instantiate an ISOFreeText
#'  }
#'  \item{\code{addTextGroup(textGroup)}}{
#'    Add a text group, object of class \code{ISOLocalisedCharacterString}
#'  }
#'  \item{\code{delTextGroup(textGroup)}}{
#'    Deletes a text group, object of class \code{ISOLocalisedCharacterString}
#'  }
#' }
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
     #+ textGroup [1..*]: ISOLocalisedCharacterString
     textGroup = list(),
     
     initialize = function(xml = NULL){
       super$initialize(xml = xml)
     },
     
     #addTextGroup
     addTextGroup = function(textGroup){
       if(!is(textGroup, "ISOLocalisedCharacterString")){
         stop("The argument should be an object of class 'ISOLocalisedCharacterString")
       }
       return(self$addListElement("textGroup", textGroup))
     },
     
     #delTextGroup
     delTextGroup = function(textGroup){
       if(!is(textGroup, "ISOLocalisedCharacterString")){
         stop("The argument should be an object of class 'ISOLocalisedCharacterString")
       }
       return(self$delListElement("textGroup", textGroup))
     }
     
   )                        
)