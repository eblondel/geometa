#' ISOLocalisedCharacterString
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO localised character string
#' @return Object of \code{\link{R6Class}} for modelling an ISO LocalisedCharacterString
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   str <- ISOLocalisedCharacterString$new(locale = "FR", value = "ma description")
#'   str$encode()
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOLocalisedCharacterString <- R6Class("ISOLocalisedCharacterString",
  inherit = ISOBaseCharacterString,
  private = list(
    xmlElement = "LocalisedCharacterString",
    xmlNamespacePrefix = list(
      "19139" = "GMD",
      "19115-3" = "LAN"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param locale locale
    #'@param value value
    initialize = function(xml = NULL, locale = NULL, value){
      super$initialize(xml = xml, value = value)
      if(is.null(xml)){
        if(!is.null(locale)) self$attrs[["locale"]] <- paste0("#",locale)
      }
    }
  )                        
)