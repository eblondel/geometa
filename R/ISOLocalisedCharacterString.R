#' ISOLocalisedCharacterString
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO localised character string
#' @return Object of \code{\link{R6Class}} for modelling an ISO LocalisedCharacterString
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml, locale, value)}}{
#'    This method is used to instantiate an ISOLocalisedCharacterString
#'  }
#' }
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
    xmlNamespacePrefix = "GMD"
  ),
  public = list(
    initialize = function(xml = NULL, locale, value){
      super$initialize(xml = xml, value = value)
      if(is.null(xml)){
        self$attrs[["locale"]] <- paste0("#",locale)
      }
    }
  )                        
)