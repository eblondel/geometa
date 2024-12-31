#' ISOImagerySequence
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery sequence
#' @return Object of \code{\link[R6]{R6Class}} for modelling an ISO imagery sequence
#' @format \code{\link[R6]{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImagerySequence$values(labels = TRUE)
#'   
#'   #some def
#'   inst <- ISOImagerySequence$new(value = "instantaneous")
#' 
#' @references 
#'   - 19139 \url{https://schemas.isotc211.org/19115/-2/gmi/1.0/gmi/#element_MI_SequenceCode}
#'   
#'   - 19115-3 \url{https://schemas.isotc211.org/19115/-3/mac/1.0/mac/#element_MI_SequenceCode}
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImagerySequence <- R6Class("ISOImagerySequence",
  inherit = ISOCodeListItem,
  private = list(
    xmlElement = "MI_SequenceCode",
    xmlNamespacePrefix = list(
      "19139" = "GMI",
      "19115-3" = "MAC"
    )
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link[XML]{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                       addCodeSpaceAttr = FALSE)
    }
  )                        
)

ISOImagerySequence$values <- function(labels = FALSE){
  return(ISOCodeListItem$values(ISOImagerySequence, labels))
}
