#' ISOImagerySequence
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO imagery sequence
#' @return Object of \code{\link{R6Class}} for modelling an ISO imagery sequence
#' @format \code{\link{R6Class}} object.
#' 
#' @examples
#'   #possible values
#'   values <- ISOImagerySequence$values(labels = TRUE)
#'   
#'   #some def
#'   inst <- ISOImagerySequence$new(value = "instantaneous")
#' 
#' @references 
#'   ISO 19115-2:2009 - Geographic information -- Metadata Part 2: Extensions for imagery and gridded data
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOImagerySequence <- R6Class("ISOImagerySequence",
  inherit = ISOCodeListValue,
  private = list(
    xmlElement = "MI_SequenceCode",
    xmlNamespacePrefix = "GMI"
  ),
  public = list(
    
    #'@description Initializes object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    #'@param description description
    initialize = function(xml = NULL, value, description = NULL){
      super$initialize(xml = xml, id = private$xmlElement, value = value, description = description, 
                       addCodeSpaceAttr = FALSE)
    }
  )                        
)

ISOImagerySequence$values <- function(labels = FALSE){
  return(ISOCodeListValue$values(ISOImagerySequence, labels))
}