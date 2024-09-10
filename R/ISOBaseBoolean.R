#' ISOBaseBoolean
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO boolean
#' @return Object of \code{\link{R6Class}} for modelling an ISO Boolean
#' @format \code{\link{R6Class}} object.
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @references
#'  ISO/TS 19103:2005 Geographic information -- Conceptual schema language
#'  
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseBoolean <- R6Class("ISOBaseBoolean",
  inherit = ISOAbstractObject,
  private = list(
    xmlElement = "Boolean",
    xmlNamespacePrefix = list(
      "19115-1/2" = "GCO",
      "19115-3" = "GCO"
    )
  ),
  public = list(
    #'@field value value
    value = NA,
    
    #'@description Initializes a base boolean object
    #'@param xml object of class \link{XMLInternalNode-class}
    #'@param value value
    initialize = function(xml = NULL, value){
      super$initialize(xml = xml)
      if(is.null(xml)){
        newValue <- value
        if(!is(value, "logical")){
          newValue <- as.logical(value)
          if(is.na(newValue)){
            stop(sprintf("Value '%s' cannot be coerced to 'logical'", value))
          }
        }
        self$value = tolower(as.character(newValue))
      }
    }
  )                        
)