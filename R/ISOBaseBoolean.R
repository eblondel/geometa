#' ISOBaseBoolean
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
#' @keywords ISO boolean
#' @return Object of \code{\link{R6Class}} for modelling an ISO Boolean
#' @format \code{\link{R6Class}} object.
#'
#' @field value
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(xml,value)}}{
#'    This method is used to instantiate an ISOBaseBoolean
#'  }
#' }
#' 
#' @note Class used by geometa internal XML decoder/encoder
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseBoolean <- R6Class("ISOBaseBoolean",
  inherit = ISOMetadataElement,
  private = list(
    xmlElement = "Boolean",
    xmlNamespacePrefix = "GCO"
  ),
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(
        xml = xml,
        element = private$xmlElement,
        namespace = getISOMetadataNamespace(private$xmlNamespacePrefix)
      )
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