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
#'  \item{\code{new(value)}}{
#'    This method is used to instantiate an ISOBaseBoolean
#'  }
#' }
#' 
#' @author Emmanuel Blondel <emmanuel.blondel1@@gmail.com>
#'
ISOBaseBoolean <- R6Class("ISOBaseBoolean",
  inherit = ISOMetadataElement,
  public = list(
    value = NA,
    initialize = function(xml = NULL, value){
      super$initialize(
        element = "Boolean",
        namespace = ISOMetadataNamespace$GCO
      )
      if(!is.null(xml)){
        self$decode(xml)
      }else{
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